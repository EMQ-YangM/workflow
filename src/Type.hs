{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Type where

import           Control.Algebra
import           Control.Carrier.Reader
import           Control.Carrier.State.Strict
import           Control.Concurrent.STM
import           Control.Effect.Labelled
import           Control.Effect.Labelled        ( HasLabelled )
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.IORef
import           Data.Kind


data BaseAction :: Type -> Type -> (Type -> Type) -> Type -> Type where
    GetInput ::BaseAction input output m input
    PutOutput ::output -> BaseAction input output m ()

getInput :: HasLabelled BaseAction (BaseAction input output) sig m => m input
getInput = sendLabelled @BaseAction GetInput

putOutput
    :: HasLabelled BaseAction (BaseAction input output) sig m => output -> m ()
putOutput output = sendLabelled @BaseAction (PutOutput output)

type Counter = IORef Int

newtype BaseActionC input output m a = BaseActionC {unBaseActionC :: ReaderC (TChan input, Counter, TChan output, Counter) m a}
    deriving (Functor, Applicative, Monad)

instance (Algebra sig m, MonadIO m) => Algebra (BaseAction input output :+: sig) (BaseActionC input output m) where
    alg hdl sig ctx = BaseActionC $ ReaderC $ \(input, ic, output, oc) ->
        case sig of
            L GetInput -> do
                v <- liftIO $ do
                    rv <- atomically $ readTChan input
                    atomicModifyIORef' ic (\x -> (x - 1, ()))
                    pure rv
                pure (v <$ ctx)
            L (PutOutput o) -> do
                liftIO $ do
                    atomically $ writeTChan output o
                    atomicModifyIORef oc (\x -> (x + 1, ()))
                pure ctx
            R signa -> alg
                (runReader (input, ic, output, oc) . unBaseActionC . hdl)
                signa
                ctx

type WorkName = String

loop
    :: ( HasLabelled BaseAction (BaseAction input output) sig m
       , Has (Reader WorkName) sig m
       , MonadIO m
       )
    => (input -> IO output)
    -> m ()
loop fun = forever $ do
    input <- getInput
    val   <- liftIO $ try @SomeException $ fun input
    case val of
        Left se -> do
            name <- ask @WorkName
            liftIO $ appendFile name (show se)
        Right output -> putOutput output

