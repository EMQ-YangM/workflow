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
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Type where

import           Control.Algebra
import           Control.Carrier.Fresh.Strict
import           Control.Carrier.Reader
import           Control.Carrier.State.Strict
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Effect.Labelled
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.IORef
import qualified Data.IntMap                   as IntMap
import           Data.IntMap                    ( IntMap )
import           Data.Kind
-- import           Data.Map                       ( Map )
-- import qualified Data.Map                      as Map


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
    deriving (Functor, Applicative, Monad, MonadIO)

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


runBaseActionC
    :: TChan input
    -> Counter
    -> TChan output
    -> Counter
    -> Labelled BaseAction (BaseActionC input output) m a
    -> m a
runBaseActionC inp ci out co f =
    runReader (inp, ci, out, co) $ unBaseActionC $ runLabelled f

type WorkName = String

data Command = Command

loop
    :: (HasLabelled BaseAction (BaseAction input output) sig m, MonadIO m)
    => WorkName
    -> (input -> IO output)
    -> m ()
loop workName fun = forever $ do
    input <- getInput
    val   <- liftIO $ try @SomeException $ fun input
    case val of
        Left se -> do
            let name = workName
            liftIO $ appendFile name (show se)
        Right output -> putOutput output

startThread
    :: ( Has
             (State (IntMap (WorkName, ThreadId, Counter, Counter)) :+: Fresh)
             sig
             m
       , MonadIO m
       )
    => WorkName
    -> (input -> IO output)
    -> (TChan input, Counter)
    -> m (TChan output, Counter)
startThread name fun (inp, ci) = do
    out  <- liftIO newTChanIO
    co   <- liftIO $ newIORef 0
    thid <- liftIO $ forkIO $ void $ runBaseActionC inp ci out co $ loop
        name
        fun
    number <- fresh
    modify (IntMap.insert number (name, thid, ci, co))
    pure (out, co)


data Flow a b where
     Source ::IO a -> Flow a b -> Flow () b
     Pipe ::(a -> IO b) -> Flow b c -> Flow a c
     Sink ::Flow a b -> (b -> IO ()) -> Flow a ()

type Work = Flow () ()


-- runWork
--     :: ( Has (State (IntMap (WorkName, ThreadId, Counter)) :+: Fresh) sig m
--        , MonadIO m
--        )
--     => (forall a b . Flow a b -> TChan a -> m (TChan b))
-- runWork = \case
--     Source io fl -> \_ -> liftIO $ do
--         tc1 <- newTChanIO
--         forkIO $ void $ forever $ do
--             a <- io
--             atomically $ writeTChan tc1 a
--         tc2 <- newTChanIO
--         runWork fl tc1 
--         pure tc2
--     Pipe f  fl -> undefined
--     Sink fl f  -> undefined


-- runFun ::  -> IO ()
-- runFun name fun (inp, ci) = do
--     out <- newTChanIO
--     co  <- newIORef 0
--     -- runLoop inp ci out co name fun
--     undefined

-- t1 = runFun "t1" fun1
-- t2 = runFun "t2" fun2

-- fun1 :: Int -> IO [String]
-- fun1 i = replicateM i getLine

-- fun2 :: [String] -> IO String
-- fun2 i = pure (concat i)

-- data FlowType a b  = SourceType a | WorkType a b | SinkType b
