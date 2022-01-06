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
import           Data.Void
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
     Source ::IO a -> Flow a b -> Flow Void b
     Pipe ::(a -> IO b) -> Flow b c -> Flow a c
     Sink ::(a -> IO ()) -> Flow a ()

runFlow
    :: ( Has
             (State (IntMap (WorkName, ThreadId, Counter, Counter)) :+: Fresh)
             sig
             m
       , MonadIO m
       )
    => (TChan a, Counter)
    -> Flow a b
    -> m ()
runFlow (ti, ci) = \case
    Pipe f fl -> do
        (to, co) <- startThread "nn" f (ti, ci) -- shuold fork workManager
        runFlow (to, co) fl
    Source f fl -> do
        res <- liftIO $ do
            to <- newTChanIO
            co <- newIORef 0
            forkIO $ void $ forever $ do
                v' <- try @SomeException f
                case v' of
                    Left  se -> writeFile "source" (show se)
                    Right v  -> do
                        atomically $ writeTChan to v
                        atomicModifyIORef' co (\x -> (x + 1, ()))
            pure (to, co)
        runFlow res fl
    Sink f -> do
        liftIO $ forkIO $ void $ forever $ do
            oc <- atomically $ readTChan ti
            atomicModifyIORef' ci (\x -> (x - 1, ()))
            f oc
        pure ()

type Work = Flow Void ()

runWork
    :: ( Has
             (State (IntMap (WorkName, ThreadId, Counter, Counter)) :+: Fresh)
             sig
             m
       , MonadIO m
       )
    => Work
    -> m ()
runWork work = do
    res <- liftIO $ do
        ti <- newTChanIO
        ci <- newIORef 0
        pure (ti, ci)
    runFlow res work

workRun :: Work -> IO ()
workRun work = do
    void
        $ runState @(IntMap (WorkName, ThreadId, Counter, Counter)) IntMap.empty
        $ runFresh 0
        $ runWork work
    threadDelay 200000000

s1 :: IO Int
s1 = do
    putStr "input number:"
    read <$> getLine

p1 :: Int -> IO [Int]
p1 i = do
    print i
    pure (replicate i i)

s2 :: [Int] -> IO ()
s2 = print


work1 :: Flow Void ()
work1 = Source s1 (Pipe p1 (Sink s2))

e1 = workRun work1
