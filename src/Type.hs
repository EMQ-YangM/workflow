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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns, TemplateHaskell #-}

module Type where

import           Control.Algebra
import           Control.Carrier.Error.Either
import           Control.Carrier.Fresh.Strict
import           Control.Carrier.Reader
import           Control.Carrier.State.Strict
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Effect.Labelled
import           Control.Effect.Optics
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Foldable
import           Data.IORef
import qualified Data.IntMap                   as IntMap
import           Data.IntMap                    ( IntMap )
import           Data.Kind
import           Data.Void
import           Optics
import           System.Random
-- import           Data.Map                       ( Map )
-- import qualified Data.Map                      as Map


data WorkAction :: Type -> Type -> (Type -> Type) -> Type -> Type where
    GetInput ::WorkAction input output m input
    PutOutput ::output -> WorkAction input output m ()
    GetCommand ::WorkAction input output m Command

getInput :: HasLabelled WorkAction (WorkAction input output) sig m => m input
getInput = sendLabelled @WorkAction GetInput

putOutput
    :: HasLabelled WorkAction (WorkAction input output) sig m => output -> m ()
putOutput output = sendLabelled @WorkAction (PutOutput output)

getCommand
    :: HasLabelled WorkAction (WorkAction input output) sig m => m Command
getCommand = sendLabelled @WorkAction GetCommand

type Counter = IORef Int

data Command = NoCommand | StopWork

data WorkState input output = WorkState
    { inputChan         :: TChan input
    , inputChanCounter  :: Counter
    , outputChan        :: TChan output
    , outputChanCounter :: Counter
    , commandRef        :: IORef Command
    }

newtype WorkActionC input output m a = WorkActionC {unWorkActionC :: ReaderC (WorkState input output) m a}
    deriving (Functor, Applicative, Monad, MonadIO)

instance (Algebra sig m, MonadIO m) => Algebra (WorkAction input output :+: sig) (WorkActionC input output m) where
    alg hdl sig ctx = WorkActionC $ ReaderC $ \ws@WorkState {..} -> case sig of
        L GetInput -> do
            v <- liftIO $ do
                rv <- atomically $ readTChan inputChan
                atomicModifyIORef' inputChanCounter (\x -> (x - 1, ()))
                pure rv
            pure (v <$ ctx)
        L (PutOutput o) -> do
            liftIO $ do
                atomically $ writeTChan outputChan o
                atomicModifyIORef outputChanCounter (\x -> (x + 1, ()))
            pure ctx
        L GetCommand -> do
            v <- liftIO $ readIORef commandRef
            pure (v <$ ctx)
        R signa -> alg (runReader ws . unWorkActionC . hdl) signa ctx


runWorkActionC
    :: WorkState input output
    -> Labelled WorkAction (WorkActionC input output) m a
    -> m a
runWorkActionC ws f = runReader ws $ unWorkActionC $ runLabelled f

type WorkName = String

data WorkError = WorkStop

workloop
    :: ( HasLabelled WorkAction (WorkAction input output) sig m
       , Has (Error WorkError) sig m
       , MonadIO m
       )
    => WorkName
    -> (input -> IO output)
    -> m ()
workloop workName fun = forever $ do
    comm <- getCommand
    case comm of
        StopWork  -> throwError WorkStop
        NoCommand -> do
            input <- getInput
            val   <- liftIO $ try @SomeException $ fun input
            case val of
                Left se -> do  -- handle error
                    let name = workName
                    liftIO $ appendFile name (show se)
                Right output -> putOutput output

data WorkRecord = WorkRecord
    { workId      :: Int
    , workName    :: WorkName
    , workCommand :: IORef Command
    , workThid    :: ThreadId
    }

newtype WorkManState = WorkManState
    { allWorks :: IntMap WorkRecord
    }

data WorkManEnv = WorkManEnv
    { inpoutCounter :: Counter
    , outputCounter :: Counter
    }

data Action = ForkAWorker | KillAWorker | NoOperate
  deriving (Show, Eq)

dynamciForkWork :: Int -> Int -> Int -> Action
dynamciForkWork inc out wroks | wroks == 0 = ForkAWorker
                              | otherwise  = NoOperate

manage
    :: ( Has (State WorkManState :+: Reader WorkManEnv :+: Fresh) sig m
       , MonadIO m
       )
    => (input -> IO output)
    -> TChan input
    -> Counter
    -> TChan output
    -> Counter
    -> m ()
manage f inputChan inputChanCounter outputChan outputChanCounter = forever $ do
    inc   <- liftIO $ readIORef inputChanCounter
    out   <- liftIO $ readIORef outputChanCounter
    works <- gets (IntMap.size . allWorks)
    case dynamciForkWork inc out works of
        NoOperate   -> pure ()
        KillAWorker -> do
            aRandomIndex                       <- liftIO $ randomRIO (0, works)
            WorkRecord { workId, workCommand } <- gets
                ((IntMap.! aRandomIndex) . allWorks)
            liftIO $ writeIORef workCommand StopWork
            modify @WorkManState
                (WorkManState . IntMap.delete workId . allWorks)
        ForkAWorker -> do
            number     <- fresh
            commandRef <- liftIO $ newIORef NoCommand
            thid       <-
                liftIO
                $ forkIO
                $ void
                $ runWorkActionC (WorkState { .. })
                $ runError @WorkError
                $ workloop (show number) f
            modify
                ( WorkManState
                . IntMap.insert
                      number
                      (WorkRecord number "nameless" commandRef thid)
                . allWorks
                )
    liftIO $ threadDelay 1000000


data Flow a b where
     Source ::IO a -> Flow a b -> Flow Void b
     Pipe ::(a -> IO b) -> Flow b c -> Flow a c
     Sink ::(a -> IO ()) -> Flow a ()

data ManManState = ManManState
    { _allCounter :: [Counter]
    , _manThid    :: [ThreadId]
    }
makeLenses ''ManManState

runFlow
    :: (Has (State ManManState) sig m, MonadIO m)
    => (TChan a, Counter)
    -> Flow a b
    -> m ()
runFlow (ti, ci) = \case
    Pipe f fl -> do
        to   <- liftIO newTChanIO
        co   <- liftIO $ newIORef 0
        thid <-
            liftIO
            $ forkIO
            $ void
            $ runState @WorkManState (WorkManState IntMap.empty)
            $ runReader (WorkManEnv ci co)
            $ runFresh 0
            $ manage f ti ci to co
        allCounter %= (co :)
        manThid %= (thid :)
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

runWork :: (Has (State ManManState) sig m, MonadIO m) => Work -> m ()
runWork work = do
    res <- liftIO $ do
        ti <- newTChanIO
        ci <- newIORef 0
        pure (ti, ci)
    runFlow res work

workRun :: Work -> IO ()
workRun work = do
    (mms, ()) <- runState (ManManState [] []) $ runWork work
    forever $ do
        for_ (mms ^. allCounter) $ \counter -> do
            v <- readIORef counter
            print v
        threadDelay 100000

fib :: [Integer]
fib = 1 : 1 : zipWith (+) fib (tail fib)

getLength :: Integer -> Int
getLength i = length (show i)

s1 :: IO Int
s1 = do
    threadDelay 1000000
    randomRIO (10, 30)

p1 :: Int -> IO [Integer]
p1 i = do
    print i
    pure (take i fib)

p2 :: [Integer] -> IO [Int]
p2 ls = do
    threadDelay 3000000
    print ls
    pure (fmap getLength ls)

s2 :: [Int] -> IO ()
s2 _ = pure ()


work1 :: Flow Void ()
work1 = Source s1 (Pipe p1 (Pipe p2 (Sink s2)))

e1 = workRun work1
