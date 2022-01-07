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
import           Data.Traversable               ( for )
import           Data.Void
import           Optics                         ( (^.)
                                                , makeLenses
                                                )
import           System.Random
import           System.Timeout                 ( timeout )

type Counter = IORef Int

data Command = NoCommand | StopWork

data WorkEnv input output = WorkEnv
    { _inputChan         :: TChan input
    , _inputChanCounter  :: Counter
    , _outputChan        :: TChan output
    , _outputChanCounter :: Counter
    , _commandRef        :: IORef Command
    }

makeLenses ''WorkEnv

type WorkName = String

data WorkError = WorkStop

workloop
    :: forall input output sig m
     . ( Has (Reader (WorkEnv input output) :+: Error WorkError) sig m
       , MonadIO m
       )
    => WorkName
    -> (input -> IO output)
    -> m ()
workloop workName fun = forever $ do
    crf  <- view @(WorkEnv input output) commandRef
    comm <- liftIO $ readIORef crf
    case comm of
        StopWork  -> throwError WorkStop
        NoCommand -> do
            tic   <- view @(WorkEnv input output) inputChan
            input <- liftIO $ atomically $ readTChan tic
            ic    <- view @(WorkEnv input output) inputChanCounter
            liftIO $ atomicModifyIORef' ic (\x -> (x - 1, ()))
            val <- liftIO $ try @SomeException $ fun input
            case val of
                Left se -> do  -- handle error
                    let name = workName
                    liftIO $ appendFile name (show se)
                Right output -> do
                    toc <- view @(WorkEnv input output) outputChan
                    liftIO $ atomically $ writeTChan toc output
                    oc <- view @(WorkEnv input output) outputChanCounter
                    liftIO $ atomicModifyIORef' oc (\x -> (x + 1, ()))

data WorkRecord = WorkRecord
    { workId      :: Int
    , workName    :: WorkName
    , workCommand :: IORef Command
    , workThid    :: ThreadId
    }

newtype WorkManState = WorkManState
    { allWorks :: IntMap WorkRecord
    }


data Action = ForkAWorker | KillAWorker | NoOperate
  deriving (Show, Eq)

dynamciForkWork :: Int -> Int -> Int -> Action
dynamciForkWork inc out wroks | wroks == 0     = ForkAWorker
                              | inc - out > 20 = ForkAWorker
                              | out - inc > 20 = KillAWorker
                              | otherwise      = NoOperate

manage
    :: (Has (State WorkManState :+: Fresh) sig m, MonadIO m)
    => (input -> IO output)
    -> TChan input
    -> Counter
    -> TChan output
    -> Counter
    -> Counter
    -> m ()
manage f inputChan inputChanCounter outputChan outputChanCounter threadCounter
    = forever $ do
        inc   <- liftIO $ readIORef inputChanCounter
        out   <- liftIO $ readIORef outputChanCounter
        works <- gets (IntMap.size . allWorks)
        liftIO $ writeIORef threadCounter works
        case dynamciForkWork inc out works of
            NoOperate   -> pure ()
            KillAWorker -> do
                aRandomIndex <- liftIO $ randomRIO (0, works)
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
                    $ runReader
                          (WorkEnv inputChan
                                   inputChanCounter
                                   outputChan
                                   outputChanCounter
                                   commandRef
                          )
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
    { _allCounter    :: [Counter]
    , _manThid       :: [ThreadId]
    , _threadCounter :: [Counter]
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
        td   <- liftIO $ newIORef 0
        thid <-
            liftIO
            $ forkIO
            $ void
            $ runState @WorkManState (WorkManState IntMap.empty)
            $ runFresh 0
            $ manage f ti ci to co td
        allCounter %= (ci :)
        manThid %= (thid :)
        threadCounter %= (td :)
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
    (mms, ()) <- runState (ManManState [] [] []) $ runWork work
    forever $ do
        res <- for (zip [1 ..] $ mms ^. threadCounter) $ \(idx, counter) -> do
            v <- readIORef counter
            pure (idx, v)
        print res
        threadDelay 100000

fib :: [Integer]
fib = 1 : 1 : zipWith (+) fib (tail fib)

getLength :: Integer -> Int
getLength i = length (show i)

s1 :: IO Int
s1 = do
    threadDelay 100000
    randomRIO (10, 30)

p1 :: Int -> IO [Integer]
p1 i = do
    threadDelay 2000000
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

data ThreadCall message result = Call message (TMVar result) | Cast message

type ThreadChan message result = TChan (ThreadCall message result)

type Pid = Int

call
    :: Pid
    -> message
    -> Int
    -> IntMap (ThreadChan message result)
    -> IO (Maybe result)
call pid message tt tm = do
    tmv <- newEmptyTMVarIO
    let tc = Call message tmv
    case IntMap.lookup pid tm of
        Nothing  -> error "????"
        Just tc' -> do
            atomically $ writeTChan tc' tc
            timeout tt $ atomically $ readTMVar tmv


handle :: ThreadCall message reuslt -> IO ()
handle (Call messge tmv) = undefined
handle (Cast message   ) = undefined
