{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
module Example.Example1.E2 where

import           Control.Algebra
import           Control.Carrier.Error.Either
import           Control.Carrier.Reader
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Effect.Labelled
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Foldable                  ( for_ )
import           Data.Proxy
import           Example.Example1.Type
import           Process.HasServer
import           Process.HasWorkGroup
import           Process.Metric
import           Process.Type
import           Process.Util

work
    :: ( Has
             (    Reader WorkEnv
              :+: MessageChan SigLog
              :+: MessageChan SigCom
              :+: Error Stop
              :+: Metric WorkMetric
              :+: Metric LogMetric1
             )
             sig
             m
       , MonadIO m
       )
    => m ()
work = forever $ withTwoMessageChan @SigCom @SigLog
    (\case
        SigCom1 Stop           -> liftIO (print "log server stop!") >> throwError Stop
        SigCom2 (WorkInfo tmv) -> do
            WorkEnv a b <- ask
            resp tmv (a, b)
        SigCom3 (AllCycle tmv) -> do
            v           <- getVal w_total
            WorkEnv a b <- ask
            resp tmv (b, v)
    )
    (\case
        SigLog1 l -> inc w_total >> inc log_all >> liftIO (print l)
        SigLog2 (Allmetric tmv) -> getAll @LogMetric1 Proxy >>= resp tmv
        SigLog3 _               -> undefined
        SigLog4 _               -> undefined
    )

manager
    :: ( HasWorkGroup "work" SigCom '[Stop , WorkInfo , AllCycle] sig m
       , MonadIO m
       )
    => m ()
manager = do
    logChan <- liftIO newTChanIO

    let logServer name idx chan =
            void
                $ runReader (WorkEnv name idx)
                $ runWorkerWithChan @SigCom chan
                $ runServerWithChan @SigLog logChan
                $ runMetric @WorkMetric
                $ runMetric @LogMetric1
                $ runError @Stop work

    createWorker @SigCom $ logServer "logServer" 1

    rs <- callAll @"work" WorkInfo

    liftIO $ print rs

    castAll @"work" Stop

    liftIO $ forever $ do
        threadDelay 1000000


data WorkEnv = WorkEnv
    { name :: String
    , nid  :: Int
    }
    deriving Show

runAll :: IO ()
runAll = void $ runWithWorkGroup @"work" manager
