{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
module Example.E2 where

import           Control.Algebra
import           Control.Carrier.Error.Either
import           Control.Carrier.Reader
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Foldable                  ( for_ )
import           Data.Proxy
import           Example.Type
import           HasServer
import           HasWorkGroup            hiding ( resp )
import           Metric
import           Util

manager
    :: ( HasWorkGroup "work" SigCom '[Stop , WorkInfo , AllCycle] sig m
       , HasServer "log" SigLog1 '[Log , Allmetric] sig m
       , MonadIO m
       )
    => m ()
manager = do
    res <- callById @"work" 1 WorkInfo
    cast @"log" (Log L1 (show res))

    replicateM_ 10 $ cast @"log" (Log L1 "v")

    v <- call @"log" Allmetric
    cast @"log" (Log L4 $ show v)

    res <- callById @"work" 1 AllCycle
    cast @"log" (Log L3 (show res))

    liftIO $ threadDelay 1000000
    castById @"work" 1 Stop

data WorkEnv = WorkEnv
    { name :: String
    , nid  :: Int
    }
    deriving Show

work
    :: ( Has
             (    Reader WorkEnv
              :+: MessageChan SigLog1
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
work = forever $ withTwoMessageChan @SigCom @SigLog1
    (\case
        SigCom1 Stop           -> throwError Stop
        SigCom2 (WorkInfo tmv) -> do
            WorkEnv a b <- ask
            resp tmv (a, b)
        SigCom3 (AllCycle tmv) -> do
            v           <- getVal w_total
            WorkEnv a b <- ask
            resp tmv (b, v)
    )
    (\case
        SigLog11 l -> inc w_total >> inc log_all >> liftIO (print l)
        SigLog12 (Allmetric tmv) -> getAll @LogMetric1 Proxy >>= resp tmv
    )

runAll :: IO ()
runAll = void $ do
    tcs     <- replicateM 1 newTChanIO
    logChan <- newTChanIO

    for_ (zip [1 ..] tcs) $ \(idx, t) -> do
        forkIO
            $ void
            $ runReader (WorkEnv (show idx) idx)
            $ runWorkerWithChan @SigCom t
            $ runServerWithChan @SigLog1 logChan
            $ runMetric @WorkMetric
            $ runMetric @LogMetric1
            $ runError @Stop work

    void $ runWithServer @"log" logChan $ runWithWorkGroup @"work"
        (zip [1 ..] tcs)
        manager

    forever $ do
        liftIO $ threadDelay 1000000
