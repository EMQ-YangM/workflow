{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell  #-}
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
import           Example.E1
import           Example.Type
import           HasServer
import           HasWorkGroup            hiding ( resp )
import           Metric
import           System.Random
import           TH
import           Type
import           Util

newtype WorkInfo = WorkInfo (MVar (String, Int))
newtype AllCycle = AllCycle (MVar (Int, Int))

mkSigAndClass "SigCom"
    [ ''Stop
    , ''WorkInfo
    , ''AllCycle
    ]

manager
    :: ( HasWorkGroup "work" SigCom '[Stop , WorkInfo , AllCycle] sig m
       , HasServer "log" SigLog1 '[Log , Allmetric] sig m
       , MonadIO m
       )
    => m ()
manager = do
    res <- mcall @"work" [1 .. 10] WorkInfo
    cast @"log" (Log L1 (show res))

    res <- mcall @"work" [1 .. 10] AllCycle
    cast @"log" (Log L1 (show res))

    liftIO $ threadDelay 1000000
    mcast @"work" [1 .. 10] Stop

    v <- call @"log" Allmetric
    cast @"log" (Log L1 $ show v)

data WorkEnv = WorkEnv
    { name :: String
    , nid  :: Int
    }
    deriving Show

mkMetric "WorkMetric" ["w_total"]

work
    :: ( HasServer "log" SigLog1 '[Log] sig m
       , Has
             ( ToWrokMessage SigCom :+: Reader WorkEnv :+: Error Stop :+: Metric WorkMetric
             )
             sig
             m
       , MonadIO m
       )
    => m ()
work = forever $ workHelper @SigCom
    (\case
        SigCom1 Stop -> do
            WorkEnv a b <- ask
            cast @"log" (Log L4 (a ++ " work stop"))
            throwError Stop
        SigCom2 (WorkInfo tmv) -> do
            WorkEnv a b <- ask
            resp tmv (a, b)
        SigCom3 (AllCycle tmv) -> do
            v           <- getVal w_total
            WorkEnv a b <- ask
            resp tmv (b, v)
    )
    (do
        WorkEnv a b <- ask
        inc w_total
        cast @"log" (Log L1 $ "work is running, it's id " ++ a)
        liftIO $ do
            i <- randomRIO (100, 10000)
            threadDelay i
    )

runAll :: IO ()
runAll = void $ do
    tcs     <- replicateM 10 newTChanIO
    logChan <- newTChanIO

    forkIO $ void $ runWithServer @"log" logChan $ runWithWorkGroup @"work"
        (zip [1 ..] tcs)
        manager

    for_ (zip [1 ..] tcs) $ \(idx, t) -> do
        forkIO
            $ void
            $ runWorkerWithChan @SigCom t
            $ runReader (WorkEnv (show idx) idx)
            $ runWithServer @"log" logChan
            $ runMetric @WorkMetric
            $ runError @Stop work


    forkIO $ void $ runReader logChan $ runMetric @LogMetric1 logServer

    forever $ do
        liftIO $ threadDelay 1000000

fun :: IO ()
fun = do
    l <- newTChanIO @Int
    r <- newTChanIO @Bool
    atomically $ writeTChan r True
    v <- atomically $ waitEither l r
    print v
