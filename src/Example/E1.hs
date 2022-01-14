{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
module Example.E1 where

import           Control.Algebra
import           Control.Carrier.Reader
import           Control.Concurrent
import           Control.Concurrent.STM         ( newTChanIO )
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Default.Class
import           Data.Foldable
import           Data.Proxy
import           HasServer
import           Metric
import           TH
import           Type

type Name = String

data Level = L1 | L2 | L3 | L4

data Log = Log Level String
newtype Allmetric = Allmetric (MVar [Int])

instance Show Level where
    show = \case
        L1 -> "😎"
        L2 -> "🥶"
        L3 -> "👿"
        L4 -> "👾"

instance Show Log where
    show (Log l s) = show l ++ " " ++ s

mkSigAndClass "SigLog"
    [ ''Log
    , ''Allmetric
    ]

client :: (HasServer "log" SigLog '[Log , Allmetric] sig m, MonadIO m) => m ()
client = do
    cast @"log" $ Log L1 "val"
    cast @"log" $ Log L2 "val"
    cast @"log" $ Log L3 "val"
    cast @"log" $ Log L4 "val"
    v <- call @"log" Allmetric
    cast @"log" $ Log L1 (show v)

mkMetric "LogMetric" ["log_all"]

logServer
    :: (Has (ToServerMessage SigLog :+: Metric LogMetric) sig m, MonadIO m)
    => m ()
logServer = serverHelper @SigLog $ \case
    SigLog1 l               -> inc log_all >> liftIO (print l)
    SigLog2 (Allmetric tmv) -> getAll @LogMetric Proxy >>= resp tmv

run :: IO ()
run = void $ do
    logChan <- newTChanIO

    forkIO $ void $ runReader logChan $ runMetric @LogMetric logServer

    runWithServer @"log" logChan client
    forever $ do
        threadDelay 1000000
