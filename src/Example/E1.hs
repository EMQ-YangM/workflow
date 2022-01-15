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
import           Data.Proxy
import           Example.Type
import           HasServer
import           Metric
import           Util

client :: (HasServer "log" SigLog1 '[Log , Allmetric] sig m, MonadIO m) => m ()
client = do
    cast @"log" $ Log L1 "val"
    cast @"log" $ Log L2 "val"
    cast @"log" $ Log L3 "val"
    cast @"log" $ Log L4 "val"
    v <- call @"log" Allmetric
    cast @"log" $ Log L1 (show v)

logServer
    :: (Has (ToServerMessage SigLog1 :+: Metric LogMetric1) sig m, MonadIO m)
    => m ()
logServer = forever $ serverHelper @SigLog1 $ \case
    SigLog11 l               -> inc log_all >> liftIO (print l)
    SigLog12 (Allmetric tmv) -> getAll @LogMetric1 Proxy >>= resp tmv

run :: IO ()
run = void $ do
    logChan <- newTChanIO

    forkIO $ void $ runReader logChan $ runMetric @LogMetric1 logServer

    runWithServer @"log" logChan client
    forever $ do
        threadDelay 1000000
