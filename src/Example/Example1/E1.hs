{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
module Example.Example1.E1 where

import           Control.Algebra
import           Control.Carrier.Reader
import           Control.Carrier.State.Strict
import           Control.Concurrent
import           Control.Concurrent.STM         ( newTChanIO )
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Proxy
import           Example.Example1.Type
import           Process.HasServer
import           Process.Metric
import           Process.Util

client
    :: ( HasServer "log" SigLog '[Log , Allmetric , P , SetLevel] sig m
       , MonadIO m
       )
    => m ()
client = do
    call @"log" $ SetLevel "1" L2
    cast @"log" $ Log L1 "client" "val"
    cast @"log" $ Log L2 "client" "val"
    cast @"log" $ Log L3 "client" "val"
    cast @"log" $ Log L4 "client" "val"
    v <- call @"log" Allmetric
    cast @"log" $ Log L1 "client" (show v)
    cast @"log" $ P 10010

client1 :: (HasServer "log" SigLog '[Log] sig m, MonadIO m) => m ()
client1 = do
    cast @"log" $ Log L1 "client1" "val"
    cast @"log" $ Log L2 "client1" "val"
    cast @"log" $ Log L3 "client1" "val"
    cast @"log" $ Log L4 "client1" "val"


logServer
    :: ( Has (MessageChan SigLog :+: State Level :+: Metric LogMetric1) sig m
       , MonadIO m
       )
    => m ()
logServer = forever $ withMessageChan @SigLog $ \case
    SigLog1 l@(Log lv _ _) -> do
        inc log_all
        lv' <- get @Level
        if lv' <= lv then liftIO (print l) else pure ()
    SigLog2 (Allmetric tmv  ) -> getAll @LogMetric1 Proxy >>= resp tmv
    SigLog3 (P         v    ) -> liftIO (print v)
    SigLog4 (SetLevel token lv tmv) -> do
        put lv
        resp tmv ()

run :: IO ()
run = void $ do
    logChan <- newMessageChan

    forkIO $ void $ runReader logChan $ runState L1 $ runMetric @LogMetric1
        logServer

    forkIO $ void $ runWithServer @"log" logChan client

    forkIO $ void $ runWithServer @"log" logChan client1

    forever $ do
        threadDelay 1000000
