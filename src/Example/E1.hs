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
import           Control.Carrier.State.Strict
import           Control.Concurrent
import           Control.Concurrent.STM         ( newTChanIO )
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Proxy
import           Example.Type
import           HasServer
import           Metric
import           Util

client
    :: ( HasServer "log" SigLog1 '[Log , Allmetric , P , SetLevel] sig m
       , MonadIO m
       )
    => m ()
client = do
    call @"log" $ SetLevel "1" L2
    cast @"log" $ Log L1 "val"
    cast @"log" $ Log L2 "val"
    cast @"log" $ Log L3 "val"
    cast @"log" $ Log L4 "val"
    v <- call @"log" Allmetric
    cast @"log" $ Log L1 (show v)
    cast @"log" $ P 10010

client1 :: (HasServer "log" SigLog1 '[Log] sig m, MonadIO m) => m ()
client1 = do
    cast @"log" $ Log L1 "val"
    cast @"log" $ Log L2 "val"
    cast @"log" $ Log L3 "val"
    cast @"log" $ Log L4 "val"


logServer
    :: ( Has (MessageChan SigLog1 :+: State Level :+: Metric LogMetric1) sig m
       , MonadIO m
       )
    => m ()
logServer = forever $ withMessageChan @SigLog1 $ \case
    SigLog11 l@(Log lv _) -> do
        inc log_all
        lv' <- get @Level
        if lv' <= lv then liftIO (print l) else pure ()
    SigLog12 (Allmetric tmv  ) -> getAll @LogMetric1 Proxy >>= resp tmv
    SigLog13 (P         v    ) -> liftIO (print v)
    SigLog14 (SetLevel token lv tmv) -> do
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
