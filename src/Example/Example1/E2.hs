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
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Foldable                  ( for_ )
import           Data.Proxy
import           Example.Example1.Type
import           HasServer
import           HasWorkGroup
import           Metric
import           Type
import           Util

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
        SigLog1 l -> inc w_total >> inc log_all >> liftIO (print l)
        SigLog2 (Allmetric tmv) -> getAll @LogMetric1 Proxy >>= resp tmv
        SigLog3 _               -> undefined
        SigLog4 _               -> undefined
    )

-- t = --             $ void
--             $ runReader (WorkEnv (show idx) idx)
--             $ runWorkerWithChan @SigCom t
--             $ runServerWithChan @SigLog logChan
--             $ runMetric @WorkMetric
--             $ runMetric @LogMetric1
--             $ runError @Stop work

manager
    :: ( HasWorkGroup "work" SigCom '[Stop , WorkInfo , AllCycle] sig m
       , HasServer "log" SigLog '[Log , Allmetric] sig m
       , MonadIO m
       )
    => TChan (Some SigLog)
    -> m ()
manager tc = do
    let fun chan =
            void
                $ runReader (WorkEnv "nice" 1)
                $ runWorkerWithChan @SigCom chan
                $ runServerWithChan @SigLog tc
                $ runMetric @WorkMetric
                $ runMetric @LogMetric1
                $ runError @Stop work

    forkAwork @"work" Stop fun  -- @"work" fun
    -- forkAwork1 @"work" fun

    -- res <- callById @"work" 1 WorkInfo
    -- cast @"log" (Log L1 "manager" (show res))

    -- replicateM_ 10 $ cast @"log" (Log L1 "manager" "v")

    -- v <- call @"log" Allmetric
    -- cast @"log" (Log L4 "manager" $ show v)

    -- res <- callById @"work" 1 AllCycle
    -- cast @"log" (Log L3 "manager" (show res))

    -- liftIO $ threadDelay 1000000
    castById @"work" 1 Stop

data WorkEnv = WorkEnv
    { name :: String
    , nid  :: Int
    }
    deriving Show


-- runAll :: IO ()
-- runAll = void $ do
--     tcs     <- replicateM 1 newMessageChan
--     logChan <- newMessageChan

--     for_ (zip [1 ..] tcs) $ \(idx, t) -> do
--         forkIO
--             $ void
--             $ runReader (WorkEnv (show idx) idx)
--             $ runWorkerWithChan @SigCom t
--             $ runServerWithChan @SigLog logChan
--             $ runMetric @WorkMetric
--             $ runMetric @LogMetric1
--             $ runError @Stop work

--     void $ runWithServer @"log" logChan $ runWithWorkGroup @"work"
--         (zip [1 ..] tcs)
--         manager

--     forever $ do
--         liftIO $ threadDelay 1000000
