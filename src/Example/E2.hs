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
import           Metric
import           T
import           TH
import           Type

newtype Stop = Stop (MVar Int)
newtype WorkInfo = WorkInfo (MVar (String, Int))

mkSigAndClass "SigCom"
    [ ''Stop
    , ''WorkInfo
    ]

manager
    :: (HasLabelledServer "work" SigCom '[Stop , WorkInfo] sig m, MonadIO m)
    => m ()
manager = do
    res <- mcall @"work" [1 .. 10] Stop
    liftIO $ print res

data WorkEnv = WorkEnv
    { name :: String
    , nid  :: Int
    }
    deriving Show

work
    :: ( Has
             (Reader (TChan (Some SigCom)) :+: Reader WorkEnv :+: Error String)
             sig
             m
       , MonadIO m
       )
    => m ()
work = serverHelper @SigCom
    (\case
        SigCom1 (Stop tmv) -> do
            WorkEnv _ b <- ask
            resp tmv b
            throwError "stop"
        SigCom2 (WorkInfo tmv) -> do
            WorkEnv a b <- ask
            resp tmv (a, b)
    )
    (do
        WorkEnv a b <- ask
        liftIO $ do
            print (WorkEnv a b)
            threadDelay 100000
    )

runAll :: IO ()
runAll = void $ do
    tcs <- replicateM 10 newTChanIO
    for_ (zip [1 ..] tcs) $ \(idx, t) -> do
        forkIO
            $ void
            $ runReader t
            $ runReader (WorkEnv (show idx) idx)
            $ runError @String work
    forkIO $ void $ runWithServer @"work" (zip [1 ..] tcs) manager
    forever $ do
        liftIO $ threadDelay 1000000
