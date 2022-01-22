{-# LANGUAGE GADTs, TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Example.Example1.HasServerExample where
import           Control.Carrier.Error.Either
import           Control.Carrier.Reader
import           Control.Carrier.State.Strict
import           Control.Concurrent
import           Control.Concurrent.STM         ( newTChanIO )
import           Control.Effect.Labelled
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IntMap
import           Data.List                      ( intersect )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Proxy
import           Example.Example1.Servers
import           Example.Example1.Type
import           HasServer
import           HasWorkGroup
import           Metric
import           System.Random
import           Text.Read               hiding ( get )
import           Util


client
    :: ( HasServer "log" SigLog '[Log] sig m
       , HasServer "log1" SigLog '[SetLevel] sig m
       , HasServer
             "db"
             SigDB
             '[ WriteUser
              , GetUser
              , GetDBSize
              , GetAllMetric
              , GetAllUser
              , DeleteAll
              ]
             sig
             m
       , HasServer "add" SigAdd '[Add1 , Sub1 , GetAllMetric] sig m
       , HasServer
             "auth"
             SigAuth
             '[GetToken , VerifyToken , GetAllTokens , DeleteAllTokens]
             sig
             m
       , HasWorkGroup "work" SigCommand '[Finish , Talk] sig m
       , Has (Error Stop :+: Metric ClientMetric :+: Reader Name) sig m
       , MonadIO m
       )
    => m a
client = do
    castAll @"work" (Talk "ready!")
    forever $ do
        li <- liftIO getLine
        case words li of
            ["size"] -> do
                v <- call @"db" GetDBSize
                l1 $ "DB size: " ++ show v
            "set" : token : i -> do
                case readMaybe @Level (concat i) of
                    Nothing -> liftIO $ print "input error"
                    Just le -> do
                        call @"log1" $ SetLevel token le
            "finish" : _ -> do
                l4 "finish all"
                liftIO $ threadDelay 100000
                callAll @"work" Finish
                throwError Stop
            "castAll" : s -> do
                castAll @"work" (Talk (concat s))
            "+1" : _ -> do
                cast @"add" Add1
            "-1" : _ -> do
                cast @"add" Sub1
            "num" : _ -> do
                v <- call @"add" GetAllMetric
                l1 $ "num is" ++ show v
            "deleteAll" : token : _ -> do
                v <- call @"db" $ DeleteAll token
                l4 $ "DB delete all: " ++ show v
            "getAllUser" : _ -> do
                v <- call @"db" GetAllUser
                l1 $ "result: " ++ show v
            "writeUser" : token : idx : val -> do
                let num = readMaybe @Int idx
                case num of
                    Nothing -> liftIO $ print "input error"
                    Just n  -> do
                        cast @"db" $ WriteUser token n (concat val)
            "getUser" : idx -> do
                let num = readMaybe @Int (concat idx)
                case num of
                    Nothing -> liftIO $ print "input error"
                    Just n  -> do
                        v <- call @"db" (GetUser n)
                        l1 $ "result: " ++ show v
            "getToken" : _ -> do
                v <- call @"auth" $ GetToken
                l4 $ "getToken: " ++ v
            "verifyToken" : s -> do
                v <- call @"auth" $ VerifyToken (concat s)
                l3 $ show v
            "getAllTokens" : _ -> do
                v <- call @"auth" GetAllTokens
                l4 $ show v
            "deleteAllTokens" : token : _ -> do
                v <- call @"auth" (DeleteAllTokens token)
                l4 $ "delete all tokens " ++ show v
            _ -> do
                l1 "writeDB"
                idx   <- liftIO randomIO
                val   <- liftIO $ replicateM 4 randomIO
                token <- call @"auth" GetToken
                cast @"db" (WriteUser token idx val)
                val <- call @"db" (GetUser idx)
                l2 $ "readDB result is: " ++ show val

                allMetric <- call @"db" GetAllMetric
                l1 $ "DB all metrics" ++ show allMetric

---- DB server

---- 
example
    :: (HasWorkGroup "work" SigCommand '[Finish , Talk] sig m, MonadIO m)
    => m ()
example = do

    dbChan   <- liftIO $ newMessageChan @SigDB
    logChan  <- liftIO $ newMessageChan @SigLog
    addChan  <- liftIO $ newMessageChan @SigAdd
    authChan <- liftIO $ newMessageChan @SigAuth


    --- fork auth server, need log server
    let auth comm =
            void @IO
                $ runWorkerWithChan comm
                $ runServerWithChan authChan
                -----------------
                $ runWithServer @"log" logChan
                -----------------
                $ runReader "auth"
                $ runState @[String] []
                $ runError @Stop authServer

    createWorker @SigCommand $ auth

    -- - fork db server, need log, auth server
    let db comm =
            void
                $ runWorkerWithChan comm
                $ runServerWithChan dbChan
                -----------------
                $ runWithServer @"log" logChan
                $ runWithServer @"auth" authChan
                -----------------
                $ runReader "db"
                $ runMetric @DBmetric
                $ runError @Stop
                $ runState @(Map Int String) Map.empty dbServer

    createWorker @SigCommand db

    --- fork log server, need auth server
    let log comm =
            void
                $ runWorkerWithChan comm
                $ runServerWithChan logChan
                -----------------
                $ runWithServer @"auth" authChan
                $ runWithServer @"log" logChan
                -----------------
                $ runReader "log"
                $ runError @Stop
                $ runState L1
                $ runMetric @LogMetric logServer

    createWorker @SigCommand log

    -- fork addServer server, need log server
    let add comm =
            void
                $ runWorkerWithChan comm
                $ runServerWithChan addChan
                -----------------
                $ runWithServer @"log" logChan
                -----------------
                $ runReader "add"
                $ runError @Stop
                $ runMetric @AddMetric addServer

    createWorker @SigCommand add

    -- run client, need db, log, log1, add, auth server and w workGroup
    void
        $ runWithServer @"db" dbChan
        $ runWithServer @"log" logChan
        $ runWithServer @"log1" logChan
        $ runWithServer @"add" addChan
        $ runWithServer @"auth" authChan
        -----------------
        $ runReader "client"
        $ runMetric @ClientMetric
        $ runError @Stop client


runExample :: IO ()
runExample = void $ runWithWorkGroup @"work" example

