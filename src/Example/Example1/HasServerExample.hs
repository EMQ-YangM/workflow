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
import           Example.Example1.Type
import           HasServer
import           HasWorkGroup
import           Metric
import           System.Random
import           Text.Read               hiding ( get )
import           Util
import Example.Example1.Type (VerifyToken(VerifyToken))


client
    :: ( HasServer "Some" SigMessage '[Message1 , GetAllMetric] sig m
       , HasServer "log" SigLog '[Log] sig m
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
       , HasServer "auth" SigAuth '[GetToken , VerifyToken , GetAllTokens, DeleteAllTokens] sig m
       , HasWorkGroup "w" SigCommand '[Finish , Talk] sig m
       , Has (Error Stop :+: Metric ClientMetric :+: Reader Name) sig m
       , MonadIO m
       )
    => m a
client = do
    castAll @"w" (Talk "ready!")
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
                callAll @"w" Finish
                throwError Stop
            "castAll" : s -> do
                castAll @"w" (Talk (concat s))
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
                val <- call @"Some" (Message1 li)
                l1 "writeDB"
                idx   <- liftIO randomIO
                val   <- liftIO $ replicateM 4 randomIO
                token <- call @"auth" GetToken
                cast @"db" (WriteUser token idx val)
                val <- call @"db" (GetUser idx)
                l2 $ "readDB result is: " ++ show val

                allMetric <- call @"db" GetAllMetric
                l1 $ "DB all metrics" ++ show allMetric

                allMetric <- call @"Some" GetAllMetric
                l1 $ "Some all metrics" ++ show allMetric

---- DB server
handCommand
    :: ( HasServer "log" SigLog '[Log] sig m
       , Has (Error Stop :+: Reader Name) sig m
       , MonadIO m
       )
    => SigCommand f
    -> m ()
handCommand = \case
    SigCommand1 (Finish tmv) -> do
        name <- ask @Name
        liftIO $ putStrLn $ name ++ " server stop"
        resp tmv ()
        throwError Stop
    SigCommand2 (Talk s) -> do
        name <- ask @Name
        l4 $ ", talk " ++ s

dbServer
    :: ( HasServer "log" SigLog '[Log] sig m
       , HasServer "auth" SigAuth '[VerifyToken] sig m
       , Has
             (MessageChan SigDB
             :+: MessageChan SigCommand
             :+: State (Map Int String)
             :+: Metric DBmetric
             :+: Error Stop
             :+: Reader Name
             )
             sig
             m
       , MonadIO m
       )
    => m ()
dbServer = forever $ withTwoMessageChan @SigCommand @SigDB
    handCommand
    (\case
        SigDB1 (WriteUser token k v) -> do
            vt <- call @"auth" $ VerifyToken token
            if vt
                then do
                    inc db_write
                    modify (Map.insert k v)
                    l1 "write DB successed"
                else l4 "write DB filed"
        SigDB2 (GetAllMetric tmv) -> do
            am <- getAll @DBmetric Proxy
            resp tmv am
        SigDB3 (GetUser k tmv) -> do
            inc db_read
            val <- gets (Map.lookup k)
            resp tmv val
            l1 "read DB"
        SigDB4 (GetDBSize tmv) -> do
            v <- gets @(Map Int String) Map.size
            resp tmv v
        SigDB5 (GetAllUser tmv) -> do
            v <- gets @(Map Int String) Map.keys
            resp tmv v
        SigDB6 (DeleteAll token tmv) -> do
            vt <- call @"auth" $ VerifyToken token
            if vt
                then do
                    v <- gets @(Map Int String) Map.size
                    put @(Map Int String) Map.empty
                    resp tmv v
                    l4 "delete all user"
                else do
                    resp tmv 0
                    l4 "verify token faild, you can't delete db"
    )

--- log server 

logServer
    :: ( HasServer "log" SigLog '[Log] sig m
       , HasServer "auth" SigAuth '[VerifyToken] sig m
       , Has (MessageChan SigLog
            :+: MessageChan SigCommand
            :+: Metric LogMetric
            :+: Error Stop
            :+: Reader Name
            :+: State Level
            )
            sig
            m
        , MonadIO m)
    => m ()
logServer = do
    forever $ withTwoMessageChan @SigCommand @SigLog handCommand $ \case
        SigLog1 l@(Log lv from s) -> do
            lv' <- get @Level
            if lv' <= lv
                then do
                    v <- getVal log_total
                    inc log_total
                    liftIO $ putStrLn $ show v ++ ": " ++ show l
                else pure ()
        SigLog2 (Allmetric tmv) -> do
            inc log_t
            am <- getAll @LogMetric Proxy
            resp tmv am
        SigLog3 _                       -> undefined
        SigLog4 (SetLevel token lv tmv) -> do
            t <- call @"auth" $ VerifyToken token
            if t
                then put lv >> liftIO (print "set level success")
                else liftIO (print "verifyToken faild, set level faild")
            resp tmv ()

---- Some server

server
    :: ( HasServer "log" SigLog '[Log] sig m
       , Has (Metric SomeMetric
             :+: MessageChan SigMessage
             :+: MessageChan SigCommand
             :+: Error Stop
             :+: Reader Name
             ) sig m
       , MonadIO m
       )
    => m ()
server =
    forever $ withTwoMessageChan @SigCommand @SigMessage handCommand $ \case
        SigMessage1 (Message1 a b) -> do
            inc m1
            l2 a
            resp b a
        SigMessage2 (GetAllMetric tmv) -> do
            am <- getAll @SomeMetric Proxy
            resp tmv am

addServer
    :: ( HasServer "log" SigLog '[Log] sig m
       , Has (MessageChan SigAdd
             :+: Metric AddMetric
             :+: MessageChan SigCommand
             :+: Error Stop
             :+: Reader Name
             ) sig m
       , MonadIO m
       )
    => m ()
addServer =
    forever $ withTwoMessageChan @SigCommand @SigAdd handCommand $ \case
        SigAdd1 Add1 -> do
            inc add_total
            l1 "add1"
        SigAdd2 Sub1 -> do
            dec add_total
            l1 "sub1"
        SigAdd3 (GetAllMetric tmv) -> do
            am <- getAll @AddMetric Proxy
            resp tmv am

-- Auth server
authServer
    :: ( HasServer "log" SigLog '[Log] sig m
       , HasServer "auth" SigAuth '[VerifyToken] sig m
       , Has (MessageChan SigAuth
            :+: State [String]
            :+: MessageChan SigCommand
            :+: Error Stop
            :+: Reader Name
            ) sig m
       , MonadIO m)
    => m ()
authServer =
    forever $ withTwoMessageChan @SigCommand @SigAuth handCommand $ \case
        SigAuth1 (GetToken tmv) -> do
            rs <- replicateM 4 $ randomRIO ('a', 'z')
            modify (rs :)
            l4 $ "create token: " ++ rs
            resp tmv rs
        SigAuth2 (VerifyToken s tmv) -> do
            v <- gets @[String] (s `elem`)
            l4 $ "verify token [" ++ s ++ "] " ++ show v
            resp tmv v
        SigAuth3 (GetAllTokens tmv) -> do
            s <- get @[String]
            resp tmv s
        SigAuth4 (DeleteAllTokens token tmv) -> do
            if token == "admin"
                then put @[String] [] >> l4 ("delete all tokens by " ++ token) >> resp tmv True
                else l4 ("delete all tokens failed by" ++ token) >> resp tmv False

---- 
runExample :: IO ()
runExample = do
    tc              <- newMessageChan @SigMessage
    dbc             <- newMessageChan @SigDB
    tlc             <- newMessageChan @SigLog
    addc            <- newMessageChan @SigAdd
    authc           <- newMessageChan @SigAuth

    [a, b, c, d, e] <- replicateM 5 $ newMessageChan @SigCommand

    --- fork auth server, need log server
    forkIO
        $ void
        $ runWithServer @"log" tlc
        $ runWithServer @"auth" authc
        $ runServerWithChan authc
        $ runWorkerWithChan a
        $ runState @[String] []
        $ runReader "auth"
        $ runError @Stop authServer

    --- fork some server , need log server
    forkIO
        $ void
        $ runWithServer @"log" tlc
        $ runServerWithChan tc
        $ runMetric @SomeMetric
        $ runWorkerWithChan b
        $ runReader "some"
        $ runError @Stop server

    -- - fork db server, need log, auth server
    forkIO
        $ void
        $ runWithServer @"log" tlc
        $ runWithServer @"auth" authc
        $ runServerWithChan dbc
        $ runWorkerWithChan c
        $ runMetric @DBmetric
        $ runError @Stop
        $ runReader "db"
        $ runState @(Map Int String) Map.empty dbServer

    --- fork log server, need auth server
    forkIO
        $ void
        $ runWithServer @"auth" authc
        $ runWithServer @"log" tlc
        $ runServerWithChan tlc
        $ runWorkerWithChan d
        $ runError @Stop
        $ runReader "log"
        $ runState L1
        $ runMetric @LogMetric logServer

    -- fork addServer server, need log server
    forkIO
        $ void
        $ runWithServer @"log" tlc
        $ runServerWithChan addc
        $ runWorkerWithChan e
        $ runError @Stop
        $ runReader "add"
        $ runMetric @AddMetric addServer

    -- run client, need Some, db, log, log1, add, auth server and w workGroup
    forkIO
        $ void
        $ runWithServer @"Some" tc
        $ runWithServer @"db" dbc
        $ runWithServer @"log" tlc
        $ runWithServer @"log1" tlc
        $ runWithServer @"add" addc
        $ runWithServer @"auth" authc
        $ runMetric @ClientMetric
        $ runWithWorkGroup @"w" [(1, a), (2, b), (3, c), (4, d), (5, e)]
        $ runReader "client"
        $ runError @Stop client

    forever $ do
        threadDelay 1000000

l_fun
    :: (HasServer "log" SigLog '[Log] sig m, Has (Reader Name) sig m, MonadIO m)
    => Level
    -> String
    -> m ()
l_fun l s = do
    name <- ask @Name
    cast @"log" (Log l name s)

l1
    :: (HasServer "log" SigLog '[Log] sig m, Has (Reader Name) sig m, MonadIO m)
    => String
    -> m ()
l1 = l_fun L1
l2
    :: (HasServer "log" SigLog '[Log] sig m, Has (Reader Name) sig m, MonadIO m)
    => String
    -> m ()
l2 = l_fun L2
l3
    :: (HasServer "log" SigLog '[Log] sig m, Has (Reader Name) sig m, MonadIO m)
    => String
    -> m ()
l3 = l_fun L3
l4
    :: (HasServer "log" SigLog '[Log] sig m, Has (Reader Name) sig m, MonadIO m)
    => String
    -> m ()
l4 = l_fun L4
