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
module Example.HasServerExample where
import           Control.Carrier.Error.Either
import           Control.Carrier.Reader
import           Control.Carrier.State.Strict
import           Control.Concurrent
import           Control.Effect.Labelled
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Proxy
import           GHC.TypeLits
import           HasServer
import           TH
import           Metric
import           System.Random
import           Text.Read

data Message1  where
     Message1 ::String -> MVar String -> Message1
    --  Message1 ::String -> MVar String %1 -> Message1
    --                   add linear type  (call function, server must response)
data Stop = Stop
newtype GetAllMetric = GetAllMetric (MVar [Int])

data WriteUser = WriteUser Int String
data GetUser = GetUser Int (MVar (Maybe String))
newtype GetDBSize = GetDBSize (MVar Int)
newtype GetAllUser = GetAllUser (MVar [Int])
newtype DeleteAll = DeleteAll (MVar Int)

data LogMessage = LogMessage String String

mkSigAndClass "SigMessage"
  [ ''Message1
  , ''GetAllMetric
  ]

mkSigAndClass "SigDB"
  [ ''WriteUser
  , ''GetAllMetric
  , ''GetUser
  , ''GetDBSize
  , ''GetAllUser
  , ''DeleteAll
  ]

mkSigAndClass "SigLog"
  [ ''LogMessage
  , ''GetAllMetric
  ]

data Add1 = Add1
data Sub1 = Sub1

mkSigAndClass "SigAdd" [''Add1, ''Sub1, ''GetAllMetric]

mkMetric "ClientMetric" ["total_loop", "t_m1", "t_m2", "t_str"]

client
    :: ( HasLabelledServer "Some" SigMessage '[Message1 , GetAllMetric] sig m
       , HasLabelledServer "log" SigLog '[LogMessage , GetAllMetric] sig m
       , HasLabelledServer
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
       , HasLabelledServer "add" SigAdd '[Add1 , Sub1 , GetAllMetric] sig m
       , Has (Error Stop :+: Metric ClientMetric) sig m
       , MonadIO m
       )
    => m a
client = forever $ do
    li <- liftIO getLine
    case words li of
        ["size"] -> do
            v <- call @"db" GetDBSize
            cast @"log" $ LogMessage "client" $ "DB size: " ++ show v
        "+1" : _ -> do
            cast @"add" Add1
        "-1" : _ -> do
            cast @"add" Sub1
        "num" : _ -> do
            v <- call @"add" GetAllMetric
            cast @"log" $ LogMessage "client" $ "num is " ++ show v
        "deleteAll" : _ -> do
            v <- call @"db" DeleteAll
            cast @"log" $ LogMessage "client" $ "DB dlete total: " ++ show v
        "getAllUser" : _ -> do
            v <- call @"db" GetAllUser
            cast @"log" $ LogMessage "client" $ "result: " ++ show v
        "writeUser" : idx : val -> do
            let num = readMaybe @Int idx
            case num of
                Nothing -> liftIO $ print "input error"
                Just n  -> do
                    cast @"db" $ WriteUser n (concat val)
        "getUser" : idx -> do
            let num = readMaybe @Int (concat idx)
            case num of
                Nothing -> liftIO $ print "input error"
                Just n  -> do
                    v <- call @"db" (GetUser n)
                    cast @"log" $ LogMessage "client" $ "result: " ++ show v
        _ -> do
            val <- call @"Some" (Message1 li)
            cast @"log" $ LogMessage "client" val

            cast @"log" $ LogMessage "client" "writeDB "

            idx <- liftIO randomIO
            val <- liftIO $ replicateM 4 randomIO
            cast @"db" (WriteUser idx val)
            val <- call @"db" (GetUser idx)
            cast @"log" $ LogMessage "client" $ "readDB result is: " ++ show val

            allMetric <- call @"db" GetAllMetric
            cast @"log"
                $  LogMessage "client"
                $  "DB all metrics: "
                ++ show allMetric

            allMetric <- call @"Some" GetAllMetric
            cast @"log"
                $  LogMessage "client"
                $  "Some all metrics: "
                ++ show allMetric

            allMetric <- call @"log" GetAllMetric
            cast @"log"
                $  LogMessage "client"
                $  "log all metrics: "
                ++ show allMetric

---- DB server

mkMetric "DBmetric" ["db_write", "db_read"]

dbServer
    :: ( HasLabelledServer "log" SigLog '[LogMessage] sig m
       , Has
             ( Reader (Chan (Some SigDB)) :+: State (Map Int String) :+: Metric DBmetric
             )
             sig
             m
       , MonadIO m
       )
    => m ()
dbServer = serverHelper $ \case
    SigDB1 (WriteUser k v) -> do
        inc db_write
        modify (Map.insert k v)
        cast @"log" (LogMessage "dbServer" "write DB")
    SigDB2 (GetAllMetric tmv) -> do
        am <- getAll @DBmetric Proxy
        resp tmv am
    SigDB3 (GetUser k tmv) -> do
        inc db_read
        val <- gets (Map.lookup k)
        resp tmv val
        cast @"log" (LogMessage "dbServer" "read DB")
    SigDB4 (GetDBSize tmv) -> do
        v <- gets @(Map Int String) Map.size
        resp tmv v
    SigDB5 (GetAllUser tmv) -> do
        v <- gets @(Map Int String) Map.keys
        resp tmv v
    SigDB6 (DeleteAll tmv) -> do
        v <- gets @(Map Int String) Map.size
        put @(Map Int String) Map.empty
        resp tmv v
        cast @"log" (LogMessage "dbServer" "delete all user")

--- log server 

mkMetric "LogMetric" ["log_total", "log_t"]

logServer
    :: (Has (Reader (Chan (Some SigLog)) :+: Metric LogMetric) sig m, MonadIO m)
    => m ()
logServer = serverHelper $ \case
    SigLog1 (LogMessage from s) -> do
        v <- getVal log_total
        inc log_total
        liftIO $ do
            putStrLn $ show v ++ ": " ++ from ++ ": " ++ s
    SigLog2 (GetAllMetric tmv) -> do
        inc log_t
        am <- getAll @LogMetric Proxy
        resp tmv am

---- Some server

mkMetric "SomeMetric" ["m1", "m2", "m3", "m4", "putlog"]

server
    :: ( HasLabelled "log" (HasServer SigLog '[LogMessage]) sig m
       , Has
             ( Metric SomeMetric :+: Reader (Chan (Some SigMessage)) :+: Error Stop
             )
             sig
             m
       , MonadIO m
       )
    => m ()
server = serverHelper $ \case
    SigMessage1 (Message1 a b) -> do
        inc m1
        cast @"log" (LogMessage "server" a)
        resp b a
    SigMessage2 (GetAllMetric tmv) -> do
        am <- getAll @SomeMetric Proxy
        resp tmv am

mkMetric "AddMetric" ["add_total"]

addServer
    :: ( HasLabelled "log" (HasServer SigLog '[LogMessage]) sig m
       , Has (Reader (Chan (Some SigAdd)) :+: Metric AddMetric) sig m
       , MonadIO m
       )
    => m ()
addServer = serverHelper $ \case
    SigAdd1 Add1 -> do
        inc add_total
        cast @"log" $ LogMessage "addServer" "add 1"
    SigAdd2 Sub1               -> do 
         dec add_total
         cast @"log" $ LogMessage "addServer" "sub 1"
    SigAdd3 (GetAllMetric tmv) -> do
        am <- getAll @AddMetric Proxy
        resp tmv am

---- 
runExample :: IO (Either Stop a)
runExample = do
    tc   <- newChan
    dbc  <- newChan
    tlc  <- newChan
    addc <- newChan
    --- fork some server , need log server
    forkIO
        $ void
        $ runHasServerWith @"log" tlc
        $ runReader tc
        $ runMetric @SomeMetric
        $ runError @Stop server

    -- - fork db server, need log server
    forkIO
        $ void
        $ runHasServerWith @"log" tlc
        $ runReader dbc
        $ runMetric @DBmetric
        $ runState @(Map Int String) Map.empty dbServer

    --- fork log server , need db server
    forkIO $ void $ runReader tlc $ runMetric @LogMetric logServer

    forkIO
        $ void
        $ runHasServerWith @"log" tlc
        $ runReader addc
        $ runMetric @AddMetric addServer

    -- run client
    runHasServerWith @"Some" tc
        $ runHasServerWith @"db" dbc
        $ runHasServerWith @"log" tlc
        $ runHasServerWith @"add" addc
        $ runMetric @ClientMetric
        $ runError @Stop client
