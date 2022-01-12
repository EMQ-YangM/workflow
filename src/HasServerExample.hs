{-# LANGUAGE GADTs, TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module HasServerExample where
import           Control.Carrier.Error.Either
import           Control.Carrier.Reader
import           Control.Carrier.State.Strict
import           Control.Concurrent
import           Control.Effect.Labelled
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Default.Class
import           Data.Kind
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Proxy
import           GHC.TypeLits
import           HasServer
import           HasServerTH
import           Metrics
import           System.Random

data Message1  where
     Message1 ::String -> MVar String -> Message1
    --  Message1 ::String -> MVar String %1 -> Message1
    --                   add linear type  (call function, server must response)
data Stop = Stop
newtype GetAllMetric = GetAllMetric (MVar [Int])

mkSigAndClass "SigMessage"
  [ ''Message1
  , ''GetAllMetric
  ]

data DBwrite = DBwrite Int String
data DBReader = DBReader Int (MVar (Maybe String))
newtype GetDBSize = GetDBSize (MVar Int)

mkSigAndClass "SigDB"
  [ ''DBwrite
  , ''GetAllMetric
  , ''DBReader
  , ''GetDBSize
  ]

newtype LogMessage = LogMessage String

mkSigAndClass "SigLog"
 [ ''LogMessage
 , ''GetAllMetric
 ]

type T = '[Message1 , GetAllMetric]
type DB = '[DBwrite , GetAllMetric , DBReader , GetDBSize]
type Log = '[LogMessage , GetAllMetric]

makeMetrics "ClientMetric" ["total_loop", "t_m1", "t_m2", "t_str"]

client
    :: ( HasLabelled "Some" (HasServer SigMessage T) sig m
       , HasLabelled "db" (HasServer SigDB DB) sig m
       , HasLabelled "log" (HasServer SigLog Log) sig m
       , Has (Error Stop :+: Metric ClientMetric) sig m
       , MonadIO m
       )
    => m a
client = forever $ do
    li <- liftIO getLine
    cast @"log" (LogMessage li)

    case li of
        "size" -> do
            v <- call @"db" GetDBSize
            cast @"log" $ LogMessage $ "DB size: " ++ show v
        _ -> do
            val <- call @"Some" (Message1 li)
            cast @"log" $ LogMessage val

    cast @"log" $ LogMessage "writeDB "

    idx <- liftIO randomIO
    val <- liftIO $ replicateM 4 randomIO
    cast @"db" (DBwrite idx val)
    val <- call @"db" (DBReader idx)
    cast @"log" $ LogMessage $ "readDB result is: " ++ show val

    allMetric <- call @"db" GetAllMetric
    cast @"log" $ LogMessage $ "DB all metrics: " ++ show allMetric

    allMetric <- call @"Some" GetAllMetric
    cast @"log" $ LogMessage $ "Some all metrics: " ++ show allMetric

    allMetric <- call @"log" GetAllMetric
    cast @"log" $ LogMessage $ "log all metrics: " ++ show allMetric

---- DB server

makeMetrics "DBmetric" ["db_write", "db_read"]

dbServer
    :: ( HasLabelled "log" (HasServer SigLog Log) sig m
       , Has
             ( Reader (Chan (Some SigDB)) :+: State (Map Int String) :+: Metric DBmetric
             )
             sig
             m
       , MonadIO m
       )
    => m ()
dbServer = serverHelper $ \case
    SigDB1 (DBwrite k v) -> do
        inc db_write
        modify (Map.insert k v)
        cast @"log" (LogMessage "write DB")
    SigDB2 (GetAllMetric tmv) -> do
        am <- getAll @DBmetric Proxy
        liftIO $ putMVar tmv am
    SigDB3 (DBReader k tmv) -> do
        inc db_read
        val <- gets (Map.lookup k)
        liftIO $ putMVar tmv val
        cast @"log" (LogMessage "read DB")
    SigDB4 (GetDBSize tmv) -> do
        v <- gets @(Map Int String) Map.size
        liftIO $ putMVar tmv v

--- log server 

makeMetrics "LogMetric" ["log_total", "log_t"]

logServer
    :: (Has (Reader (Chan (Some SigLog)) :+: Metric LogMetric) sig m, MonadIO m)
    => m ()
logServer = serverHelper $ \case
    SigLog1 (LogMessage s) -> do
        v <- getVal log_total
        inc log_total
        liftIO $ do
            putStrLn $ show v ++ ": " ++ s
    SigLog2 (GetAllMetric tmv) -> do
        inc log_t
        am <- getAll @LogMetric Proxy
        liftIO $ putMVar tmv am

---- Some server

makeMetrics "SomeMetric" ["m1", "m2", "m3", "m4", "putlog"]

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
        cast @"log" (LogMessage $ "server to log: " ++ a)
        liftIO $ putMVar b a
    SigMessage2 (GetAllMetric tmv) -> do
        am <- getAll @SomeMetric Proxy
        liftIO $ putMVar tmv am

---- 
runExample :: IO (Either Stop a)
runExample = do
    tc  <- newChan
    dbc <- newChan
    tlc <- newChan
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

    --- fork log server 
    forkIO $ void $ runReader tlc $ runMetric @LogMetric logServer

    -- run client
    runHasServerWith @"Some" tc
        $ runHasServerWith @"db" dbc
        $ runHasServerWith @"log" tlc
        $ runMetric @ClientMetric
        $ runError @Stop client
