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
    liftIO $ putStrLn "writeDB "

    idx <- liftIO randomIO
    val <- liftIO $ replicateM 4 randomIO
    cast @"db" (DBwrite idx val)
    val <- call @"db" (DBReader idx)
    liftIO $ putStrLn $ "readDB result is: " ++ show val

    allMetric <- call @"db" GetAllMetric
    liftIO $ putStrLn $ "DB all metrics: " ++ show allMetric

    allMetric <- call @"Some" GetAllMetric
    liftIO $ putStrLn $ "Some all metrics: " ++ show allMetric

    allMetric <- call @"log" GetAllMetric
    liftIO $ putStrLn $ "log all metrics: " ++ show allMetric

    li <- liftIO getLine
    cast @"log" (LogMessage li)
    case li of
        "size" -> do
            v <- call @"db" GetDBSize
            liftIO $ putStrLn $ "DB size: " ++ show v
        _ -> do
            val <- call @"Some" (Message1 li)
            liftIO $ do
                putStrLn val

---- DB server

type DBType = Sum SigDB DB

makeMetrics "DBmetric" ["db_write", "db_read"]

dbServer
    :: ( Has
             ( Reader (Chan DBType) :+: State (Map Int String) :+: Metric DBmetric
             )
             sig
             m
       , MonadIO m
       )
    => m ()
dbServer = serverHelper @SigDB @DB $ \case
    SigDB1 (DBwrite k v) -> do
        addOne db_write
        modify (Map.insert k v)
    SigDB2 (GetAllMetric tmv) -> do
        am <- getAll @DBmetric Proxy
        liftIO $ putMVar tmv am
    SigDB3 (DBReader k tmv) -> do
        addOne db_read
        val <- gets (Map.lookup k)
        liftIO $ putMVar tmv val
    SigDB4 (GetDBSize tmv) -> do
        v <- gets @(Map Int String) Map.size
        liftIO $ putMVar tmv v

--- log server 

type LogType = Sum SigLog Log

makeMetrics "LogMetric" ["log_total"]

logServer
    :: (Has (Reader (Chan LogType) :+: Metric LogMetric) sig m, MonadIO m)
    => m ()
logServer = serverHelper @SigLog @Log $ \case
    SigLog1 (LogMessage s) -> do
        addOne log_total
        liftIO $ do
            putStr "log thread print: "
            print s
    SigLog2 (GetAllMetric tmv) -> do
        am <- getAll @LogMetric Proxy
        liftIO $ putMVar tmv am

---- Some server

makeMetrics "SomeMetric" ["m1", "m2", "m3", "m4", "putlog"]

type SType = Sum SigMessage T

server
    :: ( Has (Metric SomeMetric :+: Reader (Chan SType) :+: Error Stop) sig m
       , MonadIO m
       )
    => m ()
server = serverHelper @SigMessage @T $ \case
    SigMessage1 (Message1 a b) -> do
        addOne m1
        liftIO $ do
            putMVar b (a ++ " received")
    SigMessage2 (GetAllMetric tmv) -> do
        am <- getAll @SomeMetric Proxy
        liftIO $ putMVar tmv am

---- 
runExample :: IO (Either Stop a)
runExample = do
    tc  <- newChan
    dbc <- newChan
    tlc <- newChan
    --- fork some server
    forkIO $ void $ runReader tc $ runMetric @SomeMetric $ runError @Stop server

    --- fork db server
    forkIO
        $ void
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
