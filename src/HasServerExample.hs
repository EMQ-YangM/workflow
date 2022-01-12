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

data Message1  where
     Message1 ::String -> MVar String -> Message1

data Stop = Stop

newtype PutLog = PutLog String
newtype GetAllMetric = GetAllMetric (MVar [Int])

mkSigAndClass "SigMessage"
  [ ''Message1
  , ''GetAllMetric
  ]

type T = '[Message1 , GetAllMetric]

data DBwrite = DBwrite Int String
data DBReader = DBReader Int (MVar (Maybe String))

mkSigAndClass "SigDB"
  [ ''DBwrite
  , ''GetAllMetric
  , ''DBReader
  ]

type DB = '[DBwrite , GetAllMetric , DBReader]

makeMetrics "ClientMetric" ["total_loop", "t_m1", "t_m2", "t_str"]

client
    :: ( HasLabelled "Some" (HasServer SigMessage T) sig m
       , HasLabelled "db" (HasServer SigDB DB) sig m
       , Has (Error Stop :+: Metric ClientMetric) sig m
       , MonadIO m
       )
    => m a
client = forever $ do
    liftIO $ putStrLn "writeDB "
    cast @"db" (DBwrite 1 "nice")
    val <- call @"db" (DBReader 1)
    liftIO $ putStrLn $ "readDB result is: " ++ show val

    allMetric <- call @"db" GetAllMetric
    liftIO $ putStrLn $ "DB all metrics: " ++ show allMetric

    allMetric <- call @"Some" GetAllMetric
    liftIO $ putStrLn $ "Some all metrics: " ++ show allMetric

    li  <- liftIO getLine
    val <- call @"Some" (Message1 li)
    liftIO $ do
        putStr "received: "
        putStrLn val

---- DB server
type DBType = Sum SigDB DB
type TB = Chan DBType

makeMetrics "DBmetric" ["db_write", "db_read"]

dbServer
    :: ( Has (Reader TB :+: State (Map Int String) :+: Metric DBmetric) sig m
       , MonadIO m
       )
    => m ()
dbServer = forever $ do
    tc    <- ask @TB
    Sum v <- liftIO $ readChan tc
    dbHandler (Sum v)

dbHandler
    :: (Has (State (Map Int String) :+: Metric DBmetric) sig m, MonadIO m)
    => DBType
    -> m ()
dbHandler (Sum v) = case v of
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

---- Some server

makeMetrics "SomeMetric" ["m1", "m2", "m3", "m4", "putlog"]

type SType = Sum SigMessage T

type TC = Chan SType

server
    :: (Has (Metric SomeMetric :+: Reader TC :+: Error Stop) sig m, MonadIO m)
    => m ()
server = forever $ do
    tc    <- ask @TC
    Sum v <- liftIO $ readChan tc
    handler (Sum v)

handler
    :: (Has (Metric SomeMetric) sig m, Has (Error Stop) sig m, MonadIO m)
    => SType
    -> m ()
handler (Sum v) = case v of
    SigMessage1 (Message1 a b) -> do
        addOne m1
        liftIO $ do
            putStr "  receive message1: "
            print a
            putStrLn $ "  send respon: " ++ (a ++ " received")
            putMVar b (a ++ " received")
    SigMessage2 (GetAllMetric tmv) -> do
        am <- getAll @SomeMetric Proxy
        liftIO $ putMVar tmv am
---- 
runExample :: IO (Either Stop a)
runExample = do
    tc  <- newChan
    dbc <- newChan
    --- fork some server
    forkIO $ void $ runReader tc $ runMetric @SomeMetric $ runError @Stop server

    --- fork db server
    forkIO
        $ void
        $ runReader dbc
        $ runMetric @DBmetric
        $ runState @(Map Int String) Map.empty dbServer

    -- run client
    runHasServerWith @"Some" tc
        $ runHasServerWith @"db" dbc
        $ runMetric @ClientMetric
        $ runError @Stop client
