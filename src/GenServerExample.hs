{-# LANGUAGE GADTs, TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
module GenServerExample where
import           Control.Carrier.Error.Either
import           Control.Carrier.Reader
import           Control.Concurrent
import           Control.Effect.Labelled
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Default.Class
import           Data.Proxy
import           GenServer
import           GenServerTH
import           Metrics

data Message1  where
     Message1 ::String -> MVar String -> Message1
newtype Message2 = Message2 String
data Stop = Stop
newtype GetMetric1 = GetMetric1 (MVar Int)
newtype PutLog = PutLog String
newtype GetAllMetric = GetAllMetric (MVar [Int])

mkSigAndClass "SigMessage"
  [ ''Message1
  , ''Message2
  , ''Stop
  , ''GetMetric1
  , ''PutLog
  , ''GetAllMetric
  ]

type T = '[Message1 , Message2 , Stop , GetMetric1 , PutLog , GetAllMetric]

makeMetrics "ClientMetric" ["total_loop", "t_m1", "t_m2", "t_str"]

client
    :: ( HasLabelled GenServer (GenServer SigMessage T) sig m
       , Has (Error Stop :+: Metric ClientMetric) sig m
       , MonadIO m
       )
    => m a
client = forever $ do
    addOne total_loop

    liftIO $ putStr "input: "
    li <- liftIO getLine
    case li of
        "stop" -> cast Stop >> throwError Stop
        "m1"   -> addOne t_m1 >> call GetMetric1 >>= liftIO . print
        "all"  -> call GetAllMetric >>= liftIO . print
        _      -> do
            addOne t_str
            val <- call (Message1 li)
            liftIO $ do
                putStr "received: "
                putStrLn val

logClient
    :: (HasLabelled GenServer (GenServer SigMessage T) sig m, MonadIO m) => m ()
logClient = forever $ do
    cast (PutLog "hello")
    liftIO $ threadDelay 1000000

makeMetrics "SomeMetric" ["m1", "m2", "m3", "m4", "putlog"]

type SType = Sum SigMessage T

type TC = Chan SType

server :: (Has (Metric SomeMetric :+: Reader TC) sig m, MonadIO m) => m ()
server = forever $ do
    tc    <- ask @TC
    Sum v <- liftIO $ readChan tc
    handler (Sum v)

handler :: (Has (Metric SomeMetric) sig m, MonadIO m) => SType -> m ()
handler (Sum v) = case v of
    SigMessage1 (Message1 a b) -> do
        addOne m1
        liftIO $ do
            putStr "  receive message1: "
            print a
            putStrLn $ "  send respon: " ++ (a ++ " received")
            putMVar b (a ++ " received")
    SigMessage2 mes            -> pure ()
    SigMessage3 Stop           -> liftIO $ print "Stop"
    SigMessage4 (GetMetric1 v) -> do
        addOne m2
        getVal m1 >>= liftIO . putMVar v
    SigMessage5 (PutLog s) -> do
        addOne putlog
    SigMessage6 (GetAllMetric v) -> do
        all <- getAll @SomeMetric Proxy
        liftIO $ putMVar v all

runExample :: IO (Either Stop a)
runExample = do
    tc <- newChan
    forkIO $ runReader tc $ runMetric @SomeMetric server
    replicateM_ 10 $ do 
        threadDelay 100000
        forkIO $ runGenServerWith tc logClient
    runGenServerWith tc $ runMetric @ClientMetric $ runError @Stop client
