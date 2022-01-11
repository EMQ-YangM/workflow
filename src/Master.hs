{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes, TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Master where
import           Control.Carrier.Error.Either
import           Control.Carrier.Reader
import           Control.Concurrent
import           Control.Effect.Labelled
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Default.Class
import           Data.Kind                      ( Constraint
                                                , Type
                                                )
import           GHC.TypeLits                   ( ErrorMessage(Text)
                                                , TypeError
                                                )
import           Metrics

data Sum f (r :: [*]) where
    Sum ::f t -> Sum f r

class ToSig a b where
    toSig :: a -> b a

type family Elem (t :: Type) (ts :: [Type]) :: Constraint where
    Elem t '[] = TypeError ('Text "not in req list")
    Elem t (t ': xs) = ()
    Elem t (t1 ': xs) = Elem t xs

inject :: (ToSig e f, Elem e r) => e -> Sum f r
inject = Sum . toSig

type Master :: (Type -> Type)
            -> [Type]
            -> (Type -> Type)
            -> Type
            -> Type
data Master s ts m a where
    SendReq ::(Elem t ts, ToSig t s) => t -> Master s ts m ()

sendReq
    :: (HasLabelled Master (Master s ts) sig m, Elem t ts, ToSig t s)
    => t
    -> m ()
sendReq t = sendLabelled @Master (SendReq t)

call
    :: forall s ts sig m e b
     . ( HasLabelled Master (Master s ts) sig m
       , Elem e ts
       , ToSig e s
       , MonadIO m
       )
    => (MVar b -> e)
    -> m b
call f = do
    liftIO $ putStrLn "send call, wait response"
    mvar <- liftIO newEmptyMVar
    sendReq (f mvar)
    liftIO $ takeMVar mvar

cast
    :: forall s ts sig m e b
     . ( HasLabelled Master (Master s ts) sig m
       , Elem e ts
       , ToSig e s
       , MonadIO m
       )
    => e
    -> m ()
cast f = do
    liftIO $ putStrLn "send cast"
    sendReq f

newtype MasterC s ts m a = MasterC { unMasterC :: ReaderC (Chan (Sum s ts)) m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance (Algebra sig m, MonadIO m) => Algebra (Master s ts :+: sig) (MasterC s ts m) where
    alg hdl sig ctx = MasterC $ ReaderC $ \c -> case sig of
        L (SendReq t) -> do
            liftIO $ writeChan c (inject t)
            pure ctx
        R signa -> alg (runReader c . unMasterC . hdl) signa ctx

runMasterWith :: Chan (Sum s ts) -> Labelled Master (MasterC s ts) m a -> m a
runMasterWith chan f = runReader chan $ unMasterC $ runLabelled f

runMaster :: MonadIO m => Labelled Master (MasterC s ts) m a -> m a
runMaster f = do
    chan <- liftIO newChan
    runMasterWith chan f

-- example

data Message1  where
     Message1 ::String -> MVar String -> Message1
newtype Message2 = Message2 String
data Stop = Stop
data GetMetric = GetMetric String (MVar Int)

data SigMessage a where
    SigMessage1 ::Message1 -> SigMessage Message1
    SigMessage2 ::Message2 -> SigMessage Message2
    SigMessage3 ::Stop -> SigMessage Stop
    SigMessage4 ::GetMetric -> SigMessage GetMetric

instance ToSig Message1 SigMessage where
    toSig ms = SigMessage1 ms

instance ToSig Message2 SigMessage where
    toSig ms = SigMessage2 ms

instance ToSig Stop SigMessage where
    toSig ms = SigMessage3 ms

instance ToSig GetMetric SigMessage where
    toSig ms = SigMessage4 ms

type T = '[Message1 , Message2 , Stop , GetMetric]
type TC = Chan (Sum SigMessage T)

master
    :: ( HasLabelled Master (Master SigMessage T) sig m
       , Has (Error Stop) sig m
       , MonadIO m
       )
    => m a
master = do
    forever $ do
        liftIO $ putStr "input: "
        li <- liftIO getLine
        case li of
            "stop" -> cast Stop >> throwError Stop
            "m1"   -> call (GetMetric "m1") >>= liftIO . print
            "m2"   -> call (GetMetric "m2") >>= liftIO . print
            _      -> do
                val <- call (Message1 li)
                liftIO $ do
                    putStr "received: "
                    putStrLn val

makeMetrics "SomeMetric" ["m1", "m2", "m3"]

worker :: (Has (Metric SomeMetric :+: Reader TC) sig m, MonadIO m) => m ()
worker = do
    tc    <- ask @TC
    Sum v <- liftIO $ readChan tc
    case v of
        SigMessage1 (Message1 a b) -> do
            addOne m1
            liftIO $ do
                putStr "  receive message1: "
                print a
                putStrLn $ "  send respon: " ++ (a ++ " received")
                putMVar b (a ++ " received")
            worker
        SigMessage2 mes                -> worker
        SigMessage3 Stop               -> liftIO $ print "Stop"
        SigMessage4 (GetMetric name v) -> do
            addOne m2
            case name of
                "m1" -> getVal m1 >>= liftIO . putMVar v
                "m2" -> getVal m2 >>= liftIO . putMVar v
            worker

runVal :: IO (Either Stop a)
runVal = do
    tc <- newChan
    forkIO $ runReader tc $ runMetric @SomeMetric worker
    runMasterWith tc $ runError @Stop master
