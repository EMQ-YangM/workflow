{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ConstraintKinds #-}

module T where
import           Control.Carrier.Reader
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Effect.Labelled
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Foldable
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IntMap
import           Data.Kind
import           Data.Traversable               ( for )
import           GHC.TypeLits
import           Type
import           Unsafe.Coerce                  ( unsafeCoerce )

type HasLabelledServer (serverName :: Symbol) s ts sig m
    = ( Elems serverName ts (ToList s)
      , HasLabelled serverName (HasServer s ts) sig m
      )

type HasServer :: (Type -> Type)
            -> [Type]
            -> (Type -> Type)
            -> Type
            -> Type
data HasServer s ts m a where
    SendReq ::(ToSig t s) =>Int -> t -> HasServer s ts m ()

sendReq
    :: forall (serverName :: Symbol) s ts sig m t
     . ( Elem serverName t ts
       , ToSig t s
       , HasLabelled serverName (HasServer s ts) sig m
       )
    => Int
    -> t
    -> m ()
sendReq i t = sendLabelled @serverName (SendReq i t)

call
    :: forall serverName s ts sig m e b
     . ( Elem serverName e ts
       , ToSig e s
       , MonadIO m
       , HasLabelled (serverName :: Symbol) (HasServer s ts) sig m
       )
    => Int
    -> (MVar b -> e)
    -> m b
call i f = do
    mvar <- liftIO newEmptyMVar
    sendReq @serverName i (f mvar)
    liftIO $ takeMVar mvar

mcall
    :: forall serverName s ts sig m e b
     . ( Elem serverName e ts
       , ToSig e s
       , MonadIO m
       , HasLabelled (serverName :: Symbol) (HasServer s ts) sig m
       )
    => [Int]
    -> (MVar b -> e)
    -> m [b]
mcall is f = do
    for is $ \idx -> do
        mvar <- liftIO newEmptyMVar
        v    <- sendReq @serverName idx (f mvar)
        liftIO $ takeMVar mvar

cast
    :: forall serverName s ts sig m e b
     . ( Elem serverName e ts
       , ToSig e s
       , MonadIO m
       , HasLabelled (serverName :: Symbol) (HasServer s ts) sig m
       )
    => Int
    -> e
    -> m ()
cast i f = do
    sendReq @serverName i f


mcast
    :: forall serverName s ts sig m e b
     . ( Elem serverName e ts
       , ToSig e s
       , MonadIO m
       , LabelledMember serverName (HasServer s ts) sig
       , Algebra sig m
       )
    => [Int]
    -> e
    -> m ()
mcast is f = mapM_ (\x -> cast @serverName x f) is

newtype HasServerC s ts m a = HasServerC { unHasServerC :: ReaderC (IntMap (TChan (Sum s ts))) m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance (Algebra sig m, MonadIO m) => Algebra (HasServer s ts :+: sig) (HasServerC s ts m) where
    alg hdl sig ctx = HasServerC $ ReaderC $ \c -> case sig of
        L (SendReq i t) -> do
            case IntMap.lookup i c of
                Nothing -> error "...."
                Just ch -> do
                    liftIO $ atomically $ writeTChan ch (inject t)
                    pure ctx
        R signa -> alg (runReader c . unHasServerC . hdl) signa ctx

runWithServer
    :: forall serverName s ts m a
     . [(Int, TChan (Some s))]
    -> Labelled (serverName :: Symbol) (HasServerC s ts) m a
    -> m a
runWithServer chan f =
    runReader (unsafeCoerce $ IntMap.fromList chan) $ unHasServerC $ runLabelled
        f
serverHelper
    :: forall f es sig m
     . (Has (Reader (TChan (Some f))) sig m, MonadIO m)
    => (forall s . f s -> m ())
    -> m ()
    -> m ()
serverHelper f w = forever $ do
    tc  <- ask @(TChan (Some f))
    isE <- liftIO $ atomically (isEmptyTChan tc)
    if isE then w else go tc
  where
    go tc = do
        Some v <- liftIO $ atomically $ readTChan tc
        f v
        isE <- liftIO $ atomically (isEmptyTChan tc)
        if isE then pure () else go tc

resp :: (MonadIO m) => MVar a -> a -> m ()
resp tmv a = liftIO $ putMVar tmv a
