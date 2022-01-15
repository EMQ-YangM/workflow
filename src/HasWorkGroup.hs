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
{-# LANGUAGE ConstraintKinds #-}

module HasWorkGroup where
import           Control.Carrier.Reader         ( Algebra
                                                , Has
                                                , Reader
                                                , ReaderC(..)
                                                , ask
                                                , runReader
                                                )
import           Control.Concurrent             ( MVar
                                                , newEmptyMVar
                                                , putMVar
                                                , takeMVar
                                                )
import           Control.Concurrent.STM         ( TChan
                                                , atomically
                                                , isEmptyTChan
                                                , readTChan
                                                , writeTChan
                                                )
import           Control.Effect.Labelled        ( type (:+:)(..)
                                                , Algebra(..)
                                                , Has
                                                , HasLabelled
                                                , Labelled
                                                , LabelledMember
                                                , runLabelled
                                                , sendLabelled
                                                )
import           Control.Monad                  ( forM
                                                , forM_
                                                , forever
                                                , replicateM
                                                )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IntMap
import           Data.Kind                      ( Type )
import           Data.Traversable               ( for )
import           GHC.TypeLits                   ( Symbol )
import           Type                           ( Elem
                                                , Elems
                                                , Some(..)
                                                , Sum
                                                , ToList
                                                , ToSig
                                                , inject
                                                )
import           Unsafe.Coerce                  ( unsafeCoerce )

type HasWorkGroup (serverName :: Symbol) s ts sig m
    = ( Elems serverName ts (ToList s)
      , HasLabelled serverName (Request s ts) sig m
      )

type Request :: (Type -> Type)
            -> [Type]
            -> (Type -> Type)
            -> Type
            -> Type
data Request s ts m a where
    SendReq ::(ToSig t s) =>Int -> t -> Request s ts m ()
    SendAllCall ::(ToSig t s) => (MVar b -> t) -> Request s ts m [MVar b]
    SendAllCast ::(ToSig t s) => t -> Request s ts m ()

sendReq
    :: forall (serverName :: Symbol) s ts sig m t
     . ( Elem serverName t ts
       , ToSig t s
       , HasLabelled serverName (Request s ts) sig m
       )
    => Int
    -> t
    -> m ()
sendReq i t = sendLabelled @serverName (SendReq i t)

sendAllCall
    :: forall (serverName :: Symbol) s ts sig m t b
     . ( Elem serverName t ts
       , ToSig t s
       , HasLabelled serverName (Request s ts) sig m
       )
    => (MVar b -> t)
    -> m [MVar b]
sendAllCall t = sendLabelled @serverName (SendAllCall t)

sendAllCast
    :: forall (serverName :: Symbol) s ts sig m t b
     . ( Elem serverName t ts
       , ToSig t s
       , HasLabelled serverName (Request s ts) sig m
       )
    => t
    -> m ()
sendAllCast t = sendLabelled @serverName (SendAllCast t)

callById
    :: forall serverName s ts sig m e b
     . ( Elem serverName e ts
       , ToSig e s
       , MonadIO m
       , HasLabelled (serverName :: Symbol) (Request s ts) sig m
       )
    => Int
    -> (MVar b -> e)
    -> m b
callById i f = do
    mvar <- liftIO newEmptyMVar
    sendReq @serverName i (f mvar)
    liftIO $ takeMVar mvar

callAll
    :: forall (serverName :: Symbol) s ts sig m t b
     . ( Elem serverName t ts
       , ToSig t s
       , HasLabelled serverName (Request s ts) sig m
       , MonadIO m
       )
    => (MVar b -> t)
    -> m [b]
callAll t = do
    vs <- sendLabelled @serverName (SendAllCall t)
    mapM (liftIO . takeMVar) vs

mcall
    :: forall serverName s ts sig m e b
     . ( Elem serverName e ts
       , ToSig e s
       , MonadIO m
       , HasLabelled (serverName :: Symbol) (Request s ts) sig m
       )
    => [Int]
    -> (MVar b -> e)
    -> m [b]
mcall is f = do
    for is $ \idx -> do
        mvar <- liftIO newEmptyMVar
        v    <- sendReq @serverName idx (f mvar)
        liftIO $ takeMVar mvar

castById
    :: forall serverName s ts sig m e b
     . ( Elem serverName e ts
       , ToSig e s
       , MonadIO m
       , HasLabelled (serverName :: Symbol) (Request s ts) sig m
       )
    => Int
    -> e
    -> m ()
castById i f = do
    sendReq @serverName i f

castAll
    :: forall (serverName :: Symbol) s ts sig m t b
     . ( Elem serverName t ts
       , ToSig t s
       , HasLabelled serverName (Request s ts) sig m
       , MonadIO m
       )
    => t
    -> m ()
castAll t = sendLabelled @serverName (SendAllCast t)

mcast
    :: forall serverName s ts sig m e b
     . ( Elem serverName e ts
       , ToSig e s
       , MonadIO m
       , LabelledMember serverName (Request s ts) sig
       , Algebra sig m
       )
    => [Int]
    -> e
    -> m ()
mcast is f = mapM_ (\x -> castById @serverName x f) is

newtype RequestC s ts m a = RequestC { unRequestC :: ReaderC (IntMap (TChan (Sum s ts))) m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance (Algebra sig m, MonadIO m) => Algebra (Request s ts :+: sig) (RequestC s ts m) where
    alg hdl sig ctx = RequestC $ ReaderC $ \c -> case sig of
        L (SendReq i t) -> do
            case IntMap.lookup i c of
                Nothing -> error "...."
                Just ch -> do
                    liftIO $ atomically $ writeTChan ch (inject t)
                    pure ctx
        L (SendAllCall t) -> do
            mvs <- forM (IntMap.elems c) $ \ch -> do
                mv <- liftIO newEmptyMVar
                liftIO $ atomically $ writeTChan ch (inject (t mv))
                pure mv
            pure (mvs <$ ctx)
        L (SendAllCast t) -> do
            IntMap.traverseWithKey
                (\_ ch -> liftIO $ atomically $ writeTChan ch (inject t))
                c
            pure ctx
        R signa -> alg (runReader c . unRequestC . hdl) signa ctx

runWithWorkGroup
    :: forall serverName s ts m a
     . [(Int, TChan (Some s))]
    -> Labelled (serverName :: Symbol) (RequestC s ts) m a
    -> m a
runWithWorkGroup chan f =
    runReader (unsafeCoerce $ IntMap.fromList chan) $ unRequestC $ runLabelled f
