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
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}

module HasServer where
import           Control.Carrier.Error.Either   ( Algebra
                                                , Has
                                                )
import           Control.Carrier.Reader         ( Algebra
                                                , Has
                                                , Reader
                                                , ReaderC(..)
                                                , ask
                                                , runReader
                                                )
import           Control.Concurrent             ( Chan
                                                , MVar
                                                , newChan
                                                , newEmptyMVar
                                                , readChan
                                                , takeMVar
                                                , writeChan
                                                )
import           Control.Concurrent.MVar        ( putMVar )
import           Control.Effect.Labelled        ( type (:+:)(..)
                                                , Algebra(..)
                                                , Has
                                                , HasLabelled
                                                , Labelled
                                                , runLabelled
                                                , sendLabelled
                                                )
import           Control.Monad                  ( forever )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.Kind                      ( Constraint
                                                , Type
                                                )
import           GHC.TypeLits                   ( ErrorMessage
                                                    ( (:<>:)
                                                    , ShowType
                                                    , Text
                                                    )
                                                , Symbol
                                                , TypeError
                                                )
import           Metric
import           Unsafe.Coerce                  ( unsafeCoerce )

data Sum f (r :: [*]) where
    Sum ::f t -> Sum f r

class ToSig a b where
    toSig :: a -> b a

data Some f where
    Some ::f a -> Some f

type family Elem (name :: Symbol) (t :: Type) (ts :: [Type]) :: Constraint where
    Elem name t '[] = TypeError ('Text "server ":<>:
                                 'ShowType name ':<>:
                                 'Text " not add " :<>:
                                 'ShowType t :<>:
                                 'Text " to it method list"
                                 )
    Elem name t (t ': xs) = ()
    Elem name t (t1 ': xs) = Elem name t xs

type family ElemO (name :: Symbol) (t :: Type) (ts :: [Type]) :: Constraint where
    ElemO name t '[] = TypeError ('Text "server ":<>:
                                 'ShowType name ':<>:
                                 'Text " not support method " :<>:
                                 'ShowType t
                                 )
    ElemO name t (t ': xs) = ()
    ElemO name t (t1 ': xs) = ElemO name t xs

type family Elems (name :: Symbol) (ls :: [Type]) (ts :: [Type]) :: Constraint where
    Elems name (l ': ls) ts = (ElemO name l ts, Elems name ls ts)
    Elems name '[] ts = ()

type family ToList (a :: (Type -> Type)) :: [Type]

type HasLabelledServer (serverName :: Symbol) s ts sig m
    = ( Elems serverName ts (ToList s)
      , HasLabelled serverName (HasServer s ts) sig m
      )

inject :: ToSig e f => e -> Sum f r
inject = Sum . toSig

type HasServer :: (Type -> Type)
            -> [Type]
            -> (Type -> Type)
            -> Type
            -> Type
data HasServer s ts m a where
    SendReq ::(ToSig t s) =>t -> HasServer s ts m ()

sendReq
    :: forall serverName s ts sig m t
     . ( Elem serverName t ts
       , ToSig t s
       , HasLabelled (serverName :: Symbol) (HasServer s ts) sig m
       )
    => t
    -> m ()
sendReq t = sendLabelled @serverName (SendReq t)

call
    :: forall serverName s ts sig m e b
     . ( Elem serverName e ts
       , ToSig e s
       , MonadIO m
       , HasLabelled (serverName :: Symbol) (HasServer s ts) sig m
       )
    => (MVar b -> e)
    -> m b
call f = do
    -- liftIO $ putStrLn "send call, wait response"
    mvar <- liftIO newEmptyMVar
    sendReq @serverName (f mvar)
    liftIO $ takeMVar mvar

cast
    :: forall serverName s ts sig m e b
     . ( Elem serverName e ts
       , ToSig e s
       , MonadIO m
       , HasLabelled (serverName :: Symbol) (HasServer s ts) sig m
       )
    => e
    -> m ()
cast f = do
    -- liftIO $ putStrLn "send cast"
    sendReq @serverName f

newtype HasServerC s ts m a = HasServerC { unHasServerC :: ReaderC (Chan (Sum s ts)) m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance (Algebra sig m, MonadIO m) => Algebra (HasServer s ts :+: sig) (HasServerC s ts m) where
    alg hdl sig ctx = HasServerC $ ReaderC $ \c -> case sig of
        L (SendReq t) -> do
            liftIO $ writeChan c (inject t)
            pure ctx
        R signa -> alg (runReader c . unHasServerC . hdl) signa ctx

runHasServerWith
    :: forall serverName s ts m a
     . Chan (Some s)
    -> Labelled (serverName :: Symbol) (HasServerC s ts) m a
    -> m a
runHasServerWith chan f =
    runReader (unsafeCoerce chan) $ unHasServerC $ runLabelled f

runHasServer
    :: forall serverName s ts m a
     . MonadIO m
    => Labelled (serverName :: Symbol) (HasServerC s ts) m a
    -> m a
runHasServer f = do
    chan <- liftIO newChan
    runHasServerWith @serverName chan f

serverHelper
    :: forall f es sig m
     . (Has (Reader (Chan (Some f))) sig m, MonadIO m)
    => (forall s . f s -> m ())
    -> m ()
serverHelper f = forever $ do
    tc     <- ask @(Chan (Some f))
    Some v <- liftIO $ readChan tc
    f v

resp :: (MonadIO m) => MVar a -> a -> m ()
resp tmv a = liftIO $ putMVar tmv a
