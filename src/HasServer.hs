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

module HasServer where
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
                                                , Symbol
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

type HasServer :: (Type -> Type)
            -> [Type]
            -> (Type -> Type)
            -> Type
            -> Type
data HasServer s ts m a where
    SendReq ::(Elem t ts, ToSig t s) => t -> HasServer s ts m ()

sendReq
    :: forall serverName s ts sig m t
     . ( HasLabelled (serverName :: Symbol) (HasServer s ts) sig m
       , Elem t ts
       , ToSig t s
       )
    => t
    -> m ()
sendReq t = sendLabelled @serverName (SendReq t)

call
    :: forall serverName s ts sig m e b
     . ( HasLabelled (serverName :: Symbol) (HasServer s ts) sig m
       , Elem e ts
       , ToSig e s
       , MonadIO m
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
     . ( HasLabelled (serverName :: Symbol) (HasServer s ts) sig m
       , Elem e ts
       , ToSig e s
       , MonadIO m
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
     . Chan (Sum s ts)
    -> Labelled (serverName :: Symbol) (HasServerC s ts) m a
    -> m a
runHasServerWith chan f = runReader chan $ unHasServerC $ runLabelled f

runHasServer
    :: forall serverName s ts m a
     . MonadIO m
    => Labelled (serverName :: Symbol) (HasServerC s ts) m a
    -> m a
runHasServer f = do
    chan <- liftIO newChan
    runHasServerWith @serverName chan f
