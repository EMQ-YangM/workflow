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

module GenServer where
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

type GenServer :: (Type -> Type)
            -> [Type]
            -> (Type -> Type)
            -> Type
            -> Type
data GenServer s ts m a where
    SendReq ::(Elem t ts, ToSig t s) => t -> GenServer s ts m ()

sendReq
    :: (HasLabelled GenServer (GenServer s ts) sig m, Elem t ts, ToSig t s)
    => t
    -> m ()
sendReq t = sendLabelled @GenServer (SendReq t)

call
    :: forall s ts sig m e b
     . ( HasLabelled GenServer (GenServer s ts) sig m
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
     . ( HasLabelled GenServer (GenServer s ts) sig m
       , Elem e ts
       , ToSig e s
       , MonadIO m
       )
    => e
    -> m ()
cast f = do
    -- liftIO $ putStrLn "send cast"
    sendReq f

newtype GenServerC s ts m a = GenServerC { unGenServerC :: ReaderC (Chan (Sum s ts)) m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance (Algebra sig m, MonadIO m) => Algebra (GenServer s ts :+: sig) (GenServerC s ts m) where
    alg hdl sig ctx = GenServerC $ ReaderC $ \c -> case sig of
        L (SendReq t) -> do
            liftIO $ writeChan c (inject t)
            pure ctx
        R signa -> alg (runReader c . unGenServerC . hdl) signa ctx

runGenServerWith
    :: Chan (Sum s ts) -> Labelled GenServer (GenServerC s ts) m a -> m a
runGenServerWith chan f = runReader chan $ unGenServerC $ runLabelled f

runGenServer :: MonadIO m => Labelled GenServer (GenServerC s ts) m a -> m a
runGenServer f = do
    chan <- liftIO newChan
    runGenServerWith chan f
