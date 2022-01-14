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
import           Type
import           Unsafe.Coerce                  ( unsafeCoerce )

type HasServer (serverName :: Symbol) s ts sig m
    = ( Elems serverName ts (ToList s)
      , HasLabelled serverName (SeverReq s ts) sig m
      )

type SeverReq :: (Type -> Type)
            -> [Type]
            -> (Type -> Type)
            -> Type
            -> Type
data SeverReq s ts m a where
    SendReq ::(ToSig t s) =>t -> SeverReq s ts m ()

sendReq
    :: forall serverName s ts sig m t
     . ( Elem serverName t ts
       , ToSig t s
       , HasLabelled (serverName :: Symbol) (SeverReq s ts) sig m
       )
    => t
    -> m ()
sendReq t = sendLabelled @serverName (SendReq t)

call
    :: forall serverName s ts sig m e b
     . ( Elem serverName e ts
       , ToSig e s
       , MonadIO m
       , HasLabelled (serverName :: Symbol) (SeverReq s ts) sig m
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
       , HasLabelled (serverName :: Symbol) (SeverReq s ts) sig m
       )
    => e
    -> m ()
cast f = do
    -- liftIO $ putStrLn "send cast"
    sendReq @serverName f

newtype SeverReqC s ts m a = SeverReqC { unHasServerC :: ReaderC (Chan (Sum s ts)) m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance (Algebra sig m, MonadIO m) => Algebra (SeverReq s ts :+: sig) (SeverReqC s ts m) where
    alg hdl sig ctx = SeverReqC $ ReaderC $ \c -> case sig of
        L (SendReq t) -> do
            liftIO $ writeChan c (inject t)
            pure ctx
        R signa -> alg (runReader c . unHasServerC . hdl) signa ctx
-- client
runWithServer
    :: forall serverName s ts m a
     . Chan (Some s)
    -> Labelled (serverName :: Symbol) (SeverReqC s ts) m a
    -> m a
runWithServer chan f =
    runReader (unsafeCoerce chan) $ unHasServerC $ runLabelled f

-- server 
type ToServerMessage f = Reader (Chan (Some f))

serverHelper
    :: forall f es sig m
     . (Has (ToServerMessage f) sig m, MonadIO m)
    => (forall s . f s -> m ())
    -> m ()
serverHelper f = forever $ do
    tc     <- ask @(Chan (Some f))
    Some v <- liftIO $ readChan tc
    f v

runServerWithChan :: forall f m a . Chan (Some f) -> ReaderC (Chan (Some f)) m a -> m a
runServerWithChan = runReader

resp :: (MonadIO m) => MVar a -> a -> m ()
resp tmv a = liftIO $ putMVar tmv a
