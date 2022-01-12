{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, LambdaCase #-}
module T where
import           Control.Carrier.Reader
import           Control.Concurrent
import           Control.Effect.Labelled
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Kind
import           GHC.TypeLits                   ( ErrorMessage(Text)
                                                , Symbol
                                                , TypeError
                                                )

type family TS (a :: [Type]) :: (Type -> Type)

newtype A = A (MVar Int)
data B = B
data C = C

data SigT a where
    SigT1 ::A -> SigT A
    SigT2 ::B -> SigT B
    SigT3 ::C -> SigT C

type instance TS '[A , B , C] = SigT

instance ToSig A SigT where
    toSig ms = SigT1 ms

type Sum :: (Type -> Type) -> [Type] -> Type
data Sum f (r :: [*]) where
    Sum ::(TS r) t -> Sum f r

class ToSig a b where
    toSig :: a -> b a

type family Elem (t :: Type) (ts :: [Type]) :: Constraint where
    Elem t '[] = TypeError ('Text "not in req list")
    Elem t (t ': xs) = ()
    Elem t (t1 ': xs) = Elem t xs

inject :: (ToSig e (TS r), Elem e r) => e -> Sum f r
inject = Sum . toSig

type HasServer
  :: [Type] -> (Type -> Type) -> Type -> Type
data HasServer ts m a where
    SendReq ::(Elem t ts, ToSig t (TS ts)) => t -> HasServer ts m ()

sendReq
    :: forall serverName ts sig m t
     . ( HasLabelled (serverName :: Symbol) (HasServer ts) sig m
       , Elem t ts
       , ToSig t (TS ts)
       )
    => t
    -> m ()
sendReq t = sendLabelled @serverName (SendReq t)

call
    :: forall serverName ts sig m e b
     . ( HasLabelled (serverName :: Symbol) (HasServer ts) sig m
       , Elem e ts
       , ToSig e (TS ts)
       , MonadIO m
       )
    => (MVar b -> e)
    -> m b
call f = do
    -- liftIO $ putStrLn "send call, wait response"
    mvar <- liftIO newEmptyMVar
    sendReq @serverName (f mvar)
    liftIO $ takeMVar mvar

newtype HasServerC ts m a = HasServerC { unHasServerC :: ReaderC (Chan (Sum (TS ts) ts)) m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance (Algebra sig m, MonadIO m) => Algebra (HasServer ts :+: sig) (HasServerC ts m) where
    alg hdl sig ctx = HasServerC $ ReaderC $ \c -> case sig of
        L (SendReq t) -> do
            liftIO $ writeChan c (inject t)
            pure ctx
        R signa -> alg (runReader c . unHasServerC . hdl) signa ctx

runHasServerWith
    :: forall serverName ts m a v
     . (v ~  TS ts)
    => Chan (Sum v ts)
    -> Labelled (serverName :: Symbol) (HasServerC ts) m a
    -> m a
runHasServerWith chan f = runReader chan $ unHasServerC $ runLabelled f

runHasServer
    :: forall serverName ts m a
     . MonadIO m
    => Labelled (serverName :: Symbol) (HasServerC ts) m a
    -> m a
runHasServer f = do
    chan <- liftIO newChan
    runHasServerWith @serverName chan f

client :: (HasLabelled "t" (HasServer '[A , B , C]) sig m, MonadIO m) => m ()
client = do
    call @"t" A
    undefined

server :: Chan (SigT a) -> IO ()
server chan = do
    readChan chan
        >>= (\case
                SigT1 a -> undefined
                SigT2 b -> undefined
                SigT3 c -> undefined
            )
    undefined

runClient :: IO ()
runClient = void $ do
    tc <- newChan
    runHasServerWith @"t" tc client
    undefined
