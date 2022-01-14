{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Util where
import           Control.Carrier.Reader         ( Has
                                                , Reader
                                                , ReaderC
                                                , ask
                                                , runReader
                                                )
import           Control.Concurrent             ( MVar
                                                , putMVar
                                                )
import           Control.Concurrent.STM         ( STM
                                                , TChan
                                                , atomically
                                                , isEmptyTChan
                                                , orElse
                                                , readTChan
                                                )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Type                           ( Some(..) )

waitEither :: TChan f -> TChan l -> STM (Either f l)
waitEither left right =
    (Left <$> readTChan left) `orElse` (Right <$> readTChan right)

resp :: (MonadIO m) => MVar a -> a -> m ()
resp tmv a = liftIO $ putMVar tmv a

-- server 
type ToServerMessage f = Reader (TChan (Some f))

serverHelper
    :: forall f es sig m
     . (Has (ToServerMessage f) sig m, MonadIO m)
    => (forall s . f s -> m ())
    -> m ()
serverHelper f = do
    tc     <- ask @(TChan (Some f))
    Some v <- liftIO $ atomically $ readTChan tc
    f v

runServerWithChan
    :: forall f m a . TChan (Some f) -> ReaderC (TChan (Some f)) m a -> m a
runServerWithChan = runReader

-- work
type ToWrokMessage f = Reader (TChan (Some f))

workHelper
    :: forall f es sig m
     . (Has (Reader (TChan (Some f))) sig m, MonadIO m)
    => (forall s . f s -> m ())
    -> m ()
    -> m ()
workHelper f w = do
    tc  <- ask @(TChan (Some f))
    isE <- liftIO $ atomically (isEmptyTChan tc)
    if isE then w else go tc
  where
    go tc = do
        Some v <- liftIO $ atomically $ readTChan tc
        f v
        isE <- liftIO $ atomically (isEmptyTChan tc)
        if isE then pure () else go tc

runWorkerWithChan
    :: forall f m a . TChan (Some f) -> ReaderC (TChan (Some f)) m a -> m a
runWorkerWithChan = runReader
