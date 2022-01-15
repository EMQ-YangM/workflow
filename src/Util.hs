{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
module Util where
import           Control.Algebra
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

type MessageChan f = Reader (TChan (Some f))

waitEither :: TChan f -> TChan l -> STM (Either f l)
waitEither left right =
    (Left <$> readTChan left) `orElse` (Right <$> readTChan right)

resp :: (MonadIO m) => MVar a -> a -> m ()
resp tmv a = liftIO $ putMVar tmv a

-- server 
withMessageChan
    :: forall f es sig m
     . (Has (MessageChan f) sig m, MonadIO m)
    => (forall s . f s -> m ())
    -> m ()
withMessageChan f = do
    tc     <- ask @(TChan (Some f))
    Some v <- liftIO $ atomically $ readTChan tc
    f v

runServerWithChan
    :: forall f m a . TChan (Some f) -> ReaderC (TChan (Some f)) m a -> m a
runServerWithChan = runReader

-- work
runWorkerWithChan
    :: forall f m a . TChan (Some f) -> ReaderC (TChan (Some f)) m a -> m a
runWorkerWithChan = runReader

withTwoMessageChan
    :: forall f g sig m
     . (Has (   MessageChan g
            :+: MessageChan f
            ) sig m, MonadIO m)
    => (forall s . f s -> m ())
    -> (forall s . g s -> m ())
    -> m ()
withTwoMessageChan f1 f2 = do
    f <- ask @(TChan (Some f))
    g <- ask @(TChan (Some g))
    liftIO (atomically (waitEither f g)) >>= \case
        Left  (Some so) -> f1 so
        Right (Some so) -> f2 so

data Three a b c = T1 a | T2 b | T3 c

waitTEither :: TChan f -> TChan g -> TChan l -> STM (Three f g l)
waitTEither t1 t2 t3 =
    (T1 <$> readTChan t1)
        `orElse` (T2 <$> readTChan t2)
        `orElse` (T3 <$> readTChan t3)

withThreeMessageChan
    :: forall f g l sig m
     . (Has (   MessageChan g
            :+: MessageChan f
            :+: MessageChan l
            ) sig m, MonadIO m)
    => (forall s . f s -> m ())
    -> (forall s . g s -> m ())
    -> (forall s . l s -> m ())
    -> m ()
withThreeMessageChan f1 f2 f3 = do
    f <- ask @(TChan (Some f))
    g <- ask @(TChan (Some g))
    l <- ask @(TChan (Some l))
    liftIO (atomically (waitTEither f g l)) >>= \case
        T1 (Some so) -> f1 so
        T2 (Some so) -> f2 so
        T3 (Some so) -> f3 so
