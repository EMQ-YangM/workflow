module Util where
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad.IO.Class

waitEither :: TChan f -> TChan l -> STM (Either f l)
waitEither left right =
    (Left <$> readTChan left) `orElse` (Right <$> readTChan right)

resp :: (MonadIO m) => MVar a -> a -> m ()
resp tmv a = liftIO $ putMVar tmv a
