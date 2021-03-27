module Specstrom.Channel (Receive, Send, newChannel, send, receive, tryReceive, tryReceiveTimeout, unreceive) where

import Control.Concurrent.STM (TQueue, atomically, newTQueueIO, readTQueue, tryReadTQueue, writeTQueue, unGetTQueue, TVar, STM, check, readTVar, registerDelay)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad ((<=<))
import Control.Applicative ((<|>))

newtype Receive a = Receive (TQueue a)
newtype Send a = Send (TQueue a)

newChannel :: MonadIO m => m (Receive a, Send a)
newChannel = do
  queue <- liftIO newTQueueIO
  pure (Receive queue, Send queue)

send :: MonadIO m => Send a -> a -> m ()
send (Send output) msg = liftIO (atomically (writeTQueue output msg))

receive :: MonadIO m => Receive a -> m a
receive (Receive input) = liftIO (atomically (readTQueue input))

tryReceive :: MonadIO m => Receive a -> m (Maybe a)
tryReceive (Receive input) = liftIO (atomically (tryReadTQueue input))

tryReceiveTimeout :: MonadIO m => Receive a -> Int -> m (Maybe a)
tryReceiveTimeout (Receive input) t = liftIO (readTQueueTimeout t input)

-- Read the next value from a TQueue or timeout
readTQueueTimeout :: Int -> TQueue a -> IO (Maybe a)
readTQueueTimeout timeout q = do
  delay <- registerDelay timeout
  atomically $
    Just <$> readTQueue q
      <|> Nothing <$ fini delay
  where
    fini :: TVar Bool -> STM ()
    fini = check <=< readTVar

unreceive :: MonadIO m => Receive a -> a -> m ()
unreceive (Receive input) a = liftIO (atomically (unGetTQueue input a))
