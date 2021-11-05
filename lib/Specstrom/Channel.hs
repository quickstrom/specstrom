module Specstrom.Channel (Receive, Send, newChannel, send, receive, tryReceive, unreceive) where

import Control.Concurrent.STM (TQueue, atomically, newTQueueIO, readTQueue, tryReadTQueue, unGetTQueue, writeTQueue)
import Control.Monad.IO.Class (MonadIO, liftIO)

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

unreceive :: MonadIO m => Receive a -> a -> m ()
unreceive (Receive input) a = liftIO (atomically (unGetTQueue input a))
