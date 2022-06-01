module Amazon.SNS.Verify.Prelude
  ( module X
  , module Amazon.SNS.Verify.Prelude
  ) where

import Prelude as X

import Control.Exception as X (Exception)
import qualified Control.Exception
import Control.Monad as X (join, (<=<))
import Control.Monad.IO.Class as X (MonadIO, liftIO)
import Data.ByteString as X (ByteString)
import Data.Text as X (Text)
import Data.Traversable as X (for)

throwIO :: (MonadIO m, Exception e) => e -> m a
throwIO = liftIO . Control.Exception.throwIO

unTry :: (MonadIO m, Exception e) => (a -> e) -> Either a b -> m b
unTry e = either (throwIO . e) pure

fromMaybeM :: Monad m => m a -> Maybe a -> m a
fromMaybeM f = maybe f pure
