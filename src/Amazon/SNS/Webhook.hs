{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Amazon.SNS.Webhook
  ( requireSNSMessage
  , requireSNSMessageJSON
  ) where

import Prelude

import Amazon.SNS.Webhook.Payload
import Amazon.SNS.Webhook.Validate
import Control.Exception (Exception, throwIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON, eitherDecode)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types (HeaderName)

data Config m = Config
  { requireJSON :: forall json . FromJSON json => m json
  , lookupHeader :: HeaderName -> m (Maybe ByteString)
  }

-- | Decode and validate an SNS message as JSON
--
-- The message type header is checked. Subscription requests are replied to and
-- thrown.
--
requireSNSMessageJSON :: (FromJSON a, MonadIO m) => Config m -> m a
requireSNSMessageJSON config =
  do
      unTry BadJSONParse . eitherDecode
    . fromStrict
    . encodeUtf8
    =<< requireSNSMessage config

-- | Decode and validate an SNS message
--
-- The message type header is checked. Subscription requests are replied to and
-- thrown.
--
requireSNSMessage :: MonadIO m => Config m -> m Text
requireSNSMessage config =
  handleSubscription =<< validateSnsMessage =<< parseSNSPayload config

parseSNSPayload :: MonadIO m => Config m -> m SNSPayload
parseSNSPayload Config {..} = do
  mMessageType <- lookupHeader "x-amz-sns-message-type"
  case mMessageType of
    Nothing -> liftIO $ throwIO MissingMessageTypeHeader
    Just messageType ->
      unTry BadJSONParse
        . parseSNSValueViaMessageType messageType
        =<< requireJSON

unTry :: (MonadIO m, Exception e) => (a -> e) -> Either a b -> m b
unTry e = either (liftIO . throwIO . e) pure
