{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

module Amazon.SNS.Webhook
  ( requireSNSMessage
  , requireSNSMessageJSON
  ) where

import Amazon.SNS.Webhook.Prelude

import Amazon.SNS.Webhook.Payload
import Amazon.SNS.Webhook.Validate
import Data.Aeson (FromJSON, Value, eitherDecode)
import Data.Aeson.Types (Result(Error, Success), fromJSON)
import Data.ByteString.Lazy (fromStrict)
import Data.Text.Encoding (encodeUtf8)

-- | Decode and validate an SNS message as JSON
--
-- The message type header is checked. Subscription requests are replied to and
-- thrown.
--
requireSNSMessageJSON :: (FromJSON a, MonadIO m) => Value -> m a
requireSNSMessageJSON =
  unTry BadJSONParse
    . eitherDecode
    . fromStrict
    . encodeUtf8
    <=< requireSNSMessage

-- | Decode and validate an SNS message
--
-- The message type header is checked. Subscription requests are replied to and
-- thrown.
--
requireSNSMessage :: MonadIO m => Value -> m Text
requireSNSMessage =
  handleSubscription <=< validateSnsMessage <=< parseSNSPayload

parseSNSPayload :: MonadIO m => Value -> m SNSPayload
parseSNSPayload = unTry BadJSONParse . fromResult . fromJSON

fromResult :: Result a -> Either String a
fromResult = \case
  Success a -> Right a
  Error str -> Left str
