module Amazon.SNS.Verify
  ( verifySNSMessage
  , verifySNSMessageWithSettings
  , verifySNSMessageEither
  , verifySNSMessageEitherWithSettings
  , verifySNSMessageJSON
  , verifySNSMessageJSONWithSettings
  , verifySNSMessageJSONEither
  , verifySNSMessageJSONEitherWithSettings
  , defaultSettings
  , SNSNotificationValidationError (..)
  ) where

import Amazon.SNS.Verify.Prelude

import Amazon.SNS.Verify.Payload
import Amazon.SNS.Verify.Validate
import Control.Error (hoistEither, runExceptT)
import Data.Aeson (FromJSON, Value, eitherDecode)
import Data.Aeson.Types (Result (Error, Success), fromJSON)
import Data.Bifunctor (first)
import Data.ByteString.Lazy (fromStrict)
import Data.Text.Encoding (encodeUtf8)

-- | Decode and verify an SNS message as JSON
--
-- The same as 'verifySNSMessage', but decodes the message as `JSON`.
verifySNSMessageJSON :: (FromJSON a, MonadIO m) => Value -> m a
verifySNSMessageJSON = verifySNSMessageJSONWithSettings defaultSettings

verifySNSMessageJSONWithSettings
  :: (FromJSON a, MonadIO m) => ValidationSettings -> Value -> m a
verifySNSMessageJSONWithSettings settings = unTryIO id <=< verifySNSMessageJSONEitherWithSettings settings

verifySNSMessageJSONEither
  :: (FromJSON a, MonadIO m)
  => Value
  -> m (Either SNSNotificationValidationError a)
verifySNSMessageJSONEither = verifySNSMessageJSONEitherWithSettings defaultSettings

verifySNSMessageJSONEitherWithSettings
  :: (FromJSON a, MonadIO m)
  => ValidationSettings
  -> Value
  -> m (Either SNSNotificationValidationError a)
verifySNSMessageJSONEitherWithSettings settings value =
  join
    . traverse (first BadJSONParse . eitherDecode . fromStrict . encodeUtf8)
    <$> verifySNSMessageEitherWithSettings settings value

-- | Decode and verify an SNS message
--
-- This function follows the process outlined in the following:
--
-- <https://docs.aws.amazon.com/sns/latest/dg/sns-verify-signature-of-message.html>
--
-- A `JSON` payload is:
--
-- 1. Parsed as an SNS message type 'Notification', `SubscriptionConfirmation`,
--    or `UnsubscribeConfirmation`.
-- 2. Verified against its signature.
-- 3. And in the case of subscription events responded to.
verifySNSMessage :: MonadIO m => Value -> m Text
verifySNSMessage = unTryIO id <=< verifySNSMessageEither

verifySNSMessageWithSettings
  :: MonadIO m => ValidationSettings -> Value -> m Text
verifySNSMessageWithSettings settings = unTryIO id <=< verifySNSMessageEitherWithSettings settings

verifySNSMessageEither
  :: MonadIO m => Value -> m (Either SNSNotificationValidationError Text)
verifySNSMessageEither = verifySNSMessageEitherWithSettings defaultSettings

verifySNSMessageEitherWithSettings
  :: MonadIO m
  => ValidationSettings
  -> Value
  -> m (Either SNSNotificationValidationError Text)
verifySNSMessageEitherWithSettings settings value = runExceptT $ do
  payload <- hoistEither $ parseSNSPayload value
  verified <- hoistEither =<< validateSnsMessageWithSettings settings payload
  hoistEither =<< handleSubscription verified

parseSNSPayload :: Value -> Either SNSNotificationValidationError SNSPayload
parseSNSPayload = first BadJSONParse . fromResult . fromJSON

fromResult :: Result a -> Either String a
fromResult = \case
  Success a -> Right a
  Error str -> Left str
