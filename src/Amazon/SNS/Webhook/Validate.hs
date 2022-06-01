{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Amazon.SNS.Webhook.Validate
  ( validateSnsMessage
  , handleSubscription
  , SNSNotificationValidationError(..)
  , ValidSNSMessage(..)
  ) where

import Amazon.SNS.Webhook.Prelude

import Amazon.SNS.Webhook.Payload
import Control.Monad (when)
import Data.ByteArray.Encoding (Base(Base64), convertFromBase)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (catMaybes)
import Data.PEM (pemContent, pemParseLBS)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.X509
  ( HashALG(..)
  , PubKeyALG(..)
  , SignatureALG(..)
  , SignedCertificate
  , certPubKey
  , decodeSignedCertificate
  , getCertificate
  )
import Data.X509.Validation
  (SignatureFailure, SignatureVerification(..), verifySignature)
import Network.HTTP.Simple
  (Response, getResponseBody, getResponseStatusCode, httpLbs, parseRequest_)
import Safe (headMay)

data ValidSNSMessage
  = SNSMessage Text
  | SNSSubscribe SNSSubscription
  | SNSUnsubscribe SNSSubscription
  deriving stock (Show, Eq)

-- | Validate SNS notification
--
-- SNS messages are validated through their signature. The algorithm is detailed
-- in the documentation below.
--
-- <https://docs.aws.amazon.com/sns/latest/dg/sns-verify-signature-of-message.html>
--
validateSnsMessage :: MonadIO m => SNSPayload -> m ValidSNSMessage
validateSnsMessage payload@SNSPayload {..} = do
  signature <- unTry BadSignature $ convertFromBase Base64 $ encodeUtf8
    snsSignature
  signedCert <- retrieveCertificate payload
  let
    valid = verifySignature
      (SignatureALG HashSHA1 PubKeyALG_RSA)
      (certPubKey $ getCertificate signedCert)
      (unsignedSignature payload)
      signature
  liftIO $ print (unsignedSignature payload)
  case valid of
    SignaturePass -> pure $ case snsTypePayload of
      Notification{} -> SNSMessage snsMessage
      SubscriptionConfirmation x -> SNSSubscribe x
      UnsubscribeConfirmation x -> SNSUnsubscribe x
    SignatureFailed err -> throwIO $ InvalidPayload err

retrieveCertificate :: MonadIO m => SNSPayload -> m SignedCertificate
retrieveCertificate SNSPayload {..} = do
  response <- httpLbs $ parseRequest_ $ T.unpack snsSigningCertURL
  pems <- unTry BadPem $ pemParseLBS $ getResponseBody response
  cert <-
    fromMaybeM (throwIO $ BadPem "Empty List") $ pemContent <$> headMay pems
  unTry BadCert $ decodeSignedCertificate cert

unsignedSignature :: SNSPayload -> ByteString
unsignedSignature SNSPayload {..} =
  encodeUtf8 $ mconcat $ (<> "\n") <$> catMaybes
    [ Just "Message"
    , Just snsMessage
    , Just "MessageId"
    , Just snsMessageId
    , "SubscrieURL" <$ mSubscribeUrl
    , mSubscribeUrl
    , "Subject" <$ mSubject
    , mSubject
    , Just "Timestamp"
    , Just snsTimestamp
    , "Token" <$ mToken
    , mToken
    , Just "TopicArn"
    , Just snsTopicArn
    , Just "Type"
    , Just snsType
    ]
 where
  (mSubject, mToken, mSubscribeUrl) = case snsTypePayload of
    Notification x -> (snsSubject x, Nothing, Nothing)
    SubscriptionConfirmation x ->
      (Nothing, Just $ snsToken x, Just $ snsSubscribeURL x)
    UnsubscribeConfirmation x ->
      (Nothing, Just $ snsToken x, Just $ snsSubscribeURL x)

handleSubscription :: MonadIO m => ValidSNSMessage -> m Text
handleSubscription = \case
  SNSMessage t -> pure t
  SNSSubscribe SNSSubscription {..} -> do
    response <- httpLbs $ parseRequest_ $ T.unpack snsSubscribeURL
    when (getResponseStatusCode response >= 300) $ do
      throwIO $ BadSubscription response
    throwIO SubscribeMessageResponded
  SNSUnsubscribe{} -> throwIO UnsubscribeMessage

data SNSNotificationValidationError
  = BadPem String
  | BadSignature String
  | BadCert String
  | BadJSONParse String
  | BadSubscription (Response LBS.ByteString)
  | InvalidPayload SignatureFailure
  | MissingMessageTypeHeader
  | UnsubscribeMessage
  | SubscribeMessageResponded
  deriving stock Show
  deriving anyclass Exception
