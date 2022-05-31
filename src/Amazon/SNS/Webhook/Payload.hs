{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Amazon.SNS.Webhook.Payload
  ( parseSNSValueViaMessageType
  , SNSPayload(..)
  , SNSType(..)
  , SNSNotification(..)
  , SNSSubscription(..)
  ) where

import Prelude

import Data.Aeson
  ( FromJSON
  , Value(Object)
  , defaultOptions
  , fieldLabelModifier
  , genericParseJSON
  , parseJSON
  , withObject
  , (.:)
  )
import Data.Aeson.Types (parseEither)
import Data.ByteString (ByteString)
import Data.Text (Text)
import GHC.Generics (Generic)

parseSNSValueViaMessageType :: ByteString -> Value -> Either String SNSPayload
parseSNSValueViaMessageType messageType =
  parseEither $ withObject "SNSPayload" $ \o -> do
    SNSPayload
      <$> o
      .: "Message"
      <*> o
      .: "MessageId"
      <*> o
      .: "Timestamp"
      <*> o
      .: "TopicArn"
      <*> o
      .: "Type"
      <*> o
      .: "SignatureVersion"
      <*> o
      .: "Signature"
      <*> o
      .: "SigningCertURL"
      <*> parseType o
 where
  parseType o = case messageType of
    "SubscriptionConfirmation" ->
      SubscriptionConfirmation <$> parseJSON (Object o)
    "UnsubscribeConfirmation" ->
      UnsubscribeConfirmation <$> parseJSON (Object o)
    "Notification" -> Notification <$> parseJSON (Object o)
    msg -> fail $ "Unknown message type " <> show msg

data SNSPayload = SNSPayload
  { snsMessage :: Text
  , snsMessageId :: Text
  , snsTimestamp :: Text
  , snsTopicArn :: Text
  , snsType :: Text
  , snsSignatureVersion :: Text
  , snsSignature :: Text
  , snsSigningCertURL :: Text
  , snsTypePayload :: SNSType
  }

data SNSType
  = Notification SNSNotification
  | SubscriptionConfirmation SNSSubscription
  | UnsubscribeConfirmation SNSSubscription

newtype SNSNotification = SNSNotification
  { snsSubject :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON SNSNotification where
  parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = drop 3 }

data SNSSubscription = SNSSubscription
  { snsToken :: Text
  , snsSubscribeURL :: Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON SNSSubscription where
  parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = drop 3 }
