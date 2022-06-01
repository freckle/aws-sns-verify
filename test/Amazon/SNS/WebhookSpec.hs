{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Amazon.SNS.WebhookSpec
  ( spec
  ) where

import Amazon.SNS.Webhook
import Data.Aeson.QQ
import Test.Hspec

spec :: Spec
spec = do
  describe "requireSNSMessage" $ do
    it "successfully validates an SNS notification" $ do
      let
        payload = [aesonQQ|
          { Message: "Some message"
          , MessageId: "78d4d7a0-a3eb-5c4d-834f-8d5fa9813ab6"
          , Timestamp: "2022-05-18T14:52:26.952Z"
          , TopicArn: "arn:aws:sns:us-west-2:123456789012:MyTopic"
          , Type: "Notification"
          , Subject: "SynthesisTaskNotification { TaskId: 680a1f1b-f3ae-4474-aa8f-3b6dfe52e656, Status: COMPLETED }"
          , SignatureVersion: "1"
          , Signature: "Dg24trcOUiLjclt5JwyJS0JEOnEEbi6P30XS6KBxMCwzZ08a04UwjaFTW9Ae8xurhBS5YESz1fY28vTwvEmxh/20WmB3bWIDOMp9v5RI8XSZOvpMm+hdQJ43VqGhEDyAvRU6iCDLihDlZNc/sBCwl9X0H4kh/8vIElRif9gFBbYI94ZHGgqEV+Zc3gVKo9Udrl/MxNvMVadsO/+/oPVUeWibQr3xfGK95oc/ocuNAgi0MOxZmLVnibHu36KOTSvy2qSLonnRRFcbaauYZJ4js7oTq+1ujXNO72oPLaeG3pVJ2grqMc5z8tKQxFnSTE3es7wQarU/CLrbO8j0isbnWw=="
          , SigningCertURL: "https://gist.githubusercontent.com/eborden/c96c89259b6ad84bf8b6fdd325b6ee68/raw/494530cc0c8608171e7accb0a727a0b093d27a5e/cert.pem"
          }
          |]
      x <- requireSNSMessage payload

      x `shouldBe` "Some message"
