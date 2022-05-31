{-# LANGUAGE OverloadedStrings #-}

module Amazon.SNS.Webhook.ValidateSpec
  ( spec
  ) where

import Amazon.SNS.Webhook.Payload
import Amazon.SNS.Webhook.Validate
import Data.Text (Text)
import Test.Hspec

spec :: Spec
spec = do
  describe "validateSnsMessage" $ do
    it "successfully validates an SNS notification" $ do
      let message = "Some message"
      x <- validateSnsMessage $ SNSPayload
        { snsMessage = message
        , snsMessageId = "78d4d7a0-a3eb-5c4d-834f-8d5fa9813ab6"
        , snsTimestamp = "2022-05-18T14:52:26.952Z"
        , snsTopicArn = "arn:aws:sns:us-west-2:123456789012:MyTopic"
        , snsType = "Notification"
        , snsSignatureVersion = "1"
        , snsSignature =
          "Dg24trcOUiLjclt5JwyJS0JEOnEEbi6P30XS6KBxMCwzZ08a04UwjaFTW9Ae8xurhBS5YESz1fY28vTwvEmxh/20WmB3bWIDOMp9v5RI8XSZOvpMm+hdQJ43VqGhEDyAvRU6iCDLihDlZNc/sBCwl9X0H4kh/8vIElRif9gFBbYI94ZHGgqEV+Zc3gVKo9Udrl/MxNvMVadsO/+/oPVUeWibQr3xfGK95oc/ocuNAgi0MOxZmLVnibHu36KOTSvy2qSLonnRRFcbaauYZJ4js7oTq+1ujXNO72oPLaeG3pVJ2grqMc5z8tKQxFnSTE3es7wQarU/CLrbO8j0isbnWw=="
        , snsSigningCertURL = cert
        , snsTypePayload = Notification $ SNSNotification
          { snsSubject =
            Just
              "SynthesisTaskNotification { TaskId: 680a1f1b-f3ae-4474-aa8f-3b6dfe52e656, Status: COMPLETED }"
          }
        }
      x `shouldBe` SNSMessage message

    it "fails to validate a currupt SNS notification" $ do
      let
        go = validateSnsMessage $ SNSPayload
          { snsMessage = "Some message"
          , snsMessageId = "corrupt"
          , snsTimestamp = "2022-05-18T14:52:26.952Z"
          , snsTopicArn = "arn:aws:sns:us-west-2:123456789012:MyTopic"
          , snsType = "Notification"
          , snsSignatureVersion = "1"
          , snsSignature =
            "Dg24trcOUiLjclt5JwyJS0JEOnEEbi6P30XS6KBxMCwzZ08a04UwjaFTW9Ae8xurhBS5YESz1fY28vTwvEmxh/20WmB3bWIDOMp9v5RI8XSZOvpMm+hdQJ43VqGhEDyAvRU6iCDLihDlZNc/sBCwl9X0H4kh/8vIElRif9gFBbYI94ZHGgqEV+Zc3gVKo9Udrl/MxNvMVadsO/+/oPVUeWibQr3xfGK95oc/ocuNAgi0MOxZmLVnibHu36KOTSvy2qSLonnRRFcbaauYZJ4js7oTq+1ujXNO72oPLaeG3pVJ2grqMc5z8tKQxFnSTE3es7wQarU/CLrbO8j0isbnWw=="
          , snsSigningCertURL = cert
          , snsTypePayload = Notification $ SNSNotification
            { snsSubject =
              Just
                "SynthesisTaskNotification { TaskId: 680a1f1b-f3ae-4474-aa8f-3b6dfe52e656, Status: COMPLETED }"
            }
          }
      go `shouldThrow` anyException

    it "successfully validates an SNS subscription" $ do
      -- pendingWith "need valid subscription payload"
      let
        message
          = "You have chosen to subscribe to the topic arn:aws:sns:us-west-2:123456789012:MyTopic.\nTo confirm the subscription, visit the SubscribeURL included in this message."
        subscription = SNSSubscription
          { snsToken = "2336412f37..."
          , snsSubscribeURL =
            "https://sns.us-west-2.amazonaws.com/?Action=ConfirmSubscription&TopicArn=arn:aws:sns:us-west-2:123456789012:MyTopic&Token=2336412f37..."
          }
      x <- validateSnsMessage $ SNSPayload
        { snsMessage = message
        , snsMessageId = "165545c9-2a5c-472c-8df2-7ff2be2b3b1b"
        , snsTimestamp = "2012-04-26T20:45:04.751Z"
        , snsTopicArn = "arn:aws:sns:us-west-2:123456789012:MyTopic"
        , snsType = "SubscriptionConfirmation"
        , snsSignatureVersion = "1"
        , snsSignature =
          "ZsPD3cDAHPU0mseSCKSQX8Ka2vT8lmaTHN6GbfCwmtfGFblWveBDUMWbSvhvDL4A6RujW8UfEpCytC2vwbMM2GUreTRpelK06t19nxeTHAnC5+JYBUpmlgauT92g3sLGYDWIUxAbD5u7ZY5QDfWaOpo8OD+xFy0sAB51SSDjTsXbt+gh/S6FbZQ1l/sjyBlDeJwxM/qpLn4xfQKO/Ev6lpV3ioBT8/Q7f1o4rzloYXxu1QnpDUx6L5uZB23lGEI4dcjIixi58WFwkz0SHgR9MWYIKJa2vZO2P8jFG63FGOhNaHbp3e/J6Tl6XnJnc+vriGle2dwhQjuWMDSdi92+kg=="
        , snsSigningCertURL = cert
        , snsTypePayload = SubscriptionConfirmation subscription
        }
      x `shouldBe` SNSSubscribe subscription

    it "successfully validates an SNS unsubscribe" $ do
      -- pendingWith "need valid subscription payload"
      let
        message
          = "You have chosen to subscribe to the topic arn:aws:sns:us-west-2:123456789012:MyTopic.\nTo confirm the subscription, visit the SubscribeURL included in this message."
        subscription = SNSSubscription
          { snsToken = "2336412f37..."
          , snsSubscribeURL =
            "https://sns.us-west-2.amazonaws.com/?Action=ConfirmSubscription&TopicArn=arn:aws:sns:us-west-2:123456789012:MyTopic&Token=2336412f37..."
          }
      x <- validateSnsMessage $ SNSPayload
        { snsMessage = message
        , snsMessageId = "165545c9-2a5c-472c-8df2-7ff2be2b3b1b"
        , snsTimestamp = "2012-04-26T20:45:04.751Z"
        , snsTopicArn = "arn:aws:sns:us-west-2:123456789012:MyTopic"
        , snsType = "UnsubscribeConfirmation"
        , snsSignatureVersion = "1"
        , snsSignature =
          "DqsnMTKKAW65yGEjqGGmHzw660tq9lB0yK3GVznVCINDnzrrq+mvScrHSJQ17+tT+u1aSL8nFvyzC4xdn4j3b1TNSXJdqj1ivvMVpVDbvl8Wvf/+JQ/ac6MPdQMkq0RitX5pzod3bIekY9ZXejLoEP73Q5zcLbTGRnC7G4li5W5ZG9i81zfbM1PgqmkUQnPC49Cqm/KMvBok8PfVBzWfEFLcnSsbm7JJf6y5Y7j+onDLTlbjwdRhej+MUJrrtsXbkARUg6EZAFcbhULj/3EYhvwdDClL4h0OxxhwWRnofTONIZhLB7bAkrbgsikkyhZ49zG8PjD6+TLX3YPaNB5eFw=="
        , snsSigningCertURL = cert
        , snsTypePayload = UnsubscribeConfirmation subscription
        }
      x `shouldBe` SNSUnsubscribe subscription


cert :: Text
cert =
  "https://gist.githubusercontent.com/eborden/c96c89259b6ad84bf8b6fdd325b6ee68/raw/494530cc0c8608171e7accb0a727a0b093d27a5e/cert.pem"
