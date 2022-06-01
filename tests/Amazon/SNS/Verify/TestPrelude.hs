module Amazon.SNS.Verify.TestPrelude
  ( module X
  , module Amazon.SNS.Verify.TestPrelude
  ) where

import Prelude as X

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Data.Text as X (Text)
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Handler.Warp
import Test.Hspec as X

useCertServer :: IO () -> IO ()
useCertServer action = do
  ready <- newEmptyMVar
  race_ (withMVar ready $ \() -> action)
    $ runSettings
        (setBeforeMainLoop (putMVar ready ()) . setPort 3000 $ defaultSettings)
    $ \req send -> do
        if requestMethod req == "DELETE"
          then send (responseLBS status200 [] "Goodbye!")
          else do
            send $ responseFile
              ok200
              [("Content-Type", "text/plain")]
              "./tests/cert.pem"
              Nothing
