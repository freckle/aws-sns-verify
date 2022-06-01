module Amazon.SNS.Verify.TestPrelude
  ( module X
  , module Amazon.SNS.Verify.TestPrelude
  ) where

import Prelude as X

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Monad (void)
import Data.Text as X (Text)
import Network.HTTP.Simple (httpLbs, parseRequest_)
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Handler.Warp
import Test.Hspec as X

initCertServer :: IO ()
initCertServer = do
  toDie <- newEmptyMVar
  void $ async $ race_ (takeMVar toDie) $ run 3000 $ \req send ->
    if requestMethod req == "DELETE"
      then putMVar toDie () *> send (responseLBS status200 [] "Goodbye!")
      else do
        send $ responseFile
          ok200
          [("Content-Type", "text/plain")]
          "./tests/cert.pem"
          Nothing

killCertServer :: IO ()
killCertServer = do
  void $ httpLbs $ parseRequest_ "DELETE http://localhost:3000"
