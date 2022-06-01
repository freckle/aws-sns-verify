# sns-message-verify

```hs
myEchoWebhook :: MonadHandler m => m ()
myEchoWebhook = do
  message <- verifySNSMessage =<< requireInsecureJsonBody
  logDebugN message
```

## Sign For Test

Signatures for testing are produced with the self signed certificate in this
repository.

```sh
cat unsigned.txt | openssl dgst -sha1 -sign tests/key.pem | openssl base64
```

The certificate was produced with

```sh
openssl req -newkey rsa:2048 -new -nodes -x509 -days 3650 -keyout tests/key.pem -out tests/cert.pem
```
