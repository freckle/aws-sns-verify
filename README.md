# sns-message-verify

## Sign For Test

Signatures for testing are produced with the self signed certificate in this
repository.

```sh
cat unsigned.txt | openssl dgst -sha1 -sign key.pem | openssl base64
```

The certificate was produced with

```sh
openssl req -newkey rsa:2048 -new -nodes -x509 -days 3650 -keyout key.pem -out cert.pem
```
