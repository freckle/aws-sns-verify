{-# LANGUAGE CPP #-}

module Amazon.SNS.Verify.ValidURI
  ( validScheme
  , validRegPattern
  , devRegPattern
  , prodRegPattern
  ) where

import Amazon.SNS.Verify.Prelude

validScheme :: String
validScheme =
#ifdef DEVELOPMENT
  _devSchema
#else
  _prodSchema
#endif

_devSchema :: String
_devSchema = "http:"

_prodSchema :: String
_prodSchema = "https:"

validRegPattern :: String
validRegPattern =
#ifdef DEVELOPMENT
  devRegPattern
#else
  prodRegPattern
#endif

devRegPattern :: String
devRegPattern = "^localhost$"

prodRegPattern :: String
prodRegPattern = "^sns\\.[a-zA-Z0-9\\-]{3,}\\.amazonaws\\.com(\\.cn)?$"
