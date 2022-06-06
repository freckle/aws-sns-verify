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
  _devScheme
#else
  _prodScheme
#endif

_devScheme :: String
_devScheme = "http:"

_prodScheme :: String
_prodScheme = "https:"

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
