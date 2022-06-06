{-# LANGUAGE NoOverloadedStrings #-}

module Amazon.SNS.Verify.ValidURISpec
  ( spec
  ) where

import Amazon.SNS.Verify.TestPrelude

import Amazon.SNS.Verify.ValidURI
import Text.Regex.TDFA ((=~))

spec :: Spec
spec = around_ useCertServer $ do
  describe "verifySNSMessage" $ do
    it "validates a prod schema" $ do
      "sns.us-east-2.amazonaws.com" =~ prodRegPattern `shouldBe` True

    it "validates a prod schema" $ do
      "sns.us-west-1b.amazonaws.com" =~ prodRegPattern `shouldBe` True
