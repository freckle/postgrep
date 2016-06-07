{-# LANGUAGE OverloadedStrings #-}

module PostGrep.LogPrefixSpec where

import PostGrep.LogPrefix

import Test.Hspec

spec :: Spec
spec =
  describe "prefixRegexParser" $ do
    it "parses simple prefixes" $ do
      parseLogLinePrefix "%a" `shouldBe`
        Right (LogLinePrefix [LogLineEscape ApplicationNameEscape])
      parseLogLinePrefix "%a@%r" `shouldBe`
        Right (LogLinePrefix [ LogLineEscape ApplicationNameEscape
                             , LogLineLiteral "@"
                             , LogLineEscape RemoteHostWithPortEscape
                             ])
    it "rejects unknown escape sequences" $ do
      parseLogLinePrefix "%z" `shouldBe`
        Left "Failed reading: Unknown escape character 'z'"
      parseLogLinePrefix "%a[%z]" `shouldBe`
        Left "endOfInput"
