{-# LANGUAGE OverloadedStrings #-}

module PostGrep.ParseTimeSpec where

import Data.Attoparsec.ByteString
import qualified Data.ByteString as BS
import Data.Thyme
import System.Locale (defaultTimeLocale)

import PostGrep.ParseTime

import Test.Hspec

spec :: Spec
spec =
  describe "parsePrefix" $ do
    it "parses times without milliseconds" $ do
      checkParseTime "%F %T" "2016-04-01 13:42:24 UTC"
      checkParseTime "%F %T" "2016-12-30 01:00:35 UTC"
      checkParseTime "%F %T" "2016-12-30 01:00:35.00 UTC"
      checkParseTime "%F %T" "0001-02-03 01:00:35 UTC"

    it "parses times with milliseconds" $ do
      checkParseTime "%F %T%Q" "2016-04-01 13:42:24.123 UTC"
      checkParseTime "%F %T%Q" "2016-12-30 01:00:35.0345 UTC"
      checkParseTime "%F %T%Q" "2016-12-30 01:01:35.000340 UTC"


checkParseTime :: String -> BS.ByteString -> Expectation
checkParseTime formatString timeString =
  thymeParser timeString `shouldBe` parseTimeStamp timeString
  where thymeParser = fmap buildTime . maybeResult . parse (timeParser defaultTimeLocale formatString)
