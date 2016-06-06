{-# LANGUAGE OverloadedStrings #-}

module PostGrep.LogLineSpec where

import Data.Thyme.Time.Core

import PostGrep.LogLine
import PostGrep.LogPrefix

import Test.Hspec

spec :: Spec
spec =
  describe "parsePrefix" $ do
    it "parses complex prefixes" $ do
      let (Right prefix) = parseLogLinePrefix "%t:%a[%d]@%r:%i%q:"
          parser = logLineParser prefix Stderr
          (Just expectedDay) = gregorianValid (YearMonthDay 2016 6 1)
          expectedTime = mkUTCTime expectedDay (secondsToDiffTime $ 60 * 60 * 18 + 62)
      parseLine parser "2016-06-01 18:01:02 UTC:myapp[my_db]@10.0.120.51(52094):SELECT:LOG: duration: 0.019 ms statement: Hello" `shouldBe`
        Just [ Timestamp expectedTime
             , ApplicationName "myapp"
             , DatabaseName "my_db"
             , RemoteHost "10.0.120.51"
             , RemotePort 52094
             , CommandTag "SELECT"
             , LogLevel LOG
             , DurationMilliseconds 0.019
             , Statement "statement: Hello"
             ]
    it "parses prefixes with special chars in variables" $ do
      let (Right prefix) = parseLogLinePrefix "%a[%d]"
          parser = logLineParser prefix Stderr
      parseLine parser "stupid|[app_name][stupid-db.name-]LOG:" `shouldBe`
        Just [ ApplicationName "stupid|[app_name]"
             , DatabaseName "stupid-db.name-"
             , LogLevel LOG
             , Statement ""
             ]
    it "parses syslog lines" $ do
      let (Right prefix) = parseLogLinePrefix "%a[%d]:"
          parser = logLineParser prefix Syslog
          (Just expectedDay) = gregorianValid (YearMonthDay 2016 6 21)
          expectedTime = mkUTCTime expectedDay (secondsToDiffTime 60)
          line = "Jun 21 00:01:00 vagrant postgres[3341]: [3-1] myapp[mydb]:LOG:  duration: 5.098 ms  statement: SELECT * FROM pg_stat_activity;"
      parseLine parser line `shouldBe`
        Just [ Timestamp expectedTime
             , RemoteHost "vagrant"
             , UserName "postgres"
             , ProcessID 3341
             , LogLineNumber 3
             , ApplicationName "myapp"
             , DatabaseName "mydb"
             , LogLevel LOG
             , DurationMilliseconds 5.098
             , Statement "statement: SELECT * FROM pg_stat_activity;"
             ]

    it "Returns Nothing when regex parse fails" $ do
      let (Right prefix) = parseLogLinePrefix "%a[%d]"
      parseLine (logLineParser prefix Stderr) "this is dumbLOG:" `shouldBe` Nothing

    it "Returns Nothing when component timestamp is malformed" $ do
      let (Right prefix) = parseLogLinePrefix "%t:"
      parseLine (logLineParser prefix Stderr) "2016-bad-time 00:00:00 UTC:LOG:" `shouldBe` Nothing
