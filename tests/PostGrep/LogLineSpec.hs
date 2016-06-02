{-# LANGUAGE OverloadedStrings #-}

module PostGrep.LogLineSpec where

import PostGrep.LogLine
import PostGrep.LogPrefix

import Test.Hspec

spec :: Spec
spec =
  describe "parsePrefix" $ do
    it "parses complex prefixes" $ do
      let (Right prefix) = parseLogLinePrefix "%a[%d]@%r:%i%q:"
          parser = logLineParser prefix
      parseLine parser "myapp[my_db]@10.0.120.51(52094):SELECT:LOG: duration: 0.019 ms statement: Hello" `shouldBe`
        Just [ ApplicationName "myapp"
             , DatabaseName "my_db"
             , RemoteHost "10.0.120.51"
             , RemotePort "52094"
             , CommandTag "SELECT"
             , LogLevel LOG
             , Duration "0.019 ms"
             , Statement "statement: Hello"
             ]
    it "parses prefixes with special chars in variables" $ do
      let (Right prefix) = parseLogLinePrefix "%a[%d]"
          parser = logLineParser prefix
      parseLine parser "stupid|[app_name][stupid-db.name-]LOG:" `shouldBe`
        Just [ ApplicationName "stupid|[app_name]"
             , DatabaseName "stupid-db.name-"
             , LogLevel LOG
             , Duration ""
             , Statement ""
             ]
