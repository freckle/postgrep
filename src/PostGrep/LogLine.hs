{-# LANGUAGE OverloadedStrings #-}

-- | Main module to parse log lines.

module PostGrep.LogLine
  ( LogEntryComponent (..)
  , LogLevel (..)
  , LogLineParser
  , logLineParser
  , parseLine
  ) where

import Data.Attoparsec.ByteString
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Thyme
import System.Locale (defaultTimeLocale)
import Text.Read (readMaybe)
import qualified Text.Regex.PCRE.Light as PCRE

import PostGrep.LogPrefix

data LogEntryComponent
  = ApplicationName T.Text
  | UserName T.Text
  | DatabaseName T.Text
  | RemoteHost T.Text
  | RemotePort Int
  | ProcessID Int
  | Timestamp UTCTime
  | CommandTag T.Text
  | SQLStateErrorCode T.Text
  | SessionID T.Text
  | LogLineNumber Integer
  | ProcessStartTimestamp T.Text
  | VirtualTransactionID T.Text
  | TransactionID T.Text
  | LogLevel LogLevel
  | DurationMilliseconds Double
  | Statement T.Text
  deriving (Show, Eq)

data LogLevel
  = LOG
  | WARNING
  | ERROR
  | FATAL
  | PANIC
  | DETAIL
  | STATEMENT
  | HINT
  | CONTEXT
  | LOCATION
  deriving (Show, Eq, Read)


parseLine :: LogLineParser -> BS.ByteString -> Maybe [LogEntryComponent]
parseLine (LogLineParser regex consumers) t =
  case PCRE.match regex t [] of
    Nothing -> Nothing
    Just [] -> Nothing
    -- In libpcre, the first match is the entire string, so we throw it away as
    -- we only want the captured groups.
    Just (_:xs) -> Just $ catMaybes $ fmap (\(c, m) -> c m) (zip consumers xs)

data LogLineParseComponent
  = PrefixComponent LogLinePrefixComponent
  | LogLevelComponent
  | DurationComponent
  | StatementComponent

-- | A pair of a compiled regex and a list of consumers for the matched groups
-- in that regex.
data LogLineParser = LogLineParser PCRE.Regex [BS.ByteString -> Maybe LogEntryComponent]

logLineParser :: LogLinePrefix -> LogLineParser
logLineParser (LogLinePrefix prefixComponents) = LogLineParser regex consumers
  where escapeComponents = map PrefixComponent prefixComponents
        components = escapeComponents ++ [LogLevelComponent, DurationComponent, StatementComponent]
        regex = PCRE.compile (BSC.pack $ concatMap parseComponentRegex components) []
        consumers = concatMap parseComponentConsumer components

-- | Produces a regular expression string for the given component. This string
-- may contain zero or more groups.
parseComponentRegex :: LogLineParseComponent -> String
parseComponentRegex LogLevelComponent =
  "\\s*(LOG|WARNING|ERROR|FATAL|PANIC|DETAIL|STATEMENT|HINT|CONTEXT|LOCATION):"
parseComponentRegex DurationComponent =
  "\\s*(?:duration: )?([\\d\\.]+)?(?: ms)?"
parseComponentRegex StatementComponent =
  "\\s*(.*)"
parseComponentRegex (PrefixComponent (LogLineLiteral lit)) =
  prefixLiteralRegex $ T.unpack lit
parseComponentRegex (PrefixComponent (LogLineEscape ApplicationNameEscape)) =
  "(.*)"
parseComponentRegex (PrefixComponent (LogLineEscape UserNameEscape)) =
  "([0-9a-zA-Z\\_\\[\\]\\-\\.]*)"
parseComponentRegex (PrefixComponent (LogLineEscape DatabaseNameEscape)) =
  "([0-9a-zA-Z\\_\\[\\]\\-\\.]*)"
parseComponentRegex (PrefixComponent (LogLineEscape RemoteHostWithPortEscape)) =
  parseComponentRegex (PrefixComponent (LogLineEscape RemoteHostEscape)) ++ "\\((\\d*)\\)"
parseComponentRegex (PrefixComponent (LogLineEscape RemoteHostEscape)) =
  "([a-zA-Z0-9\\-\\.]+|\\[local\\]|\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}|[0-9a-fA-F:]+)?"
parseComponentRegex (PrefixComponent (LogLineEscape ProcessIDEscape)) =
  "(\\d+)"
parseComponentRegex (PrefixComponent (LogLineEscape TimestampWithoutMillisecondsEscape)) =
  "(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2})(?: [A-Z\\+\\-\\d]{3,6})?"
parseComponentRegex (PrefixComponent (LogLineEscape TimestampWithMillisecondsEscape)) =
  "(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2})\\.\\d+(?: [A-Z\\+\\-\\d]{3,6})?"
parseComponentRegex (PrefixComponent (LogLineEscape CommandTagEscape)) =
  "([0-9a-zA-Z\\.\\-\\_]*)"
parseComponentRegex (PrefixComponent (LogLineEscape SQLStateErrorCodeEscape)) =
  "([0-9a-zA-Z]+)"
parseComponentRegex (PrefixComponent (LogLineEscape SessionIDEscape)) =
  "([0-9a-f\\.]*)"
parseComponentRegex (PrefixComponent (LogLineEscape LogLineNumberEscape)) =
  "(\\d+)"
parseComponentRegex (PrefixComponent (LogLineEscape ProcessStartTimestampEscape)) =
  "(\\d{4}-\\d{2}-\\d{2} \\d{2}):\\d{2}:\\d{2}(?: [A-Z\\d]{3,6})?"
parseComponentRegex (PrefixComponent (LogLineEscape VirtualTransactionIDEscape)) =
  "([0-9a-f\\.\\/]*)"
parseComponentRegex (PrefixComponent (LogLineEscape TransactionIDEscape)) =
  "([0-9a-f\\.\\/]*)"
parseComponentRegex (PrefixComponent (LogLineEscape NonSessionStopEscape)) =
  ""
parseComponentRegex (PrefixComponent (LogLineEscape LiteralPercentEscape)) =
  "%"

-- | Escapes parts of literals in the prefix so they don't confuse the regex.
prefixLiteralRegex :: String -> String
prefixLiteralRegex [] = []
prefixLiteralRegex ('[':xs) = '\\' : '[' : prefixLiteralRegex xs
prefixLiteralRegex (']':xs) = '\\' : ']' : prefixLiteralRegex xs
prefixLiteralRegex (')':xs) = '\\' : ')' : prefixLiteralRegex xs
prefixLiteralRegex ('(':xs) = '\\' : '(' : prefixLiteralRegex xs
prefixLiteralRegex ('|':xs) = '\\' : '|' : prefixLiteralRegex xs
prefixLiteralRegex (x:xs) = x : prefixLiteralRegex xs

-- | This function gives zero or more group consumers for matched groups. This
-- function is very tightly coupled with 'parseComponentRegex'; the number of
-- groups in the regex must match the number of consumers here.
parseComponentConsumer :: LogLineParseComponent -> [BS.ByteString -> Maybe LogEntryComponent]
parseComponentConsumer LogLevelComponent = [readComponentMaybe LogLevel]
parseComponentConsumer DurationComponent = [readComponentMaybe DurationMilliseconds]
parseComponentConsumer StatementComponent = [Just . Statement . TE.decodeUtf8]
parseComponentConsumer (PrefixComponent (LogLineLiteral _)) = []
parseComponentConsumer (PrefixComponent (LogLineEscape ApplicationNameEscape)) = [Just . ApplicationName . TE.decodeUtf8]
parseComponentConsumer (PrefixComponent (LogLineEscape UserNameEscape)) = [Just . UserName . TE.decodeUtf8]
parseComponentConsumer (PrefixComponent (LogLineEscape DatabaseNameEscape)) = [Just . DatabaseName . TE.decodeUtf8]
parseComponentConsumer (PrefixComponent (LogLineEscape RemoteHostWithPortEscape)) = [Just . RemoteHost . TE.decodeUtf8, readComponentMaybe RemotePort]
parseComponentConsumer (PrefixComponent (LogLineEscape RemoteHostEscape)) = [Just . RemoteHost . TE.decodeUtf8]
parseComponentConsumer (PrefixComponent (LogLineEscape ProcessIDEscape)) = [readComponentMaybe ProcessID]
parseComponentConsumer (PrefixComponent (LogLineEscape TimestampWithoutMillisecondsEscape)) = [fmap Timestamp . parseUTCTime "%F %T"]
parseComponentConsumer (PrefixComponent (LogLineEscape TimestampWithMillisecondsEscape)) = [fmap Timestamp . parseUTCTime "%F %T%Q"]
parseComponentConsumer (PrefixComponent (LogLineEscape CommandTagEscape)) = [Just . CommandTag . TE.decodeUtf8]
parseComponentConsumer (PrefixComponent (LogLineEscape SQLStateErrorCodeEscape)) = [Just . SQLStateErrorCode . TE.decodeUtf8]
parseComponentConsumer (PrefixComponent (LogLineEscape SessionIDEscape)) = [Just . SessionID . TE.decodeUtf8]
parseComponentConsumer (PrefixComponent (LogLineEscape LogLineNumberEscape)) = [readComponentMaybe LogLineNumber]
parseComponentConsumer (PrefixComponent (LogLineEscape ProcessStartTimestampEscape)) = [Just . ProcessStartTimestamp . TE.decodeUtf8]
parseComponentConsumer (PrefixComponent (LogLineEscape VirtualTransactionIDEscape)) = [Just . VirtualTransactionID . TE.decodeUtf8]
parseComponentConsumer (PrefixComponent (LogLineEscape TransactionIDEscape)) = [Just . TransactionID . TE.decodeUtf8]
parseComponentConsumer (PrefixComponent (LogLineEscape NonSessionStopEscape)) = []
parseComponentConsumer (PrefixComponent (LogLineEscape LiteralPercentEscape)) = []


parseUTCTime :: String -> BS.ByteString -> Maybe UTCTime
parseUTCTime format timeString = buildTime <$> maybeResult parsed
  where parser = timeParser defaultTimeLocale format
        parsed = parse parser timeString


readComponentMaybe :: (Read a) => (a -> LogEntryComponent) -> BS.ByteString -> Maybe LogEntryComponent
readComponentMaybe constructor = fmap constructor . readMaybe . T.unpack . TE.decodeUtf8
