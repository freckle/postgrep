{-# LANGUAGE OverloadedStrings #-}

-- | Main module to parse log lines.

module PostGrep.LogLine
  ( LogEntryComponent (..)
  , LogLevel (..)
  , LogLineParser
  , logLineParser
  , parseLine
  ) where

import Data.Array (elems)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Text.Regex.PCRE

import PostGrep.LogPrefix

data LogEntryComponent
  = ApplicationName T.Text
  | UserName T.Text
  | DatabaseName T.Text
  | RemoteHost T.Text
  | RemotePort T.Text
  | ProcessID T.Text
  | Timestamp T.Text
  | CommandTag T.Text
  | SQLStateErrorCode T.Text
  | SessionID T.Text
  | LogLineNumber T.Text
  | ProcessStartTimestamp T.Text
  | VirtualTransactionID T.Text
  | TransactionID T.Text
  | LogLevel LogLevel
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
  case parseLine' t regex of
    [] -> Nothing
    [_] -> Nothing
    -- In the pcre regex lib, the first match is the entire string, so we throw
    -- it away as we only want the specific groups.
    (_:xs) -> Just $ fmap (\(c, m) -> c (TE.decodeUtf8 m)) (zip consumers xs)

parseLine' :: BS.ByteString -> Regex -> [BS.ByteString]
parseLine' t regex =
  case matches of
    [] -> []
    (x:_) -> map fst $ elems x
  where matches = matchAllText regex t

data LogLineParseComponent
  = PrefixComponent LogLinePrefixComponent
  | LogLevelComponent
  | StatementComponent

-- | A pair of a compiled regex and a list of consumers for the matched groups
-- in that regex.
data LogLineParser = LogLineParser Regex [T.Text -> LogEntryComponent]

logLineParser :: LogLinePrefix -> LogLineParser
logLineParser (LogLinePrefix prefixComponents) = LogLineParser regex consumers
  where escapeComponents = map PrefixComponent prefixComponents
        components = escapeComponents ++ [LogLevelComponent, StatementComponent]
        regex = makeRegex $ concatMap parseComponentRegex components
        consumers = concatMap parseComponentConsumer components

-- | Produces a regular expression string for the given component. This string
-- may contain zero or more groups.
parseComponentRegex :: LogLineParseComponent -> String
parseComponentRegex LogLevelComponent =
  "\\s*(LOG|WARNING|ERROR|FATAL|PANIC|DETAIL|STATEMENT|HINT|CONTEXT|LOCATION):"
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
parseComponentConsumer :: LogLineParseComponent -> [T.Text -> LogEntryComponent]
parseComponentConsumer LogLevelComponent = [LogLevel . read . T.unpack]
parseComponentConsumer StatementComponent = [Statement]
parseComponentConsumer (PrefixComponent (LogLineLiteral _)) = []
parseComponentConsumer (PrefixComponent (LogLineEscape ApplicationNameEscape)) = [ApplicationName]
parseComponentConsumer (PrefixComponent (LogLineEscape UserNameEscape)) = [UserName]
parseComponentConsumer (PrefixComponent (LogLineEscape DatabaseNameEscape)) = [DatabaseName]
parseComponentConsumer (PrefixComponent (LogLineEscape RemoteHostWithPortEscape)) = [RemoteHost, RemotePort]
parseComponentConsumer (PrefixComponent (LogLineEscape RemoteHostEscape)) = [RemoteHost]
parseComponentConsumer (PrefixComponent (LogLineEscape ProcessIDEscape)) = [ProcessID]
parseComponentConsumer (PrefixComponent (LogLineEscape TimestampWithoutMillisecondsEscape)) = [Timestamp]
parseComponentConsumer (PrefixComponent (LogLineEscape TimestampWithMillisecondsEscape)) = [Timestamp]
parseComponentConsumer (PrefixComponent (LogLineEscape CommandTagEscape)) = [CommandTag]
parseComponentConsumer (PrefixComponent (LogLineEscape SQLStateErrorCodeEscape)) = [SQLStateErrorCode]
parseComponentConsumer (PrefixComponent (LogLineEscape SessionIDEscape)) = [SessionID]
parseComponentConsumer (PrefixComponent (LogLineEscape LogLineNumberEscape)) = [LogLineNumber]
parseComponentConsumer (PrefixComponent (LogLineEscape ProcessStartTimestampEscape)) = [ProcessStartTimestamp]
parseComponentConsumer (PrefixComponent (LogLineEscape VirtualTransactionIDEscape)) = [VirtualTransactionID]
parseComponentConsumer (PrefixComponent (LogLineEscape TransactionIDEscape)) = [TransactionID]
parseComponentConsumer (PrefixComponent (LogLineEscape NonSessionStopEscape)) = []
parseComponentConsumer (PrefixComponent (LogLineEscape LiteralPercentEscape)) = []
