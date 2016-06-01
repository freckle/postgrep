-- | Main module to parse log entries.

module PostGrep.LogEntry
  ( LogEntryComponent (..)
  , prefixRegexParser
  , prefixRegexString
  , parsePrefix
  ) where

import Data.Array (elems)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
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
  | Message T.Text
  deriving (Show, Eq)

data PrefixRegexParser =
  PrefixRegexParser
  { prefixRegexParserRegex :: Regex
  , prefixRegexParserPrefix :: LogLinePrefix
  }

prefixRegexParser :: LogLinePrefix -> PrefixRegexParser
prefixRegexParser pre = PrefixRegexParser regex pre
  where regex = makeRegex $ prefixRegexString pre

prefixRegexString :: LogLinePrefix -> String
prefixRegexString (LogLinePrefix components) = concatMap prefixComponentRegex components

prefixComponentRegex :: LogLinePrefixComponent -> String
prefixComponentRegex (LogLineLiteral lit) = prefixLiteralRegex $ T.unpack lit
prefixComponentRegex (LogLineEscape escape) = "(" ++ prefixEscapeRegex escape ++ ")"

-- | Escapes parts of literals in the prefix so they don't confuse the regex.
prefixLiteralRegex :: String -> String
prefixLiteralRegex [] = []
prefixLiteralRegex ('[':xs) = '\\' : '[' : prefixLiteralRegex xs
prefixLiteralRegex (']':xs) = '\\' : ']' : prefixLiteralRegex xs
prefixLiteralRegex (')':xs) = '\\' : ')' : prefixLiteralRegex xs
prefixLiteralRegex ('(':xs) = '\\' : '(' : prefixLiteralRegex xs
prefixLiteralRegex ('|':xs) = '\\' : '|' : prefixLiteralRegex xs
prefixLiteralRegex (x:xs) = x : prefixLiteralRegex xs

prefixEscapeRegex :: LogLinePrefixEscape -> String
prefixEscapeRegex ApplicationNameEscape = ".*"
prefixEscapeRegex UserNameEscape = "[0-9a-zA-Z\\_\\[\\]\\-\\.]*"
prefixEscapeRegex DatabaseNameEscape = "[0-9a-zA-Z\\_\\[\\]\\-\\.]*"
prefixEscapeRegex RemoteHostWithPortEscape = prefixEscapeRegex RemoteHostEscape ++ "[\\(\\d\\)]*"
prefixEscapeRegex RemoteHostEscape =
  "(?:[a-zA-Z0-9\\-\\.]+|\\[local\\]|\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}|[0-9a-fA-F:]+)?"
prefixEscapeRegex ProcessIDEscape = "\\d+"
prefixEscapeRegex TimestampWithoutMillisecondsEscape =
  "(?:\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2})(?: [A-Z\\+\\-\\d]{3,6})?"
prefixEscapeRegex TimestampWithMillisecondsEscape =
  "(?:\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2})\\.\\d+(?: [A-Z\\+\\-\\d]{3,6})?"
prefixEscapeRegex CommandTagEscape = "[0-9a-zA-Z\\.\\-\\_]*"
prefixEscapeRegex SQLStateErrorCodeEscape = "[0-9a-zA-Z]+"
prefixEscapeRegex SessionIDEscape = "[0-9a-f\\.]*"
prefixEscapeRegex LogLineNumberEscape = "\\d+"
prefixEscapeRegex ProcessStartTimestampEscape =
  "(?:\\d{4}-\\d{2}-\\d{2} \\d{2}):\\d{2}:\\d{2}(?: [A-Z\\d]{3,6})?"
prefixEscapeRegex VirtualTransactionIDEscape = "[0-9a-f\\.\\/]*"
prefixEscapeRegex TransactionIDEscape = "[0-9a-f\\.\\/]*"
prefixEscapeRegex NonSessionStopEscape = ""
prefixEscapeRegex LiteralPercentEscape = "%"

parsePrefix :: PrefixRegexParser -> T.Text -> Maybe [LogEntryComponent]
parsePrefix parser t =
  case parsePrefix' t (prefixRegexParserRegex parser) of
    [] -> Nothing
    [_] -> Nothing
    -- In the pcre regex lib, the first match is the entire string, so we throw
    -- it away as we only want the specific groups.
    (_:xs) -> Just $ parsedComponents (prefixRegexParserPrefix parser) xs

parsePrefix' :: T.Text -> Regex -> [String]
parsePrefix' t regex =
  case matches of
    [] -> []
    (x:_) -> map fst $ elems x
  where matches = matchAllText regex (T.unpack t)

-- | Transforms the list of matches to log entry components by pairing matches
-- with prefix strings.
parsedComponents :: LogLinePrefix -> [String] -> [LogEntryComponent]
parsedComponents (LogLinePrefix prefixComponents) matches =
  concatMap (uncurry matchToComponent) (zip escapeComponents matches)
  where escapeComponents = catMaybes $ fmap getEscape prefixComponents

matchToComponent :: LogLinePrefixEscape -> String -> [LogEntryComponent]
matchToComponent ApplicationNameEscape m = [ApplicationName $ T.pack m]
matchToComponent UserNameEscape m = [UserName $ T.pack m]
matchToComponent DatabaseNameEscape m = [DatabaseName $ T.pack m]
matchToComponent RemoteHostWithPortEscape m = [RemoteHost $ T.pack m]
matchToComponent RemoteHostEscape m = [RemoteHost $ T.pack m]
matchToComponent ProcessIDEscape m = [ProcessID $ T.pack m]
matchToComponent TimestampWithoutMillisecondsEscape m = [Timestamp $ T.pack m]
matchToComponent TimestampWithMillisecondsEscape m = [Timestamp $ T.pack m]
matchToComponent CommandTagEscape m = [CommandTag $ T.pack m]
matchToComponent SQLStateErrorCodeEscape m = [SQLStateErrorCode $ T.pack m]
matchToComponent SessionIDEscape m = [SessionID $ T.pack m]
matchToComponent LogLineNumberEscape m = [LogLineNumber $ T.pack m]
matchToComponent ProcessStartTimestampEscape m = [ProcessStartTimestamp $ T.pack m]
matchToComponent VirtualTransactionIDEscape m = [VirtualTransactionID $ T.pack m]
matchToComponent TransactionIDEscape m = [TransactionID $ T.pack m]
matchToComponent NonSessionStopEscape _ = []
matchToComponent LiteralPercentEscape _ = []
