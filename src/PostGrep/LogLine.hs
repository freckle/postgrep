-- | Main module to parse log lines.

module PostGrep.LogLine
  ( LogEntryComponent (..)
  , LogLevel (..)
  , LogLineParser
  , logLineParser
  , parseLine
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

data LogLineParseComponent
  = PrefixEscape LogLinePrefixEscape
  | LogLevelComponent
  | StatementComponent

data LogLineParser = LogLineParser [LogLineParseComponent] Regex

logLineParser :: LogLinePrefix -> LogLineParser
logLineParser prefix@(LogLinePrefix prefixComponents) = LogLineParser components regex
  where escapeComponents = map PrefixEscape $ catMaybes $ fmap getEscape prefixComponents
        components = escapeComponents ++ [LogLevelComponent, StatementComponent]
        regex = makeRegex $ prefixRegexString prefix ++ logLevelRegex ++ statementRegex

logLevelRegex :: String
logLevelRegex = "\\s*(LOG|WARNING|ERROR|FATAL|PANIC|DETAIL|STATEMENT|HINT|CONTEXT|LOCATION)"

statementRegex :: String
statementRegex = "\\s*(.*)"

parseLine :: LogLineParser -> T.Text -> Maybe [LogEntryComponent]
parseLine (LogLineParser components regex) t =
  case parseLine' t regex of
    [] -> Nothing
    [_] -> Nothing
    -- In the pcre regex lib, the first match is the entire string, so we throw
    -- it away as we only want the specific groups.
    (_:xs) -> Just $ parsedComponents components xs

parseLine' :: T.Text -> Regex -> [String]
parseLine' t regex =
  case matches of
    [] -> []
    (x:_) -> map fst $ elems x
  where matches = matchAllText regex (T.unpack t)

-- | Transforms the list of matches to log entry components by pairing matches
-- with prefix strings.
parsedComponents :: [LogLineParseComponent] -> [String] -> [LogEntryComponent]
parsedComponents components matches =
  concatMap (uncurry matchToComponent) (zip components matches)

matchToComponent :: LogLineParseComponent -> String -> [LogEntryComponent]
matchToComponent (PrefixEscape esc) m = matchEscapeToComponent esc m
matchToComponent LogLevelComponent m = [LogLevel (read m)]
matchToComponent StatementComponent m = [Statement (T.pack m)]

matchEscapeToComponent :: LogLinePrefixEscape -> String -> [LogEntryComponent]
matchEscapeToComponent ApplicationNameEscape m = [ApplicationName $ T.pack m]
matchEscapeToComponent UserNameEscape m = [UserName $ T.pack m]
matchEscapeToComponent DatabaseNameEscape m = [DatabaseName $ T.pack m]
matchEscapeToComponent RemoteHostWithPortEscape m = [RemoteHost $ T.pack m]
matchEscapeToComponent RemoteHostEscape m = [RemoteHost $ T.pack m]
matchEscapeToComponent ProcessIDEscape m = [ProcessID $ T.pack m]
matchEscapeToComponent TimestampWithoutMillisecondsEscape m = [Timestamp $ T.pack m]
matchEscapeToComponent TimestampWithMillisecondsEscape m = [Timestamp $ T.pack m]
matchEscapeToComponent CommandTagEscape m = [CommandTag $ T.pack m]
matchEscapeToComponent SQLStateErrorCodeEscape m = [SQLStateErrorCode $ T.pack m]
matchEscapeToComponent SessionIDEscape m = [SessionID $ T.pack m]
matchEscapeToComponent LogLineNumberEscape m = [LogLineNumber $ T.pack m]
matchEscapeToComponent ProcessStartTimestampEscape m = [ProcessStartTimestamp $ T.pack m]
matchEscapeToComponent VirtualTransactionIDEscape m = [VirtualTransactionID $ T.pack m]
matchEscapeToComponent TransactionIDEscape m = [TransactionID $ T.pack m]
matchEscapeToComponent NonSessionStopEscape _ = []
matchEscapeToComponent LiteralPercentEscape _ = []
