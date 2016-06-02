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
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Text.Regex.PCRE

import PostGrep.LogPrefix

data LogEntryComponent
  = ApplicationName BS.ByteString
  | UserName BS.ByteString
  | DatabaseName BS.ByteString
  | RemoteHost BS.ByteString
  | RemotePort BS.ByteString
  | ProcessID BS.ByteString
  | Timestamp BS.ByteString
  | CommandTag BS.ByteString
  | SQLStateErrorCode BS.ByteString
  | SessionID BS.ByteString
  | LogLineNumber BS.ByteString
  | ProcessStartTimestamp BS.ByteString
  | VirtualTransactionID BS.ByteString
  | TransactionID BS.ByteString
  | LogLevel LogLevel
  | Statement BS.ByteString
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
        regex = makeRegex $ prefixRegexString prefix <> logLevelRegex <> statementRegex

logLevelRegex :: BS.ByteString
logLevelRegex = "\\s*(LOG|WARNING|ERROR|FATAL|PANIC|DETAIL|STATEMENT|HINT|CONTEXT|LOCATION):"

statementRegex :: BS.ByteString
statementRegex = "\\s*(.*)"

parseLine :: LogLineParser -> BS.ByteString -> Maybe [LogEntryComponent]
parseLine (LogLineParser components regex) t =
  case parseLine' t regex of
    [] -> Nothing
    [_] -> Nothing
    -- In the pcre regex lib, the first match is the entire string, so we throw
    -- it away as we only want the specific groups.
    (_:xs) -> Just $ parsedComponents components xs

parseLine' :: BS.ByteString -> Regex -> [BS.ByteString]
parseLine' t regex =
  case matches of
    [] -> []
    (x:_) -> map fst $ elems x
  where matches = matchAllText regex t

-- | Transforms the list of matches to log entry components by pairing matches
-- with prefix strings.
parsedComponents :: [LogLineParseComponent] -> [BS.ByteString] -> [LogEntryComponent]
parsedComponents components matches =
  concatMap (uncurry matchToComponent) (zip components matches)

matchToComponent :: LogLineParseComponent -> BS.ByteString -> [LogEntryComponent]
matchToComponent (PrefixEscape esc) m = matchEscapeToComponent esc m
matchToComponent LogLevelComponent m = [LogLevel (read $ BS.unpack m)]
matchToComponent StatementComponent m = [Statement m]

matchEscapeToComponent :: LogLinePrefixEscape -> BS.ByteString -> [LogEntryComponent]
matchEscapeToComponent ApplicationNameEscape m = [ApplicationName m]
matchEscapeToComponent UserNameEscape m = [UserName m]
matchEscapeToComponent DatabaseNameEscape m = [DatabaseName m]
matchEscapeToComponent RemoteHostWithPortEscape m = [RemoteHost m]
matchEscapeToComponent RemoteHostEscape m = [RemoteHost m]
matchEscapeToComponent ProcessIDEscape m = [ProcessID m]
matchEscapeToComponent TimestampWithoutMillisecondsEscape m = [Timestamp m]
matchEscapeToComponent TimestampWithMillisecondsEscape m = [Timestamp m]
matchEscapeToComponent CommandTagEscape m = [CommandTag m]
matchEscapeToComponent SQLStateErrorCodeEscape m = [SQLStateErrorCode m]
matchEscapeToComponent SessionIDEscape m = [SessionID m]
matchEscapeToComponent LogLineNumberEscape m = [LogLineNumber m]
matchEscapeToComponent ProcessStartTimestampEscape m = [ProcessStartTimestamp m]
matchEscapeToComponent VirtualTransactionIDEscape m = [VirtualTransactionID m]
matchEscapeToComponent TransactionIDEscape m = [TransactionID m]
matchEscapeToComponent NonSessionStopEscape _ = []
matchEscapeToComponent LiteralPercentEscape _ = []
