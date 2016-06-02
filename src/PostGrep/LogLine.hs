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
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
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

logLevelRegex :: String
logLevelRegex = "\\s*(LOG|WARNING|ERROR|FATAL|PANIC|DETAIL|STATEMENT|HINT|CONTEXT|LOCATION):"

statementRegex :: String
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
matchToComponent LogLevelComponent m = [LogLevel (read $ T.unpack $ TE.decodeUtf8 m)]
matchToComponent StatementComponent m = [Statement$ TE.decodeUtf8 m]

matchEscapeToComponent :: LogLinePrefixEscape -> BS.ByteString -> [LogEntryComponent]
matchEscapeToComponent ApplicationNameEscape m = [ApplicationName $ TE.decodeUtf8 m]
matchEscapeToComponent UserNameEscape m = [UserName $ TE.decodeUtf8 m]
matchEscapeToComponent DatabaseNameEscape m = [DatabaseName $ TE.decodeUtf8 m]
matchEscapeToComponent RemoteHostWithPortEscape m = [RemoteHost $ TE.decodeUtf8 m]
matchEscapeToComponent RemoteHostEscape m = [RemoteHost $ TE.decodeUtf8 m]
matchEscapeToComponent ProcessIDEscape m = [ProcessID $ TE.decodeUtf8 m]
matchEscapeToComponent TimestampWithoutMillisecondsEscape m = [Timestamp $ TE.decodeUtf8 m]
matchEscapeToComponent TimestampWithMillisecondsEscape m = [Timestamp $ TE.decodeUtf8 m]
matchEscapeToComponent CommandTagEscape m = [CommandTag $ TE.decodeUtf8 m]
matchEscapeToComponent SQLStateErrorCodeEscape m = [SQLStateErrorCode $ TE.decodeUtf8 m]
matchEscapeToComponent SessionIDEscape m = [SessionID $ TE.decodeUtf8 m]
matchEscapeToComponent LogLineNumberEscape m = [LogLineNumber $ TE.decodeUtf8 m]
matchEscapeToComponent ProcessStartTimestampEscape m = [ProcessStartTimestamp $ TE.decodeUtf8 m]
matchEscapeToComponent VirtualTransactionIDEscape m = [VirtualTransactionID $ TE.decodeUtf8 m]
matchEscapeToComponent TransactionIDEscape m = [TransactionID $ TE.decodeUtf8 m]
matchEscapeToComponent NonSessionStopEscape _ = []
matchEscapeToComponent LiteralPercentEscape _ = []
