-- | Module to parse entire log entries that possibly scan multiple lines.

module PostGrep.LogEntry
  ( LogEntry (..)
  , makeEntry
  , parseLogLines
  , parseLogLine
  , groupParsedLines
  ) where

import qualified Data.ByteString as BS
import Data.List (foldl', groupBy)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Thyme

import PostGrep.LogLine

data LogEntry =
  LogEntry
  { logEntryApplicationName :: Maybe T.Text
  , logEntryUserName :: Maybe T.Text
  , logEntryDatabaseName :: Maybe T.Text
  , logEntryRemoteHost :: Maybe T.Text
  , logEntryRemotePort :: Maybe Int
  , logEntryProcessID :: Maybe Int
  , logEntryTimestamp :: Maybe UTCTime
  , logEntryCommandTag :: Maybe T.Text
  , logEntrySQLStateErrorCode :: Maybe T.Text
  , logEntrySessionID :: Maybe T.Text
  , logEntryLogLineNumber :: Maybe Integer
  , logEntryProcessStartTimestamp :: Maybe T.Text
  , logEntryVirtualTransactionID :: Maybe T.Text
  , logEntryTransactionID :: Maybe T.Text
  , logEntryLogLevel :: Maybe LogLevel
  , logEntryStatement :: Maybe T.Text
  , logEntryDurationMilliseconds :: Maybe Double
  } deriving (Show, Eq)

makeEntry :: [LogEntryComponent] -> LogEntry
makeEntry = foldl' modifyEntry emptyEntry
  where emptyEntry =
          LogEntry
          { logEntryApplicationName = Nothing
          , logEntryUserName = Nothing
          , logEntryDatabaseName = Nothing
          , logEntryRemoteHost = Nothing
          , logEntryRemotePort = Nothing
          , logEntryProcessID = Nothing
          , logEntryTimestamp = Nothing
          , logEntryCommandTag = Nothing
          , logEntrySQLStateErrorCode = Nothing
          , logEntrySessionID = Nothing
          , logEntryLogLineNumber = Nothing
          , logEntryProcessStartTimestamp = Nothing
          , logEntryVirtualTransactionID = Nothing
          , logEntryTransactionID = Nothing
          , logEntryLogLevel = Nothing
          , logEntryStatement = Nothing
          , logEntryDurationMilliseconds = Nothing
          }

modifyEntry :: LogEntry -> LogEntryComponent -> LogEntry
modifyEntry entry (ApplicationName x) = entry { logEntryApplicationName = Just x }
modifyEntry entry (UserName x) = entry { logEntryUserName = Just x }
modifyEntry entry (DatabaseName x) = entry { logEntryDatabaseName = Just x }
modifyEntry entry (RemoteHost x) = entry { logEntryRemoteHost = Just x }
modifyEntry entry (RemotePort x) = entry { logEntryRemotePort = Just x }
modifyEntry entry (ProcessID x) = entry { logEntryProcessID = Just x }
modifyEntry entry (Timestamp x) = entry { logEntryTimestamp = Just x }
modifyEntry entry (CommandTag x) = entry { logEntryCommandTag = Just x }
modifyEntry entry (SQLStateErrorCode x) = entry { logEntrySQLStateErrorCode = Just x }
modifyEntry entry (SessionID x) = entry { logEntrySessionID = Just x }
modifyEntry entry (LogLineNumber x) = entry { logEntryLogLineNumber = Just x }
modifyEntry entry (ProcessStartTimestamp x) = entry { logEntryProcessStartTimestamp = Just x }
modifyEntry entry (VirtualTransactionID x) = entry { logEntryVirtualTransactionID = Just x }
modifyEntry entry (TransactionID x) = entry { logEntryTransactionID = Just x }
modifyEntry entry (LogLevel x) = entry { logEntryLogLevel = Just x }
modifyEntry entry (DurationMilliseconds x) = entry { logEntryDurationMilliseconds = Just x }
modifyEntry entry (Statement x) = entry { logEntryStatement = newStatement (logEntryStatement entry) }
  where newStatement Nothing = Just x
        newStatement (Just x') = Just (x' <> x)


parseLogLines :: LogLineParser -> [BS.ByteString] -> [LogEntry]
parseLogLines parser logLines = fmap makeEntry components
  where parsed = fmap (parseLogLine parser) logLines
        grouped = groupBy groupParsedLines parsed
        components = concatMap fst <$> grouped :: [[LogEntryComponent]]

data PrefixParseResult = Parsed | NoParse

parseLogLine :: LogLineParser -> BS.ByteString -> ([LogEntryComponent], PrefixParseResult)
parseLogLine parser line =
  case parseLine parser line of
    Nothing -> ([Statement $ TE.decodeUtf8 line], NoParse)
    (Just components) -> (components, Parsed)

groupParsedLines
  :: ([LogEntryComponent], PrefixParseResult)
  -> ([LogEntryComponent], PrefixParseResult)
  -> Bool
groupParsedLines (_, Parsed) (_, NoParse) = True
groupParsedLines (_, NoParse) (_, NoParse) = True
groupParsedLines _ _ = False
