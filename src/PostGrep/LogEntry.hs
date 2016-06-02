{-# LANGUAGE RecordWildCards #-}

-- | Module to parse entire log entries that possibly scan multiple lines.

module PostGrep.LogEntry
  ( LogEntry (..)
  , makeEntry
  , parseLogLines
  ) where

import qualified Data.ByteString.Char8 as BS
import Data.List (foldl')
import Data.Monoid ((<>))

import PostGrep.LogLine

data LogEntry =
  LogEntry
  { logEntryApplicationName :: Maybe BS.ByteString
  , logEntryUserName :: Maybe BS.ByteString
  , logEntryDatabaseName :: Maybe BS.ByteString
  , logEntryRemoteHost :: Maybe BS.ByteString
  , logEntryRemotePort :: Maybe BS.ByteString
  , logEntryProcessID :: Maybe BS.ByteString
  , logEntryTimestamp :: Maybe BS.ByteString
  , logEntryCommandTag :: Maybe BS.ByteString
  , logEntrySQLStateErrorCode :: Maybe BS.ByteString
  , logEntrySessionID :: Maybe BS.ByteString
  , logEntryLogLineNumber :: Maybe BS.ByteString
  , logEntryProcessStartTimestamp :: Maybe BS.ByteString
  , logEntryVirtualTransactionID :: Maybe BS.ByteString
  , logEntryTransactionID :: Maybe BS.ByteString
  , logEntryLogLevel :: Maybe LogLevel
  , logEntryStatement :: Maybe BS.ByteString
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
modifyEntry entry (Statement x) = entry { logEntryStatement = newStatement (logEntryStatement entry) }
  where newStatement Nothing = Just x
        newStatement (Just x') = Just (x' <> x)

data LogParseState =
  LogParseState
  { currentEntryComponents :: [LogEntryComponent]
  , previousEntries :: [LogEntry]
  } deriving (Show)

parseLogLines :: LogLineParser -> [BS.ByteString] -> [LogEntry]
parseLogLines parser ts = reverse allEntries
  where LogParseState{..} = foldl' (parseLogLine parser) (LogParseState [] []) ts
        allEntries = maybeAddEntry currentEntryComponents previousEntries

parseLogLine :: LogLineParser -> LogParseState -> BS.ByteString -> LogParseState
parseLogLine parser LogParseState{..} line =
  case parseLine parser line of
    Nothing -> LogParseState (currentEntryComponents ++ [Statement line]) previousEntries
    (Just newComponents) ->
      LogParseState newComponents (maybeAddEntry currentEntryComponents previousEntries)

maybeAddEntry :: [LogEntryComponent] -> [LogEntry] -> [LogEntry]
maybeAddEntry currentEntry entries =
  if null currentEntry then entries else makeEntry currentEntry : entries
