-- | Main module to parse log entries.

module PostGrep.LogEntry
  ( LogEntryComponent (..)
  ) where

import qualified Data.Text as T

data LogEntryComponent
  = ApplicationName T.Text
  | UserName T.Text
  | DatabaseName T.Text
  | RemoteHost T.Text
  | RemotePort T.Text
  | ProcessID Int
  | Timestamp T.Text
  | CommandTag T.Text
  | SQLStateErrorCode T.Text
  | SessionID T.Text
  | LogLineNumber Integer
  | ProcessStartTimestamp T.Text
  | VirtualTransactionID Int
  | TransactionID Int
  | Message T.Text
  deriving (Show)
