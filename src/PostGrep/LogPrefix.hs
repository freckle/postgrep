{-# LANGUAGE OverloadedStrings #-}

-- | This module defines the various options and types associated with the log
-- line prefix.

module PostGrep.LogPrefix
  ( LogLinePrefix (..)
  , rdsPrefix
  , parseLogLinePrefix
  , LogLinePrefixEscape (..)
  , logLinePrefixEscapeChar
  ) where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text
import qualified Data.Text as T

-- | Represents a parsed log_line_prefix. This is used to extract data from log
-- entries.
newtype LogLinePrefix = LogLinePrefix { unLogLinePrefix :: [LogLinePrefixComponent] }
                      deriving (Show)

-- | Default prefix for AWS RDS.
rdsPrefix :: LogLinePrefix
rdsPrefix =
  case parseLogLinePrefix "%t:%r:%u@%d:[%p]:" of
    (Left err) -> error $ "Failed to parse RDS log line prefix: " ++ err
    (Right p) -> p

data LogLinePrefixComponent
  = LogLineLiteral T.Text
  | LogLineEscapeChar LogLinePrefixEscape
  deriving (Show)

-- | Type enumerating the possible escape characters in a log_line_prefix.
data LogLinePrefixEscape
  = ApplicationName
  | UserName
  | DatabaseName
  | RemoteHostWithPort
  | RemoteHost
  | ProcessID
  | TimestampWithoutMilliseconds
  | TimestampWithMilliseconds
  | CommandTag
  | SQLStateErrorCode
  | SessionID
  | LogLineNumber
  | ProcessStartTimestamp
  | VirtualTransactionID
  | TransactionID
  | NonSessionStop
  | LiteralPercent
  deriving (Show)

-- | The corresponding escape character for each 'LogLinePrefixEscape'
logLinePrefixEscapeChar :: LogLinePrefixEscape -> Char
logLinePrefixEscapeChar ApplicationName = 'a'
logLinePrefixEscapeChar UserName = 'u'
logLinePrefixEscapeChar DatabaseName = 'd'
logLinePrefixEscapeChar RemoteHostWithPort = 'r'
logLinePrefixEscapeChar RemoteHost = 'h'
logLinePrefixEscapeChar ProcessID = 'p'
logLinePrefixEscapeChar TimestampWithoutMilliseconds = 't'
logLinePrefixEscapeChar TimestampWithMilliseconds = 'm'
logLinePrefixEscapeChar CommandTag = 'i'
logLinePrefixEscapeChar SQLStateErrorCode = 'e'
logLinePrefixEscapeChar SessionID = 'c'
logLinePrefixEscapeChar LogLineNumber = 'l'
logLinePrefixEscapeChar ProcessStartTimestamp = 's'
logLinePrefixEscapeChar VirtualTransactionID = 'v'
logLinePrefixEscapeChar TransactionID = 'x'
logLinePrefixEscapeChar NonSessionStop = 'q'
logLinePrefixEscapeChar LiteralPercent = '%'

charToEscape :: Char -> Maybe LogLinePrefixEscape
charToEscape 'a' = Just ApplicationName
charToEscape 'u' = Just UserName
charToEscape 'd' = Just DatabaseName
charToEscape 'r' = Just RemoteHostWithPort
charToEscape 'h' = Just RemoteHost
charToEscape 'p' = Just ProcessID
charToEscape 't' = Just TimestampWithoutMilliseconds
charToEscape 'm' = Just TimestampWithMilliseconds
charToEscape 'i' = Just CommandTag
charToEscape 'e' = Just SQLStateErrorCode
charToEscape 'c' = Just SessionID
charToEscape 'l' = Just LogLineNumber
charToEscape 's' = Just ProcessStartTimestamp
charToEscape 'v' = Just VirtualTransactionID
charToEscape 'x' = Just TransactionID
charToEscape 'q' = Just NonSessionStop
charToEscape '%' = Just LiteralPercent
charToEscape _ = Nothing

parseLogLinePrefix :: T.Text -> Either String LogLinePrefix
parseLogLinePrefix = parseOnly prefixParser

prefixParser :: Parser LogLinePrefix
prefixParser = LogLinePrefix <$> many1 componentParser

componentParser :: Parser LogLinePrefixComponent
componentParser = parseEscape <|> parseLiteral

parseLiteral :: Parser LogLinePrefixComponent
parseLiteral = LogLineLiteral <$> takeWhile1 (/= '%')

parseEscape :: Parser LogLinePrefixComponent
parseEscape = do
  _ <- char '%'
  escapeChar <- anyChar
  case charToEscape escapeChar of
    Nothing -> fail $ "Unknown escape character '" ++ [escapeChar] ++ "'"
    (Just e) -> return $ LogLineEscapeChar e
