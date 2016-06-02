{-# LANGUAGE OverloadedStrings #-}

-- | This module defines the various options and types associated with the log
-- line prefix.

module PostGrep.LogPrefix
  ( LogLinePrefix (..)
  , rdsPrefix
  , parseLogLinePrefix
  , LogLinePrefixComponent (..)
  , LogLinePrefixEscape (..)
  , getEscape
  , logLinePrefixEscapeChar
  , prefixRegexString
  ) where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text
import qualified Data.Text as T

-- | Represents a parsed log_line_prefix. This is used to extract data from log
-- entries.
newtype LogLinePrefix = LogLinePrefix { unLogLinePrefix :: [LogLinePrefixComponent] }
                      deriving (Show, Eq)

-- | Default prefix for AWS RDS.
rdsPrefix :: LogLinePrefix
rdsPrefix =
  case parseLogLinePrefix "%t:%r:%u@%d:[%p]:" of
    (Left err) -> error $ "Failed to parse RDS log line prefix: " ++ err
    (Right p) -> p

data LogLinePrefixComponent
  = LogLineLiteral T.Text
  | LogLineEscape LogLinePrefixEscape
  deriving (Show, Eq)

-- | Type enumerating the possible escape characters in a log_line_prefix.
data LogLinePrefixEscape
  = ApplicationNameEscape
  | UserNameEscape
  | DatabaseNameEscape
  | RemoteHostWithPortEscape
  | RemoteHostEscape
  | ProcessIDEscape
  | TimestampWithoutMillisecondsEscape
  | TimestampWithMillisecondsEscape
  | CommandTagEscape
  | SQLStateErrorCodeEscape
  | SessionIDEscape
  | LogLineNumberEscape
  | ProcessStartTimestampEscape
  | VirtualTransactionIDEscape
  | TransactionIDEscape
  | NonSessionStopEscape
  | LiteralPercentEscape
  deriving (Show, Eq)

getEscape :: LogLinePrefixComponent -> Maybe LogLinePrefixEscape
getEscape (LogLineLiteral _) = Nothing
getEscape (LogLineEscape e) = Just e

-- | The corresponding escape character for each 'LogLinePrefixEscape'
logLinePrefixEscapeChar :: LogLinePrefixEscape -> Char
logLinePrefixEscapeChar ApplicationNameEscape = 'a'
logLinePrefixEscapeChar UserNameEscape = 'u'
logLinePrefixEscapeChar DatabaseNameEscape = 'd'
logLinePrefixEscapeChar RemoteHostWithPortEscape = 'r'
logLinePrefixEscapeChar RemoteHostEscape = 'h'
logLinePrefixEscapeChar ProcessIDEscape = 'p'
logLinePrefixEscapeChar TimestampWithoutMillisecondsEscape = 't'
logLinePrefixEscapeChar TimestampWithMillisecondsEscape = 'm'
logLinePrefixEscapeChar CommandTagEscape = 'i'
logLinePrefixEscapeChar SQLStateErrorCodeEscape = 'e'
logLinePrefixEscapeChar SessionIDEscape = 'c'
logLinePrefixEscapeChar LogLineNumberEscape = 'l'
logLinePrefixEscapeChar ProcessStartTimestampEscape = 's'
logLinePrefixEscapeChar VirtualTransactionIDEscape = 'v'
logLinePrefixEscapeChar TransactionIDEscape = 'x'
logLinePrefixEscapeChar NonSessionStopEscape = 'q'
logLinePrefixEscapeChar LiteralPercentEscape = '%'

charToEscape :: Char -> Maybe LogLinePrefixEscape
charToEscape 'a' = Just ApplicationNameEscape
charToEscape 'u' = Just UserNameEscape
charToEscape 'd' = Just DatabaseNameEscape
charToEscape 'r' = Just RemoteHostWithPortEscape
charToEscape 'h' = Just RemoteHostEscape
charToEscape 'p' = Just ProcessIDEscape
charToEscape 't' = Just TimestampWithoutMillisecondsEscape
charToEscape 'm' = Just TimestampWithMillisecondsEscape
charToEscape 'i' = Just CommandTagEscape
charToEscape 'e' = Just SQLStateErrorCodeEscape
charToEscape 'c' = Just SessionIDEscape
charToEscape 'l' = Just LogLineNumberEscape
charToEscape 's' = Just ProcessStartTimestampEscape
charToEscape 'v' = Just VirtualTransactionIDEscape
charToEscape 'x' = Just TransactionIDEscape
charToEscape 'q' = Just NonSessionStopEscape
charToEscape '%' = Just LiteralPercentEscape
charToEscape _ = Nothing

parseLogLinePrefix :: T.Text -> Either String LogLinePrefix
parseLogLinePrefix = parseOnly (prefixParser <* endOfInput)

prefixParser :: Parser LogLinePrefix
prefixParser = LogLinePrefix <$> many1 componentParser

componentParser :: Parser LogLinePrefixComponent
componentParser = parseLiteral <|> parseEscape

parseLiteral :: Parser LogLinePrefixComponent
parseLiteral = LogLineLiteral <$> takeWhile1 (/= '%')

parseEscape :: Parser LogLinePrefixComponent
parseEscape = do
  _ <- char '%'
  escapeChar <- anyChar
  case charToEscape escapeChar of
    Nothing -> fail $ "Unknown escape character '" ++ [escapeChar] ++ "'"
    (Just e) -> return $ LogLineEscape e


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
