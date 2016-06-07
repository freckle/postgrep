{-# LANGUAGE OverloadedStrings #-}

-- | This is a utility module to parse time values. Parsing times using thyme
-- or attoparsec can be really slow. We use functions from bytestring-read to
-- parse faster.

module PostGrep.ParseTime
  ( parseTimeStamp
  ) where


import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.ByteString.Read
import Data.Thyme
import Data.Thyme.Time.Core

-- | Tries to parse a timestamp of the following two varieties:
-- * YYYY-MM-DD HH:MM:SS
-- * YYYY-MM-DD HH:MM:SS.milliseconds
parseTimeStamp :: BS.ByteString -> Maybe UTCTime
parseTimeStamp bs = do
  ((year, month, day), restDate) <- parseYearMonthDay bs
  day' <- gregorianValid (YearMonthDay year month day)
  (_, restSpace) <- parseSpace restDate
  ((h, m, s), restTime) <- parseHourMinSec restSpace
  let (ms, _) = case parseMilliseconds restTime of
                     Nothing -> (0, restTime)
                     (Just (ms', rest')) -> (ms', rest')
      secondsDiff =
        fromIntegral h * 60 * 60 +
        fromIntegral m * 60 +
        fromIntegral s +
        fromIntegral ms / 1000 :: Double
      diffTime = fromSeconds secondsDiff :: DiffTime
  return $ mkUTCTime day' diffTime


-- | A Parser is a function that tries to eat off the desired value from the
-- beginning of the bytestring and returns the rest of the bytestring.
type Parser a = BS.ByteString -> Maybe (a, BS.ByteString)

parseChar :: Char -> Parser Char
parseChar char bs
  | BS.null bs = Nothing
  | BSC.head bs == char = Just (char, BS.tail bs)
  | otherwise = Nothing

parseSpace :: Parser Char
parseSpace = parseChar ' '

parseYearMonthDay :: Parser (Int, Int, Int)
parseYearMonthDay bs = do
  (year, restYear) <- int bs
  (_, restDash1) <- parseChar '-' restYear
  (month, restMonth) <- int restDash1
  (_, restDash2) <- parseChar '-' restMonth
  (day, rest) <- int restDash2
  return ((year, month, day), rest)

parseHourMinSec :: Parser (Int, Int, Int)
parseHourMinSec bs = do
  (hour, restHour) <- int bs
  (_, restColon1) <- parseChar ':' restHour
  (minute, restMinute) <- int restColon1
  (_, restColon2) <- parseChar ':' restMinute
  (second, rest) <- int restColon2
  return ((hour, minute, second), rest)

parseMilliseconds :: Parser Int
parseMilliseconds bs = do
  (_, restDot) <- parseChar '.' bs
  (ms, rest) <- int restDot
  return (ms, rest)


-- | Tries to parse a short 3-letter month name and returns the 0-based index
-- of that month.
-- parseMonthShort :: Parser Int
-- parseMonthShort bs =
--   case bsHead of
--     "Jan" -> Just (0, bsTail)
--     "Feb" -> Just (1, bsTail)
--     "Mar" -> Just (2, bsTail)
--     "Apr" -> Just (3, bsTail)
--     "May" -> Just (4, bsTail)
--     "Jun" -> Just (5, bsTail)
--     "Jul" -> Just (6, bsTail)
--     "Aug" -> Just (7, bsTail)
--     "Sep" -> Just (8, bsTail)
--     "Oct" -> Just (9, bsTail)
--     "Nov" -> Just (10, bsTail)
--     "Dec" -> Just (11, bsTail)
--     _ -> Nothing
--   where (bsHead, bsTail) = BS.splitAt 3 bs
