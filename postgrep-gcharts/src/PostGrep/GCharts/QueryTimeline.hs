{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module PostGrep.GCharts.QueryTimeline
  ( timelineHTML
  , QueryTimelineItem (..)
  , logEntryToTimelineItem
  ) where

import Data.AffineSpace ((.+^))
import Data.Monoid ((<>))
import qualified Data.Text.Lazy as TL
import Data.Thyme
import Text.Hamlet
import Text.Blaze.Html (preEscapedToHtml)

import PostGrep

timelineHTML :: [QueryTimelineItem] -> Html
timelineHTML items = $(shamletFile "templates/timeline.hamlet")
  where dataItems = map timelineRowHtml items

timelineRowHtml :: QueryTimelineItem -> Html
timelineRowHtml (QueryTimelineItem be s st et) = preEscapedToHtml $
  "['" <> be' <> "','" <> escapedStatement <> "', new Date(" <> showTime st <> "), new Date(" <> showTime et <> ")],\n"
   where be' = TL.pack $ show be
         escapedStatement = TL.replace "'" "\\'" s
         showTime t = "\"" <> TL.pack (show t) <> "\""


data QueryTimelineItem =
  QueryTimelineItem
  { queryTimelineItemBackend :: Int
  , queryTimelineItemStatement :: TL.Text
  , queryTimelineItemStartTime :: UTCTime
  , queryTimelineItemEndTime :: UTCTime
  } deriving (Show)

logEntryToTimelineItem :: LogEntry -> Maybe QueryTimelineItem
logEntryToTimelineItem entry =
  QueryTimelineItem
  <$> logEntryProcessID entry
  <*> (TL.fromStrict <$> logEntryStatement entry)
  <*> logEntryTimestamp entry
  <*> endTime
  where endTime = do
          startTime <- logEntryTimestamp entry
          durationMs <- logEntryDurationMilliseconds entry
          let diffTime = fromSeconds (durationMs / 1000)
          return $ startTime .+^ diffTime
