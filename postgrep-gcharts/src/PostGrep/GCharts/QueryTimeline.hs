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

-- | Produce the HTML source for a timeline visualization using Google Charts.
timelineHTML :: [QueryTimelineItem] -> Html
timelineHTML items = $(shamletFile "templates/timeline.hamlet")
  where dataItems = map timelineRowHtml items

timelineRowHtml :: QueryTimelineItem -> Html
timelineRowHtml (QueryTimelineItem be s st et) = preEscapedToHtml $
  "['" <> be' <> "','" <> escapedStatement <> "', new Date(" <> showTime st <> "), new Date(" <> showTime et <> ")],\n"
   where be' = TL.pack $ show be
         escapedStatement = TL.replace "'" "\\'" s
         showTime t = "\"" <> TL.pack (show t) <> "\""

-- | This type is fed into 'timelineHTML' to produce a timeline visualization.
data QueryTimelineItem =
  QueryTimelineItem
  { queryTimelineItemBackend :: Int
  , queryTimelineItemStatement :: TL.Text
  , queryTimelineItemStartTime :: UTCTime
  , queryTimelineItemEndTime :: UTCTime
  } deriving (Show)

-- | Tries to convert a 'LogEntry' into a 'QueryTimelineItem', but fails if
-- some required fields aren't present. This is ideally used along with
-- @Data.Conduit.List.mapMaybe@ and @postgrep-conduit@.
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
