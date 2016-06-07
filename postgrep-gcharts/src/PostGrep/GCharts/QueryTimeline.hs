{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module PostGrep.GCharts.QueryTimeline
  ( timelineHTML
  , QueryTimelineItem (..)
  ) where

import Data.Monoid ((<>))
import qualified Data.Text.Lazy as T
import Data.Thyme
import Text.Hamlet
import Text.Julius
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html (preEscapedToHtml)


timelineHTML :: [QueryTimelineItem] -> Html
timelineHTML items = $(shamletFile "templates/timeline.hamlet")
  where dataItems = map timelineRowHtml items

timelineRowHtml :: QueryTimelineItem -> Html
timelineRowHtml (QueryTimelineItem st et s) = preEscapedToHtml $
  "['" <> s <> "', new Date(" <> showTime st <> "), new Date(" <> showTime et <> ")],"
   where showTime t = "\"" <> T.pack (show t) <> "\""

data QueryTimelineItem =
  QueryTimelineItem
  { queryTimelineItemStartTime :: UTCTime
  , queryTimelineItemEndTime :: UTCTime
  , queryTImelineItemStatement :: T.Text
  } deriving (Show)
