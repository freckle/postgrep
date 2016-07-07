{-# LANGUAGE OverloadedStrings #-}

module Main where

import PostGrep.GCharts.QueryTimeline

import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TIO
import Text.Blaze.Html.Renderer.Text (renderHtml)

main :: IO ()
main = TIO.writeFile "out.html" $ renderHtml (timelineHTML items)
  where items = [ QueryTimelineItem (read "2016-06-01 18:00:00 UTC")
                                    (read "2016-06-01 18:05:00 UTC")
                                    "Hello"
                , QueryTimelineItem (read "2016-06-01 18:03:00 UTC")
                                    (read "2016-06-01 18:07:00 UTC")
                                    "Bob"
                , QueryTimelineItem (read "2016-06-01 18:03:21 UTC")
                                    (read "2016-06-01 18:30:13 UTC")
                                    "Dole"
                ]
