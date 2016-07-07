{-# LANGUAGE OverloadedStrings #-}

module Main where

import PostGrep
import PostGrep.Conduit
import PostGrep.GCharts.QueryTimeline

import Control.Monad.Trans.Resource
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TIO
import Data.Thyme
import Safe (headMay)
import System.Environment (getArgs)
import System.Exit
import Text.Blaze.Html.Renderer.Text (renderHtml)

main :: IO ()
main = do
  filePath <- getArg
  let parser = logLineParser rdsPrefix
  items <- runResourceT $
    CB.sourceFile filePath $=
    CB.lines $=
    logConduit parser $=
    CL.filter (maybe False (> 100) . logEntryDurationMilliseconds) $=
    CL.mapMaybe logEntryToTimelineItem $$
    CL.consume
  print $ length items
  TIO.writeFile "out.html" $ renderHtml (timelineHTML items)

getArg :: IO String
getArg = do
  mFilePath <- headMay <$> getArgs
  maybe (putStrLn "Usage: postgrep-conduit PATH" >> exitFailure) return mFilePath
