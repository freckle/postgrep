{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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
import Options.Applicative
import System.Environment (getArgs)
import System.Exit
import Text.Blaze.Html.Renderer.Text (renderHtml)

main :: IO ()
main = do
  Options{..} <- execParser $ info (helper <*> parseOptions)
                 (fullDesc <> progDesc "Timeline visualization of postgres logs")
  let parser = logLineParser rdsPrefix
      minDurationFilter = maybe (const True) (\md -> maybe False (> md) . logEntryDurationMilliseconds) optMinDurationMs
  items <- runResourceT $
    CB.sourceFile optFilePath $=
    CB.lines $=
    logConduit parser $=
    CL.filter minDurationFilter $=
    CL.mapMaybe logEntryToTimelineItem $$
    CL.consume
  print $ length items
  TIO.writeFile optOutputFile $ renderHtml (timelineHTML items)

data Options =
  Options
  { optFilePath :: String
  , optOutputFile :: String
  , optMinDurationMs :: Maybe Double
  } deriving (Show)

parseOptions :: Parser Options
parseOptions =
  Options
  <$> argument str
  ( metavar "FILEPATH" <>
    helpDoc (Just "Path to log file")
  )
  <*> option str
  ( metavar "OUTPUT_FILE" <>
    long "output" <>
    short 'o' <>
    value "out.html" <>
    helpDoc (Just "File to write generated HTML to")
  )
  <*> optional (
  option auto
  ( metavar "MIN_DURATION" <>
    long "minimum-duration" <>
    short 'm' <>
    helpDoc (Just "Minimum query duration in milliseconds")
  ))
