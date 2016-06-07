{-# LANGUAGE OverloadedStrings #-}

import Criterion.Main

import qualified Data.ByteString as BS
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.List (intersperse)
import Data.Maybe (catMaybes)

import PostGrep

setupEnv :: IO ([BS.ByteString], [BS.ByteString], [BS.ByteString])
setupEnv = do
  let single = shortEntry
      small = concat $ take 1000 $ intersperse longEntry (repeat shortEntry)
      large = concat $ take 500000 $ intersperse longEntry (repeat shortEntry)
  return (single, small, large)

shortEntry :: [BS.ByteString]
shortEntry =
  ["2016-05-19 18:44:03 UTC:10.0.120.51(52251):classroom_api@classroom_prod:[7652]:LOG:  duration: 0.013 ms  statement: BEGIN"]

longEntry :: [BS.ByteString]
longEntry =
  [ "2016-05-19 18:44:15 UTC:10.0.120.51(52094):datadog@classroom_prod:[6580]:LOG:  duration: 10.753 ms  statement:"
  , "SELECT schemaname, count(*)"
  , "FROM pg_stat_user_tables"
  , "GROUP BY schemaname"
  ]

rdsParser :: LogLineParser
rdsParser = logLineParser rdsPrefix

-- | We are running the filter so we are sure the log entries are fully
-- created. Without the filter, we don't actually do full parsing.
listBench :: [BS.ByteString] -> Int
listBench =
  length .
  filter (> 10) .
  catMaybes .
  fmap logEntryDurationMilliseconds .
  parseLogLines rdsParser


conduitBench :: [BS.ByteString] -> IO Int
conduitBench logLines =
  CL.sourceList logLines $=
  logConduit rdsParser $=
  CL.filter (maybe False (> 10) . logEntryDurationMilliseconds) $$
  CL.fold (\x _ -> x + 1) 0

main :: IO ()
main = defaultMain [
   -- notice the lazy pattern match here!
   env setupEnv $ \ ~(single, small, large) -> bgroup "main"
     [ bench "parseLines single" $ nf listBench single
     , bench "parseLines small" $ nf listBench small
     --, bench "parseLines large" $ nf listBench large
     , bench "logConduit large" $ nfIO (conduitBench large)
     ]
   ]
