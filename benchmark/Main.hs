{-# LANGUAGE OverloadedStrings #-}

import Criterion.Main

import qualified Data.ByteString as BS
import Data.List (intersperse)

import PostGrep

setupEnv :: IO ([BS.ByteString], [BS.ByteString], [BS.ByteString])
setupEnv = do
  let single = shortEntry
      small = concat $ take 1000 $ intersperse longEntry (repeat shortEntry)
      large = concat $ take 100000 $ intersperse longEntry (repeat shortEntry)
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

main :: IO ()
main = defaultMain [
   -- notice the lazy pattern match here!
   env setupEnv $ \ ~(single, small, large) -> bgroup "main"
     [ bench "parseLines single" $ nf length (parseLogLines rdsParser single)
     , bench "parseLines small" $ nf length (parseLogLines rdsParser small)
     , bench "parseLines large" $ nf length (parseLogLines rdsParser large)
     ]
   ]
