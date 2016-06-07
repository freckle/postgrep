-- | Specifies a Conduit for parsing log lines. This module could easily be
-- split into its own package, but for now it's in here.

module PostGrep.Conduit
  ( logConduit
  ) where

import qualified Data.ByteString as BS
import Data.Conduit
import qualified Data.Conduit.List as CL

import PostGrep.LogEntry
import PostGrep.LogLine

logConduit :: (Monad m) => LogLineParser -> ConduitM BS.ByteString LogEntry m ()
logConduit parser =
  CL.map (parseLogLine parser) $=
  CL.groupBy groupParsedLines $=
  CL.map (concatMap fst) $=
  CL.map makeEntry
