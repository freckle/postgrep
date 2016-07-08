-- | Specifies a Conduit for parsing log lines. This module could easily be
-- split into its own package, but for now it's in here.

module PostGrep.Conduit
  ( logFileConduit
  , logConduit
  ) where

import Control.Monad.Trans.Resource (MonadResource)
import qualified Data.ByteString as BS
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL

import PostGrep

-- | Creates a Conduit from a given file path. This just splits the file into
-- lines and feeds it into 'logConduit'.
logFileConduit :: (MonadResource m) => LogLineParser -> FilePath -> ConduitM a LogEntry m ()
logFileConduit parser filePath =
  CB.sourceFile filePath $=
  CB.lines $=
  logConduit parser

-- | A Conduit that parses raw log lines and groups them into 'LogEntry's.
logConduit :: (Monad m) => LogLineParser -> ConduitM BS.ByteString LogEntry m ()
logConduit parser =
  CL.map (parseLogLine parser) $=
  CL.groupBy groupParsedLines $=
  CL.map (concatMap fst) $=
  CL.map makeEntry
