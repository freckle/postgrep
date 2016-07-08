{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- | This is the core library for the PostGrep collection of packages.

module PostGrep
  ( -- * Log Line Prefix
    -- $loglineprefix
    LogLinePrefix
  , rdsPrefix
  , parseLogLinePrefix

    -- * Log Entry
    -- $logentry
  , LogEntry (..)
  , parseLogLines

  , module PostGrep.LogEntry
  , module PostGrep.LogLine
  , module PostGrep.LogPrefix
  ) where

import PostGrep.LogEntry
import PostGrep.LogLine
import PostGrep.LogPrefix

{-# ANN module "HLint: ignore Use import/export shortcut" #-}

-- $loglineprefix
--
-- Users of Postgres need to specify a @log_line_prefix@ (see here:
-- https://www.postgresql.org/docs/9.6/static/runtime-config-logging.html#GUC-LOG-LINE-PREFIX).
-- This specifies how properties of each log entry are laid out in the log. It
-- is important for our purposes because we need to parse the prefix to extract
-- this data, and to delineate the beginning of new log entries from the
-- continuation of a multi-line log entry.


-- $logentry
--
-- A 'LogEntry' can be considered the fundamental "output unit" of
-- @postgrep-core@.
