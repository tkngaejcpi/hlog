{-# LANGUAGE TemplateHaskell #-}

module Control.Log.Type
  ( LogLevel (..),
    Logger (..),
    HasLogger (..),
    noLog,
  )
where

import Control.Lens (makeClassy)
import Data.Time (UTCTime)

-- | 'LogLevel' is an adhoc type of type 'MonadLog'.
data LogLevel
  = None
  | Fatal
  | Error
  | Warn
  | Info
  | Debug
  | Trace
  | All
  deriving (Show, Eq, Ord)

-- | 'Logger' is a running context of 'MonadLog'
-- 'Logger' is applied template makeClassy.
data Logger = Logger
  { -- | '_logLevel' control which level of log should be output.
    _logLevel :: !LogLevel,
    -- | '_formatTime' decide a time format.
    _formatTime :: !(UTCTime -> String),
    -- | '_logOut' control how to output log. '_logOut' should not append a \'\\n\' automatically.
    _logOut :: !(String -> IO ())
  }

-- template
$(makeClassy ''Logger)

-- some utils

-- | 'noLog' is a 'Logger' output no log.
-- 'noLog' can be a fake logger to bypass HasLogger,
-- it is useful in testing.
noLog :: Logger
noLog =
  Logger
    { _logLevel = None,
      _formatTime = const mempty,
      _logOut = const $ pure ()
    }
