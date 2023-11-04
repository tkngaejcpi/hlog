{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Log.Type
  ( LogLevel (..),
    Logger (..),
    DefaultLogger,
    LogConfig (..),
    HasLogger (..),
    separator,
    linebreak,
    container,
    scopePadding,
    traceInPrompt,
    traceOutPrompt,
    defaultLogConfig,
    defaultLogger,
    noLog,
  )
where

import Control.Lens (makeClassy, makeLenses, (&), (.~))
import Data.Time (UTCTime, defaultTimeLocale)
import qualified Data.Time (formatTime)

-- types
--------------------------------------------------------------------------------

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

-- | 'Logger' is a running context of 'MonadLog',
-- parameter l represents log level,
-- you can use a more simplified log level or just use 'LogLevel'.
-- 'Logger' is applied template makeClassy.
data Logger l = Logger
  { -- | '_Level' control which level of log should be output.
    _level :: !l,
    -- | '_formatTime' decides a time format.
    _formatTime :: !(UTCTime -> String),
    -- | '_logConfig' is the config to control how the log format
    _logConfig :: !LogConfig,
    -- | '_logOut' controls how to output log. '_logOut' should not append a \'\\n\' automatically.
    _logOut :: !(String -> IO ())
  }

-- | 'Logger' is a 'Logger' with default 'LogLevel' as its type parameter.
type DefaultLogger = Logger LogLevel

-- | 'LogConfig' is the config to control how the log format
data LogConfig = LogConfig
  { _separator :: String,
    _linebreak :: String,
    _container :: (String, String),
    _scopePadding :: Int,
    _traceInPrompt :: String,
    _traceOutPrompt :: String
  }

-- template
$(makeClassy ''Logger)
$(makeLenses ''LogConfig)

-- utils
--------------------------------------------------------------------------------

defaultLogConfig :: LogConfig
defaultLogConfig =
  LogConfig
    { _separator = " ",
      _linebreak = "\n",
      _container = ("[", "]"),
      _scopePadding = 12,
      _traceInPrompt = "function called with input",
      _traceOutPrompt = "function ended, it returned"
    }

defaultLogger :: DefaultLogger
defaultLogger =
  Logger
    { _logConfig = defaultLogConfig,
      _level = Info,
      _formatTime = Data.Time.formatTime defaultTimeLocale "%Y%m%d %H:%M:%S",
      _logOut = putStr
    }

-- | 'noLog' is a 'Logger' output no log.
-- 'noLog' can be a fake logger to bypass HasLogger,
-- it is useful in testing.
noLog :: DefaultLogger
noLog =
  defaultLogger
    & (level .~ None)
      . (formatTime .~ const mempty)
      . (logOut .~ const (pure ()))