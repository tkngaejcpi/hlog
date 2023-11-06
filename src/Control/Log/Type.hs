{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Log.Type
  ( LogLevel (..),
    Logger (..),
    DefaultLogger,
    LogFormatString (..),
    LogPadding (..),
    LogPrompt (..),
    LogConfig (..),
    HasLogger (..),
    separator,
    messageSeparator,
    linebreak,
    debugLevelPadding,
    scopePadding,
    traceInPrompt,
    traceOutPrompt,
    formatString,
    padding,
    prompt,
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
    -- | '_logConfig' is the config to control how the log format.
    _logConfig :: !LogConfig,
    -- | '_logOut' controls how to output log. '_logOut' should not append a \'\\n\' automatically.
    _logOut :: !(String -> IO ())
  }

-- | 'Logger' is a 'Logger' with default 'LogLevel' as its type parameter.
type DefaultLogger = Logger LogLevel

-- | 'LogConfig' is the config to control how the log format.
data LogConfig = LogConfig
  { _formatString :: LogFormatString,
    _padding :: LogPadding,
    _prompt :: LogPrompt
  }
  deriving (Show, Eq)

-- | 'LogFormatString' is the collection of the string used to format the log.
data LogFormatString = LogFormatString
  { _separator :: String,
    _messageSeparator :: String,
    _linebreak :: String
  }
  deriving (Show, Eq)

-- | 'LogPadding' is the collection of the padding of the log.
data LogPadding = LogPadding
  { _debugLevelPadding :: Int,
    _scopePadding :: Int
  }
  deriving (Show, Eq)

-- | 'LogPrompt' is the collection of the prompts will be used in logging.
data LogPrompt = LogPrompt
  { _traceInPrompt :: String,
    _traceOutPrompt :: String
  }
  deriving (Show, Eq)

-- template
$(makeClassy ''Logger)
$(makeLenses ''LogConfig)
$(makeLenses ''LogFormatString)
$(makeLenses ''LogPadding)
$(makeLenses ''LogPrompt)

-- utils
--------------------------------------------------------------------------------
defaultLogFormatString :: LogFormatString
defaultLogFormatString =
  LogFormatString
    { _separator = " | ",
      _messageSeparator = " :: ",
      _linebreak = "\n"
    }

defaultLogPadding :: LogPadding
defaultLogPadding =
  LogPadding
    { _debugLevelPadding = 8,
      _scopePadding = 12
    }

enUSLogPrompt :: LogPrompt
enUSLogPrompt =
  LogPrompt
    { _traceInPrompt = "function called with input",
      _traceOutPrompt = "function ended, it returned"
    }

defaultLogPrompt :: LogPrompt
defaultLogPrompt = enUSLogPrompt

defaultLogConfig :: LogConfig
defaultLogConfig =
  LogConfig
    { _formatString = defaultLogFormatString,
      _padding = defaultLogPadding,
      _prompt = defaultLogPrompt
    }

defaultLogger :: DefaultLogger
defaultLogger =
  Logger
    { _logConfig = defaultLogConfig,
      _level = Info,
      _formatTime = Data.Time.formatTime defaultTimeLocale "%Y%m%d%H:%M:%S",
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
