{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Log.Type
  ( LogLevel (..),
    Logger (..),
    DefaultLogger,
    HasLogger (..),
    noLog,
  )
where

import Control.Lens (makeClassy, makeLenses)
import Data.Time (UTCTime)

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

-- | 'Logger' is a running context of 'MonadLog'
-- 'Logger' is applied template makeClassy.
data Logger l = Logger
  { -- | '_Level' control which level of log should be output.
    _level :: !l,
    -- | '_formatTime' decide a time format.
    _formatTime :: !(UTCTime -> String),
    -- | '_logOut' control how to output log. '_logOut' should not append a \'\\n\' automatically.
    _logOut :: !(String -> IO ())
  }

-- | 'Logger' is a 'Logger' with default 'LogLevel' as its type parameter.
type DefaultLogger = Logger LogLevel

-- template
$(makeClassy ''Logger)

-- utils
--------------------------------------------------------------------------------

-- | 'noLog' is a 'Logger' output no log.
-- 'noLog' can be a fake logger to bypass HasLogger,
-- it is useful in testing.
noLog :: DefaultLogger
noLog =
  Logger
    { _level = None,
      _formatTime = const mempty,
      _logOut = const $ pure ()
    }
