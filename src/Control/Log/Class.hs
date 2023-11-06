{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Control.Log.Class
  ( MonadLog (..),
  )
where

import Control.Lens (Field1 (_1), Field2 (_2), (^.))
import Control.Log.Type (HasLogger (formatTime, level, logConfig, logOut, logger), LogConfig, debugLevelPadding, formatString, linebreak, messageSeparator, padding, prompt, scopePadding, separator, traceInPrompt, traceOutPrompt)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader.Class (MonadReader, asks)
import Data.Time (getCurrentTime)

-- helpers
--------------------------------------------------------------------------------
formatLog :: (Show l) => LogConfig -> String -> l -> String -> String -> String
formatLog cfg time level scope msg =
  time
    ++ sprt
    ++ rightPadding (cfg ^. padding . debugLevelPadding) (show level)
    ++ sprt
    ++ rightPadding (cfg ^. padding . scopePadding) scope
    ++ (cfg ^. formatString . messageSeparator)
    ++ msg
    ++ (cfg ^. formatString . linebreak)
  where
    sprt = cfg ^. formatString . separator
    rightPadding k s = s ++ replicate (max (length s - k) 0) ' '

-- classes
--------------------------------------------------------------------------------

-- | 'MonadLog' gives the ability to log.
class (Monad m, Show l) => MonadLog l m | m -> l where
  config :: m LogConfig

  -- | 'out' tells how to output log, 'out' should never format the string.
  out :: String -> m ()

  -- | 'isOkToOut' tells if this log level should output
  isOkToOut :: l -> m Bool

  -- | 'getTimeString' tells how to get time string
  getTimeString :: m String

  -- | automatically impl.
  log_ :: l -> String -> String -> m ()
  log_ level scope msg = do
    cfg <- config
    ok <- isOkToOut level

    when ok (getTimeString >>= \t -> out (formatLog cfg t level scope msg))

  -- | 'traceInOut' hook a function and log its input and output.
  traceInOut :: (Show a, Show b) => l -> String -> (a -> b) -> a -> m b
  traceInOut ll s f a = do
    cfg <- config

    log_ ll s ((cfg ^. prompt . traceInPrompt) ++ " " ++ show a)
    let b = f a
    log_ ll s ((cfg ^. prompt . traceOutPrompt) ++ " " ++ show b)
    return b

-- instances
--------------------------------------------------------------------------------

-- | impl
instance
  ( Show l,
    Ord l,
    HasLogger r l,
    MonadIO m,
    Monad m,
    MonadReader r m
  ) =>
  MonadLog l m
  where
  config :: m LogConfig
  config = asks (^. logger . logConfig)

  out :: String -> m ()
  out s = asks (^. logger . logOut) >>= \f -> liftIO $ f s

  isOkToOut :: l -> m Bool
  isOkToOut ll = (>= ll) <$> asks (^. logger . level)

  getTimeString :: m String
  getTimeString = asks (^. logger . formatTime) <*> liftIO getCurrentTime
