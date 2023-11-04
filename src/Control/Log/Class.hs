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
import Control.Log.Type (HasLogger (formatTime, level, logConfig, logOut, logger), LogConfig, container, linebreak, scopePadding, separator, traceInPrompt, traceOutPrompt)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader.Class (MonadReader, asks)
import Data.Time (getCurrentTime)

-- helpers
--------------------------------------------------------------------------------
padding :: Int -> String -> String
padding k s = s ++ replicate paddingAmount ' '
  where
    paddingAmount = max 0 (k - length s)

-- classes
--------------------------------------------------------------------------------

-- | 'MonadLog' gives the ability to log.
class (Monad m) => MonadLog l m | m -> l where
  config :: m LogConfig

  -- | 'out' tells how to output log, 'out' should never format the string.
  out :: String -> m ()

  -- | 'isOkToOut' tells if this log level should output
  isOkToOut :: l -> m Bool

  -- | 'getTimeString' tells how to get time string
  getTimeString :: m String

  -- | automatically impl.
  log_ :: l -> String -> String -> m ()
  log_ level scope log = do
    cf <- config
    ok <- isOkToOut level

    when
      ok
      ( do
          getTimeString >>= out >> out (cf ^. separator)
          out
            ( padding
                (cf ^. scopePadding)
                ( (cf ^. container . _1)
                    ++ scope
                    ++ (cf ^. container . _2)
                )
            )
            >> out (cf ^. separator)
          out log
          out (cf ^. linebreak)
      )

  -- | 'traceInOut' hook a function and log its input and output.
  traceInOut :: (Show a, Show b) => l -> String -> (a -> b) -> a -> m b
  traceInOut ll s f a = do
    cf <- config

    log_ ll s ((cf ^. traceInPrompt) ++ (cf ^. separator) ++ show a)
    let b = f a
    log_ ll s ((cf ^. traceOutPrompt) ++ (cf ^. separator) ++ show b)
    return b

-- instances
--------------------------------------------------------------------------------

-- | impl
instance
  ( HasLogger r l,
    MonadIO m,
    Monad m,
    MonadReader r m,
    Ord l
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
