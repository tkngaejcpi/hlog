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

import Control.Lens ((^.))
import Control.Log.Type (HasLogger (formatTime, level, logOut, logger))
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader.Class (MonadReader, asks)
import Data.Time (getCurrentTime)

-- constants
--------------------------------------------------------------------------------
separator :: String
separator = " "

lineBreak :: String
lineBreak = "\n"

container :: (String, String)
container = ("[", "]")

scopePadding :: Int
scopePadding = 12

traceInPrompt :: String
traceInPrompt = "function called with input"

traceOutPrompt :: String
traceOutPrompt = "function ended, it returned"

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
  -- | 'out' tells how to output log, 'out' should never format the string.
  out :: String -> m ()

  -- | 'isOkToOut' tells if this log level should output
  isOkToOut :: l -> m Bool

  -- | 'getTimeString' tells how to get time string
  getTimeString :: m String

  -- | automatically impl.
  log_ :: l -> String -> String -> m ()
  log_ level scope log =
    isOkToOut level
      >>= flip
        when
        ( do
            getTimeString >>= out >> out separator
            out (padding scopePadding (fst container ++ scope ++ snd container)) >> out separator
            out log
            out lineBreak
        )

  -- | 'traceInOut' hook a function and log its input and output.
  traceInOut :: (Show a, Show b) => l -> String -> (a -> b) -> a -> m b
  traceInOut ll s f a = do
    log_ ll s (traceInPrompt ++ separator ++ show a)
    let b = f a
    log_ ll s (traceOutPrompt ++ separator ++ show b)
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
  out :: String -> m ()
  out s = asks (^. logger . logOut) >>= \f -> liftIO $ f s

  isOkToOut :: l -> m Bool
  isOkToOut ll = (>= ll) <$> asks (^. logger . level)

  getTimeString :: m String
  getTimeString = asks (^. logger . formatTime) <*> liftIO getCurrentTime
