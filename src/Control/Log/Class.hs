{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Log.Class
  ( MonadLog (..),
  )
where

import Control.Lens ((^.))
import Control.Log.Type (HasLogger (formatTime, logLevel, logOut, logger), LogLevel)
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

-- helpers
--------------------------------------------------------------------------------
padding :: Int -> String -> String
padding k s = s ++ replicate paddingAmount ' '
  where
    paddingAmount = max 0 (k - length s)

-- classes
--------------------------------------------------------------------------------

-- | 'MonadLog' gives the ability to log.
-- 'MonadLog' depends on 'LogLevel'.
class (Monad m) => MonadLog m where
  -- | 'out' tells how to output log, 'out' should never format the string.
  out :: String -> m ()

  -- | 'isOkToOut' tells if this log level should output
  isOkToOut :: LogLevel -> m Bool

  -- | 'getTimeString' tells how to get time string
  getTimeString :: m String

  -- | automatically impl.
  log_ :: LogLevel -> String -> String -> m ()
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

-- instances
--------------------------------------------------------------------------------

-- | impl
instance
  ( HasLogger r,
    MonadIO m,
    Monad m,
    MonadReader r m
  ) =>
  MonadLog m
  where
  out :: String -> m ()
  out s = asks (^. logger . logOut) >>= \f -> liftIO $ f s

  isOkToOut :: LogLevel -> m Bool
  isOkToOut ll = (>= ll) <$> asks (^. logger . logLevel)

  getTimeString :: m String
  getTimeString = asks (^. logger . formatTime) <*> liftIO getCurrentTime
