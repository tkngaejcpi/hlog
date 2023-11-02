module HLog
  ( module Control.Log,
  )
where

import qualified Control.Log

someFunc :: IO ()
someFunc = putStrLn "someFunc"
