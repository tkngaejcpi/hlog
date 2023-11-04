{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Test (main) where

import Control.Lens (Lens', makeLenses, (.~), (^.))
import Control.Log.Class (MonadLog (log_, traceInOut))
import Control.Log.Type (DefaultLogger, HasLogger (logger), LogLevel (Debug, Info, None), Logger (Logger, _formatTime, _level, _logOut), noLog)
import Control.Monad.Reader (MonadIO (liftIO), ReaderT (runReaderT))
import GHC.Conc (STM, TVar, atomically, newTVarIO, readTVar, readTVarIO, writeTVar)
import Test.Hspec (describe, hspec, it, shouldBe)

-- helper
--------------------------------------------------------------------------------
fakeTimeString :: String
fakeTimeString = "19700101 000000"

prefixFakeTime :: String -> String
prefixFakeTime s = fakeTimeString ++ " " ++ s

modifyTVar :: TVar a -> (a -> a) -> STM ()
modifyTVar v f = do
  x <- readTVar v
  writeTVar v (f x)

-- init test env
--------------------------------------------------------------------------------
data TestEnv = TestEnv
  { _lg :: DefaultLogger,
    _proxyLog :: TVar String
  }

$(makeLenses ''TestEnv)

instance HasLogger TestEnv LogLevel where
  logger :: Lens' TestEnv DefaultLogger
  logger = lg

mkTestLogger :: TVar String -> DefaultLogger
mkTestLogger var =
  Logger
    { _level = Info,
      _formatTime = const fakeTimeString,
      _logOut = \s -> atomically $ modifyTVar var (++ s)
    }

mkTestEnv :: IO TestEnv
mkTestEnv = do
  var <- newTVarIO ""
  let l = mkTestLogger var

  return $
    TestEnv
      { _lg = l,
        _proxyLog = var
      }

-- real test
--------------------------------------------------------------------------------
main :: IO ()
main = hspec $ do
  describe "Control.Log" $ do
    it "test env works normally" $ do
      testEnv <- mkTestEnv

      atomically $ modifyTVar (testEnv ^. proxyLog) (++ "test")
      result <- readTVarIO (testEnv ^. proxyLog)

      result `shouldBe` "test"

    it "log with correct format" $ do
      testEnv <- mkTestEnv

      runReaderT
        ( do
            log_ Info "test" "test log"
        )
        testEnv

      result <- readTVarIO (testEnv ^. proxyLog)
      result `shouldBe` prefixFakeTime "[test]       test log\n"

    it "ignore the output with lower loglevel" $ do
      testEnv <- mkTestEnv

      runReaderT
        ( do
            log_ Debug "test" "test log"
        )
        testEnv

      result <- readTVarIO (testEnv ^. proxyLog)
      result `shouldBe` ""

    it "'traceInOut' can hook a function and return input and output" $ do
      testEnv <- mkTestEnv

      runReaderT
        ( do
            let f = traceInOut Info "test" (+ 1)
            let a = 1

            f a
        )
        testEnv

      result <- readTVarIO (testEnv ^. proxyLog)
      result
        `shouldBe` ( prefixFakeTime "[test]       function called with input 1\n"
                       ++ prefixFakeTime "[test]       function ended, it returned 2\n"
                   )

    it "'nolog' with no log output" $ do
      testEnv <- (logger .~ noLog) <$> mkTestEnv

      runReaderT
        ( do
            log_ None "test" "test log"
        )
        testEnv

      result <- readTVarIO (testEnv ^. proxyLog)
      result `shouldBe` ""
