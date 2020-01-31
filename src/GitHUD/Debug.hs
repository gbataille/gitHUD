module GitHUD.Debug (
  debug,
  debugOnStderr
  ) where

import Control.Monad (when)
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import System.IO (stdout, stderr, hFlush, hPutStrLn)

debugEnvVar :: String
debugEnvVar = "GITHUD_DEBUG"

debugEnvVarValue :: String
debugEnvVarValue = "TRUE"

isDebugActive :: IO Bool
isDebugActive = do
  env <- lookupEnv debugEnvVar
  let debug = fromMaybe "FALSE" env
  return $ debug == debugEnvVarValue

whenDebugActive :: IO () -> IO ()
whenDebugActive effect = do
  debugActive <- isDebugActive
  if debugActive then effect else return ()

debug :: String
      -> IO ()
debug msg = whenDebugActive $ do
  putStrLn msg
  hFlush stdout

debugOnStderr :: String
              -> IO ()
debugOnStderr msg = whenDebugActive $ do
  hPutStrLn stderr msg
  hFlush stderr
