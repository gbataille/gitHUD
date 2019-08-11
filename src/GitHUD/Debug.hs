module GitHUD.Debug (
  debug,
  debugOnStderr
  ) where

import System.IO (stdout, stderr, hFlush, hPutStrLn)

debug :: String
      -> IO ()
debug msg = do
  putStrLn msg
  hFlush stdout

debugOnStderr :: String
              -> IO ()
debugOnStderr msg = do
  hPutStrLn stderr msg
  hFlush stderr
