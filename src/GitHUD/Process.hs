module GitHUD.Process (
  readProcessWithIgnoreExitCode
  ) where

import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(ExitSuccess))

readProcessWithIgnoreExitCode :: FilePath -> [String] -> String -> IO String
readProcessWithIgnoreExitCode command options stdin = do
  (exCode, stdout, _) <- readProcessWithExitCode command options stdin
  if (exCode == ExitSuccess)
    then return stdout
    else return ""

