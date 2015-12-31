module GitHUD (
    githud
    ) where

import Control.Monad (when)
import Control.Monad.Reader (runReader)
import System.Environment (getArgs)

import GitHUD.Terminal.Prompt
import GitHUD.Terminal.Types
import GitHUD.Git.Parse.Base
import GitHUD.Git.Command

githud :: IO ()
githud = do
  shell <- processArguments getArgs

  isGit <- checkInGitDirectory
  when isGit $ do
    repoState <- getGitRepoState
    let prompt = runReader buildPromptWithConfig $ buildOutputConfig shell repoState

    -- Necessary to use putStrLn to properly terminate the output (needs the CR)
    putStrLn prompt

processArguments :: IO [String]
                 -> IO Shell
processArguments args = do
  arguments <- args
  if (not (null arguments)) && ((head arguments) == "zsh")
    then return ZSH
    else return Other

