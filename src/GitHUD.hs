module GitHUD (
    githud
    ) where

import Control.Monad (when)
import Control.Monad.Reader (runReader)
import System.Environment (getArgs)

import GitHUD.Config.Parse
import GitHUD.Terminal.Prompt
import GitHUD.Terminal.Types
import GitHUD.Git.Parse.Base
import GitHUD.Git.Command

githud :: IO ()
githud = do
  shell <- processArguments getArgs

  -- TODO: gbataille - change that to the home folder
  -- TODO: gbataille - Error if the file does not exists. Have to check first
  config <- parseConfigFile "/Users/gbataille/Documents/Prog/Perso/gitHUD/.githudrc"

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

