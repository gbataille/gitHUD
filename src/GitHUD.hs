module GitHUD (
    githud
    ) where

import Control.Monad (when)
import Control.Monad.Reader (runReader)
import System.Environment (getArgs)
import System.Posix.Files (fileExist)
import System.Posix.User (getRealUserID, getUserEntryForID, UserEntry(..))

import GitHUD.Config.Parse
import GitHUD.Config.Types
import GitHUD.Terminal.Prompt
import GitHUD.Terminal.Types
import GitHUD.Git.Parse.Base
import GitHUD.Git.Command
import GitHUD.Types

githud :: IO ()
githud = do
  -- Exit ASAP if we are not in a git repository
  isGit <- checkInGitDirectory
  when isGit $ do
    shell <- processArguments getArgs
    config <- getAppConfig
    repoState <- getGitRepoState
    let prompt = runReader buildPromptWithConfig $ buildOutputConfig shell repoState config

    -- Necessary to use putStrLn to properly terminate the output (needs the CR)
    putStrLn prompt

processArguments :: IO [String]
                 -> IO Shell
processArguments args = do
  arguments <- args
  if (not (null arguments)) && ((head arguments) == "zsh")
    then return ZSH
    else return Other

getAppConfig :: IO Config
getAppConfig = do
  uid <- getRealUserID
  userEntry <- getUserEntryForID uid
  let configFilePath = (homeDirectory userEntry) ++ "/.githudrc"
  configFilePresent <- fileExist configFilePath
  if configFilePresent
    then parseConfigFile configFilePath
    else return defaultConfig
