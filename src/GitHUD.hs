{-# LANGUAGE OverloadedStrings #-}

module GitHUD (
    githud,
    githudd
    ) where

import Control.Concurrent.Delay (delaySeconds)
import Control.Concurrent.MVar (MVar, tryReadMVar, newMVar)
import Control.Monad (when, forever)
import Control.Monad.Reader (runReader)
import Control.Monad.State (StateT, evalStateT, get, lift)
import Control.Monad.Trans (liftIO)
import Data.Default ( def )
import Data.Maybe (fromMaybe)
import Data.Text
import System.Daemon (ensureDaemonRunning, runClient)
import System.Environment (getArgs)
import System.IO (hClose, hPutStrLn, openFile, IOMode(WriteMode))
import System.Posix.Daemon (isRunning, runDetached, Redirection(ToFile))
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
    putStrLn $ unpack (strip (pack prompt))

processArguments :: IO [String]
                 -> IO Shell
processArguments args = do
  arguments <- args
  return $ getShell arguments

getShell :: [String]
         -> Shell
getShell ("zsh":_) = ZSH
getShell ("bash":_) = BASH
getShell ("tmux":_) = TMUX
getShell ("none":_) = NONE
getShell _ = Other

getAppConfig :: IO Config
getAppConfig = do
  userEntry <- getRealUserID >>= getUserEntryForID
  let configFilePath = (homeDirectory userEntry) ++ "/.githudrc"
  configFilePresent <- fileExist configFilePath
  if configFilePresent
    then parseConfigFile configFilePath
    else return defaultConfig

githudd :: IO()
githudd = do
  mArg <- processDaemonArguments <$> getArgs
  config <- getAppConfig
  let pidFilePath = "/tmp/githudd.pid"
  let socketFile = "/tmp/githudd.sock"
  running <- isRunning pidFilePath
  when (not running) $
    runDetached (Just pidFilePath) (ToFile "/tmp/subprocess.out") (daemon (fromMaybe "default" mArg) socketFile)

processDaemonArguments :: [String]
                       -> Maybe String
processDaemonArguments [] = Nothing
processDaemonArguments (fst:_) = Just fst

daemon :: FilePath
       -> FilePath
       -> IO ()
daemon path socket = forever $ fetcher path

fetcher :: FilePath
        -> IO ()
fetcher path = do
  putStrLn $ "fetching state " ++ path
  delaySeconds 5
  return ()
