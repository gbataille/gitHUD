{-# LANGUAGE OverloadedStrings #-}

module GitHUD
  ( githud,
    githudd,
  )
where

import Control.Monad (unless, void, when)
import Control.Monad.Reader (runReader)
import Data.Text
import GitHUD.Config.Parse
import GitHUD.Config.Types
import GitHUD.Daemon.Runner
import GitHUD.Git.Command
import GitHUD.Git.Parse.Base
import GitHUD.Terminal.Prompt
import GitHUD.Terminal.Types
import GitHUD.Types
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitSuccess))
import System.FileLock (SharedExclusive (Exclusive), withTryFileLock)
import System.Posix.Files (fileExist)
import System.Posix.User (UserEntry (..), getRealUserID, getUserEntryForID)
import System.Process (readProcessWithExitCode)

githud :: IO ()
githud = do
  -- Exit ASAP if we are not in a git repository
  isGit <- checkInGitDirectory
  when isGit $ do
    shell <- processArguments getArgs
    config <- getAppConfig
    curDir <- getCurrentDirectory
    when (confRunFetcherDaemon config) $
      tryRunFetcherDaemon curDir (confGithuddLockFilePath config)
    repoState <- getGitRepoState
    let prompt = runReader buildPromptWithConfig $ buildOutputConfig shell repoState config
    -- Necessary to use putStrLn to properly terminate the output (needs the CR)
    putStrLn $ unpack (strip (pack prompt))

tryRunFetcherDaemon ::
  String ->
  FilePath ->
  IO ()
tryRunFetcherDaemon dir lockPath = do
  withTryFileLock lockPath Exclusive (\f -> runFetcherDaemon dir)
  return ()
  where
    runFetcherDaemon dir = do
      (code, out, err) <- readProcessWithExitCode "githudd" [dir] ""
      unless (Prelude.null err) (putStrLn $ "Issue with githudd: " ++ err)

processArguments ::
  IO [String] ->
  IO Shell
processArguments args = getShell <$> args

getShell ::
  [String] ->
  Shell
getShell ("zsh" : _) = ZSH
getShell ("bash" : _) = BASH
getShell ("tmux" : _) = TMUX
getShell ("none" : _) = NONE
getShell _ = Other

getAppConfig :: IO Config
getAppConfig = do
  userEntry <- getRealUserID >>= getUserEntryForID
  let configFilePath = (homeDirectory userEntry) ++ "/.githudrc"
  configFilePresent <- fileExist configFilePath
  if configFilePresent
    then parseConfigFile configFilePath
    else return defaultConfig

githudd :: IO ()
githudd = do
  mArg <- processDaemonArguments <$> getArgs
  config <- getAppConfig
  runDaemon config mArg

processDaemonArguments ::
  [String] ->
  Maybe String
processDaemonArguments [] = Nothing
processDaemonArguments (fst : _) = Just fst
