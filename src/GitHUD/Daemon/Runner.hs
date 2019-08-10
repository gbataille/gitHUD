{-# LANGUAGE NumericUnderscores #-}
module GitHUD.Daemon.Runner (
    runDaemon
    ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, readMVar, newMVar, swapMVar)
import qualified Control.Exception as E
import Control.Monad (when, forever, void, unless)
import qualified Data.ByteString.UTF8 as BSU
import Data.Maybe (fromMaybe)
import System.Directory (removeFile)
import System.Posix.Daemon (isRunning, runDetached, Redirection(DevNull, ToFile))
import System.Posix.Files (fileExist)

import GitHUD.Config.Types
import GitHUD.Git.Command
import GitHUD.Daemon.Network

runDaemon :: Config
          -> Maybe String
          -> IO ()
runDaemon config mArg = do
  let pathToPoll = (fromMaybe "/" mArg)
  ensureDaemonRunning config socketFile pathToPoll
  void $ sendOnSocket socketFile pathToPoll
  where
    socketFile = confGithuddSocketFilePath config

ensureDaemonRunning :: Config
                    -> FilePath
                    -> FilePath
                    -> IO ()
ensureDaemonRunning config socketPath pathToPoll = do
  running <- isRunning pidFilePath
  unless running $ do
    socketExists <- fileExist socketPath
    when socketExists (removeFile socketPath)
    removeLogFile stdoutFile
    runDetached (Just pidFilePath) stdoutFile (daemon delaySec pathToPoll socketPath)
  where
    stdoutFile = confGithuddLogFilePath config
    pidFilePath = confGithuddPidFilePath config
    delaySec = confGithuddSleepSeconds config

removeLogFile :: Redirection
              -> IO ()
removeLogFile DevNull = return ()
removeLogFile (ToFile file) = do
    debugFileExists <- fileExist file
    when debugFileExists (removeFile file)

daemon :: Int
       -> FilePath
       -> FilePath
       -> IO ()
daemon delaySec path socket = do
  pathToPoll <- newMVar path
  forkIO $ socketServer socket pathToPoll
  forever $ fetcher delaySec pathToPoll

socketServer :: FilePath
             -> MVar String
             -> IO ()
socketServer socketPath mvar =
  fromSocket socketPath withMessage
    where
      withMessage msg = do
        putStrLn $ "Switching to poll " ++ msg
        swapMVar mvar msg

fetcher :: Int
        -> MVar String
        -> IO ()
fetcher delaySec mvar = do
  path <- readMVar mvar
  gitCmdFetch path
  threadDelay $ delaySec * 1_000_000
  return ()
