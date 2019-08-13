{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
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
import System.Exit (exitFailure)
import System.Posix.Daemon (brutalKill, isRunning, runDetached, Redirection(DevNull, ToFile))
import System.Posix.Files (fileExist)

import GitHUD.Config.Types
import GitHUD.Daemon.Network
import GitHUD.Debug
import GitHUD.Git.Command

runDaemon :: Config
          -> Maybe String
          -> IO ()
runDaemon = tryRunDaemon 5

tryRunDaemon :: Int
             -> Config
             -> Maybe String
             -> IO ()
tryRunDaemon attempt config mArg = do
  let pathToPoll = (fromMaybe "/" mArg)
  -- If there are exception trying to access the pid file or the socket,
  -- we just kill the process and start again
  success <- E.try @E.SomeException $ do
    ensureDaemonRunning config socketFile pathToPoll
    sendOnSocket socketFile pathToPoll
  restartIfNeeded attempt success
  where
    socketFile = confGithuddSocketFilePath config
    pidFilePath = confGithuddPidFilePath config
    restartIfNeeded 0 _ = void exitFailure
    restartIfNeeded _ (Right _) = return ()
    restartIfNeeded attempt (Left e) = do
      debugOnStderr $ show e
      debugOnStderr "Error on client. Restarting daemon"
      E.try @E.SomeException (brutalKill pidFilePath)   -- ignore possible errors
      threadDelay 100_000
      pidFileExists <- fileExist pidFilePath
      when pidFileExists (removeFile pidFilePath)
      tryRunDaemon (attempt - 1) config mArg

ensureDaemonRunning :: Config
                    -> FilePath
                    -> FilePath
                    -> IO ()
ensureDaemonRunning config socketPath pathToPoll = do
  running <- isRunning pidFilePath
  unless running $ do
    removeLogFile stdoutFile
    runDetached (Just pidFilePath) stdoutFile (daemon delaySec pathToPoll socketPath)
    threadDelay 100_000     -- Give the daemon some time to start
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
socketServer socketPath mvar = do
  success <- E.try @E.SomeException (receiveOnSocket socketPath withMessage)
  restartIfNeeded success
    where
      withMessage = swapMVar mvar
      restartIfNeeded (Right _) = return ()
      restartIfNeeded (Left e) = do
        debug "Error on server. Restarting socket"
        debug $ show e
        threadDelay 100_000
        socketServer socketPath mvar

fetcher :: Int
        -> MVar String
        -> IO ()
fetcher delaySec mvar = do
  path <- readMVar mvar
  gitCmdFetch path
  threadDelay $ delaySec * 1_000_000
  return ()
