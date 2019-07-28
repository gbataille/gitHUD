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
import Network.Socket (Family(AF_UNIX), socket, defaultProtocol, SocketType(Stream), close, listen, accept, bind, SockAddr(SockAddrUnix), connect)
import Network.Socket.ByteString (recv, sendAll)
import System.Directory (removeFile)
import System.Posix.Daemon (isRunning, runDetached, Redirection(DevNull, ToFile))
import System.Posix.Files (fileExist)

import GitHUD.Config.Types
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
  forkIO $ socketClient socket pathToPoll
  forever $ fetcher delaySec socket pathToPoll

socketClient :: FilePath
             -> MVar String
             -> IO ()
socketClient socketPath mvar =
  fromSocket socketPath withMessage
    where
      withMessage msg = do
        putStrLn $ "callback with " ++ msg
        swapMVar mvar msg

fetcher :: Int
        -> FilePath
        -> MVar String
        -> IO ()
fetcher delaySec socketPath mvar = do
  path <- readMVar mvar
  putStrLn $ "fetching " ++ path
  threadDelay $ delaySec * 1_000_000
  return ()
