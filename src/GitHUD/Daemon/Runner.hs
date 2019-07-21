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
import System.Posix.Daemon (isRunning, runDetached, Redirection(ToFile))
import System.Posix.Files (fileExist)

import GitHUD.Config.Types
import GitHUD.Daemon.Network

runDaemon :: Config
          -> Maybe String
          -> IO ()
runDaemon config mArg = do
  let pathToPoll = (fromMaybe "default" mArg)
  ensureDaemonRunning socketFile pathToPoll
  void $ sendOnSocket socketFile pathToPoll
  where
    socketFile = "/tmp/githudd.sock"

ensureDaemonRunning :: FilePath
                    -> FilePath
                    -> IO ()
ensureDaemonRunning socketPath pathToPoll = do
  running <- isRunning pidFilePath
  unless running $ do
    socketExists <- fileExist socketPath
    when socketExists (removeFile socketPath)
    runDetached (Just pidFilePath) (ToFile stdoutFile) (daemon pathToPoll socketPath)
  where
    stdoutFile = "/tmp/subprocess.out"
    pidFilePath = "/tmp/githudd.pid"

daemon :: FilePath
       -> FilePath
       -> IO ()
daemon path socket = do
  pathToPoll <- newMVar path
  forkIO $ socketClient socket pathToPoll
  forever $ fetcher socket pathToPoll

socketClient :: FilePath
             -> MVar String
             -> IO ()
socketClient socketPath mvar =
  fromSocket socketPath withMessage
    where
      withMessage msg = do
        putStrLn $ "callback with " ++ msg
        swapMVar mvar msg

fetcher :: FilePath
        -> MVar String
        -> IO ()
fetcher socketPath mvar = do
  path <- readMVar mvar
  putStrLn $ "fetching " ++ path
  threadDelay 5_000_000
  return ()
