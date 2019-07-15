{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}

module GitHUD (
    githud,
    githudd
    ) where

import Control.Concurrent (forkFinally, forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, readMVar, newMVar, swapMVar)
import qualified Control.Exception as E
import Control.Monad (when, forever, void, unless)
import Control.Monad.Reader (runReader)
import qualified Data.ByteString as S
import qualified Data.ByteString.UTF8 as BSU
import Data.Maybe (fromMaybe)
import Data.Text
import Network.Socket (Family(AF_UNIX), socket, defaultProtocol, SocketType(Stream), close, listen, accept, bind, SockAddr(SockAddrUnix), connect)
import Network.Socket.ByteString (recv, sendAll)
import System.Directory (removeFile)
import System.Environment (getArgs)
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
  running <- isRunning pidFilePath
  when (not running) $ do
    socketExists <- fileExist socketFile
    when socketExists (removeFile socketFile)
    runDetached (Just pidFilePath) (ToFile "/tmp/subprocess.out") (daemon (fromMaybe "default" mArg) socketFile)
  E.bracket open mClose mTalk
  where
    pidFilePath = "/tmp/githudd.pid"
    socketFile = "/tmp/githudd.sock"
    open = do
        socketExists <- fileExist socketFile
        if socketExists
          then do
            putStrLn "Opening client socket"
            sock <- socket AF_UNIX Stream defaultProtocol
            connect sock (SockAddrUnix socketFile)
            return $ Just sock
          else return Nothing
    mClose = maybe (return ()) close
    mTalk = maybe (return ()) talk
    talk sock = do
      mArg <- processDaemonArguments <$> getArgs
      putStrLn "Sending on client socket"
      sendAll sock $ BSU.fromString (fromMaybe "default" mArg)

processDaemonArguments :: [String]
                       -> Maybe String
processDaemonArguments [] = Nothing
processDaemonArguments (fst:_) = Just fst

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
socketClient socketPath mvar = E.bracket open close loop
  where
    open = do
        putStrLn "Opening server socket"
        sock <- socket AF_UNIX Stream defaultProtocol
        bind sock (SockAddrUnix socketPath)
        listen sock 1
        return sock
    loop sock = forever $ do
        (conn, peer) <- accept sock
        void $ forkFinally (talk conn) (\_ -> close conn)
    talk conn = do
        msg <- recv conn 1024
        unless (S.null msg) $ do
          swapMVar mvar $ BSU.toString msg
          talk conn

fetcher :: FilePath
        -> MVar String
        -> IO ()
fetcher socketPath mvar = do
  path <- readMVar mvar
  putStrLn $ "fetching " ++ path
  threadDelay 5_000_000
  return ()
