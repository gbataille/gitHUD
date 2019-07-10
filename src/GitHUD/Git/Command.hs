module GitHUD.Git.Command (
  gitCmdLocalBranchName
  , gitCmdMergeBase
  , gitCmdRemoteName
  , gitCmdRemoteBranchName
  , gitCmdPorcelainStatus
  , gitCmdRevToPush
  , gitCmdRevToPull
  , gitCmdStashCount
  , gitCmdCommitShortSHA
  , gitCmdCommitTag
  , gitCmdFetch
  , checkInGitDirectory
  ) where

import Control.Concurrent.MVar (MVar, putMVar)
import GHC.IO.Handle (hGetLine)
import System.Directory (doesDirectoryExist)
import System.Exit (ExitCode(ExitSuccess))
import System.IO (hClose, hPutStrLn, openFile, IOMode(WriteMode))
import System.Process (readCreateProcess, readProcessWithExitCode, proc, StdStream(CreatePipe, UseHandle), createProcess, CreateProcess(..))

import GitHUD.Process (readProcessWithIgnoreExitCode)
import GitHUD.Git.Common

checkInGitDirectory :: IO Bool
checkInGitDirectory = do
  (exCode, _, _) <- readProcessWithExitCode "git" ["rev-parse", "--git-dir"] ""
  return (exCode == ExitSuccess)

gitCmdLocalBranchName :: MVar String -> IO ()
gitCmdLocalBranchName out = do
  localBranch <- readProcessWithIgnoreExitCode "git" ["symbolic-ref", "--short", "HEAD"] ""
  putMVar out localBranch

gitCmdMergeBase :: String     -- ^ local branch name
                -> MVar String    -- ^ output Mvar
                -> IO ()
gitCmdMergeBase localBranchName out = do
  mergeBase <- readProcessWithIgnoreExitCode "git" ["merge-base", "origin/master", localBranchName] ""
  putMVar out mergeBase

gitCmdRemoteName :: String         -- ^ local branch name
              -> MVar String   -- ^ the output mvar
              -> IO ()
gitCmdRemoteName localBranchName out = do
  remoteName <- readProcessWithIgnoreExitCode "git" ["config", "--get", gitRemoteTrackingConfigKey localBranchName] ""
  putMVar out remoteName

gitCmdRemoteBranchName :: String     -- ^ remote name
                    -> MVar String     -- ^ The output mvar
                    -> IO ()
gitCmdRemoteBranchName remoteName out = do
  remoteBranch <- readProcessWithIgnoreExitCode "git" ["config", "--get", gitRemoteBranchConfigKey remoteName] ""
  putMVar out remoteBranch


gitCmdPorcelainStatus :: MVar String -> IO ()
gitCmdPorcelainStatus out = do
  porcelainStatus <- readProcessWithIgnoreExitCode "git" ["status", "--porcelain"] ""
  putMVar out porcelainStatus

gitCmdRevToPush :: String          -- ^ from revision
             -> String          -- ^ to revision
             -> MVar String      -- ^ The output mvar
             -> IO ()
gitCmdRevToPush fromCommit toCommit out = do
  revToPush <- readProcessWithIgnoreExitCode "git" ["rev-list", "--no-merges", "--right-only", "--count", mergeBaseDiffFromTo fromCommit toCommit] ""
  putMVar out revToPush

gitCmdRevToPull :: String          -- ^ from revision
             -> String          -- ^ to revision
             -> MVar String      -- ^ The output mvar
             -> IO ()
gitCmdRevToPull fromCommit toCommit out = do
  revToPull <- readProcessWithIgnoreExitCode "git" ["rev-list", "--no-merges", "--left-only", "--count", mergeBaseDiffFromTo fromCommit toCommit] ""
  putMVar out revToPull

gitCmdStashCount :: MVar String     -- ^ The output mvar
              -> IO ()
gitCmdStashCount out = do
  ( _, Just hGitStashList, _, _) <- createProcess
    (proc "git" ["stash", "list"])
    { std_out = CreatePipe }
  ( _, Just hCountStr, _, _) <- createProcess
    (proc "wc" ["-l"])
    { std_in = UseHandle hGitStashList, std_out = CreatePipe }
  count <- hGetLine hCountStr
  putMVar out count

gitCmdCommitShortSHA :: MVar String
                  -> IO ()
gitCmdCommitShortSHA out = do
  shortSHA <- readProcessWithIgnoreExitCode "git" ["rev-parse", "--short", "HEAD"] ""
  putMVar out shortSHA

gitCmdCommitTag :: MVar String
                -> IO ()
gitCmdCommitTag out = do
  tag <- readProcessWithIgnoreExitCode "git" ["describe", "--exact-match", "--tags"] ""
  putMVar out tag

gitCmdFetch :: String
            -> IO ()
gitCmdFetch path = do
  -- TODO: gbataille - code org
  isDir <- doesDirectoryExist path
  stdout <- openFile "/tmp/out" WriteMode
  stderr <- openFile "/tmp/err" WriteMode
  if isDir
    then do
      let fetch_proc = (proc "git" ["fetch"]) { cwd = Just path, std_out = UseHandle stdout, std_err = UseHandle stderr }
      readCreateProcess fetch_proc ""
      return ()
    else do
      hPutStrLn stderr ("Folder" ++ path ++ " does not exist")
      return ()
  hClose stdout
  hClose stderr
