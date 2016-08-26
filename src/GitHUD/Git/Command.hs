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
  , checkInGitDirectory
  ) where

import Control.Concurrent.MVar (MVar, putMVar)
import System.Process (readProcessWithExitCode, proc, StdStream(CreatePipe, UseHandle), createProcess, CreateProcess(..))
import GHC.IO.Handle (hGetLine)
import System.Exit (ExitCode(ExitSuccess))

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
