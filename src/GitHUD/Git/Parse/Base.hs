module GitHUD.Git.Parse.Base (
  getGitRepoState
  ) where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar)

import GitHUD.Git.Types
import GitHUD.Git.Command
import GitHUD.Git.Parse.Status
import GitHUD.Git.Parse.Branch
import GitHUD.Git.Parse.Count

removeEndingNewline :: String -> String
removeEndingNewline str = concat . lines $ str

getGitRepoState :: IO GitRepoState
getGitRepoState = do
  -- Preparing MVars
  mvLocalBranch <- newEmptyMVar
  mvGitStatus <- newEmptyMVar
  mvRemoteName <- newEmptyMVar
  mvStashCount <- newEmptyMVar
  mvCommitShortSHA <- newEmptyMVar

  forkIO $ gitCmdLocalBranchName mvLocalBranch
  forkIO $ gitCmdPorcelainStatus mvGitStatus
  forkIO $ gitCmdStashCount mvStashCount
  forkIO $ gitCmdCommitShortSHA mvCommitShortSHA

  localBranchName <- removeEndingNewline <$> (takeMVar mvLocalBranch)
  forkIO $ gitCmdRemoteName localBranchName mvRemoteName

  remoteName <- removeEndingNewline <$> takeMVar mvRemoteName
  repoState <- gitParseStatus <$> takeMVar mvGitStatus
  stashCountStr <- takeMVar mvStashCount
  commitShortSHA <- removeEndingNewline <$> takeMVar mvCommitShortSHA

  fillGitRemoteRepoState zeroGitRepoState {
    gitLocalRepoChanges = repoState
    , gitRemote = remoteName
    , gitLocalBranch = localBranchName
    , gitCommitShortSHA = commitShortSHA
    , gitStashCount = (getCount stashCountStr)
  }

fillGitRemoteRepoState :: GitRepoState
                       -> IO GitRepoState
fillGitRemoteRepoState repoState@( GitRepoState { gitRemote = ""} ) = return repoState
fillGitRemoteRepoState repoState = do
  mvRemoteBranchName <- newEmptyMVar
  mvCommitsToPull <- newEmptyMVar
  mvCommitsToPush <- newEmptyMVar
  mvMergeBranchCommitsToPull <- newEmptyMVar
  mvMergeBranchCommitsToPush <- newEmptyMVar

  forkIO $ gitCmdRemoteBranchName (gitLocalBranch repoState) mvRemoteBranchName
  remoteBranch <- removeEndingNewline <$> (takeMVar mvRemoteBranchName)

  let fullRemoteBranchName = buildFullyQualifiedRemoteBranchName (gitRemote repoState) remoteBranch

  forkIO $ gitCmdRevToPush "origin/master" fullRemoteBranchName mvMergeBranchCommitsToPush
  forkIO $ gitCmdRevToPull "origin/master" fullRemoteBranchName mvMergeBranchCommitsToPull
  forkIO $ gitCmdRevToPush fullRemoteBranchName "HEAD" mvCommitsToPush
  forkIO $ gitCmdRevToPull fullRemoteBranchName "HEAD" mvCommitsToPull

  mergeBranchCommitsToMergeStr <- takeMVar mvMergeBranchCommitsToPush
  let mergeBranchCommitsToMerge = getCount mergeBranchCommitsToMergeStr
  mergeBranchCommitsToRMasterStr <- takeMVar mvMergeBranchCommitsToPull
  let mergeBranchCommitsToRMaster = getCount mergeBranchCommitsToRMasterStr

  commitsToPushStr <- takeMVar mvCommitsToPush
  let commitsToPush = getCount commitsToPushStr
  commitsToPullStr <- takeMVar mvCommitsToPull
  let commitsToPull = getCount commitsToPullStr

  return repoState {
    gitRemoteTrackingBranch = remoteBranch
    , gitCommitsToPull = commitsToPull
    , gitCommitsToPush = commitsToPush
    , gitMergeBranchCommitsToPull = mergeBranchCommitsToRMaster
    , gitMergeBranchCommitsToPush = mergeBranchCommitsToMerge
  }
