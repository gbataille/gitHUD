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
  mvCommitTag <- newEmptyMVar

  forkIO $ gitCmdLocalBranchName mvLocalBranch
  forkIO $ gitCmdPorcelainStatus mvGitStatus
  forkIO $ gitCmdStashCount mvStashCount
  forkIO $ gitCmdCommitShortSHA mvCommitShortSHA
  forkIO $ gitCmdCommitTag mvCommitTag

  localBranchName <- removeEndingNewline <$> (takeMVar mvLocalBranch)
  forkIO $ gitCmdRemoteName localBranchName mvRemoteName

  remoteName <- removeEndingNewline <$> takeMVar mvRemoteName
  repoState <- gitParseStatus <$> takeMVar mvGitStatus
  stashCountStr <- takeMVar mvStashCount
  commitShortSHA <- removeEndingNewline <$> takeMVar mvCommitShortSHA
  commitTag <- removeEndingNewline <$> takeMVar mvCommitTag

  fillGitRemoteRepoState zeroGitRepoState {
    gitLocalRepoChanges = repoState
    , gitRemote = remoteName
    , gitLocalBranch = localBranchName
    , gitCommitShortSHA = commitShortSHA
    , gitCommitTag = commitTag
    , gitStashCount = (getCount stashCountStr)
  }

fillGitRemoteRepoState :: GitRepoState
                       -> IO GitRepoState
fillGitRemoteRepoState repoState@( GitRepoState { gitRemote = ""} ) = return repoState
fillGitRemoteRepoState repoState = do
  mvRemoteBranchName <- newEmptyMVar
  mvMergeBase <- newEmptyMVar
  mvCommitsToPull <- newEmptyMVar
  mvCommitsToPush <- newEmptyMVar

  forkIO $ gitCmdRemoteBranchName (gitLocalBranch repoState) mvRemoteBranchName
  forkIO $ gitCmdMergeBase (gitLocalBranch repoState) mvMergeBase
  remoteBranch <- removeEndingNewline <$> (takeMVar mvRemoteBranchName)
  mergeBase <- removeEndingNewline <$> (takeMVar mvMergeBase)

  let fullRemoteBranchName = buildFullyQualifiedRemoteBranchName (gitRemote repoState) remoteBranch

  -- TODO: gbataille - Check for merge-base. If none, don't execute remote part
  forkIO $ gitCmdRevToPush fullRemoteBranchName "HEAD" mvCommitsToPush
  forkIO $ gitCmdRevToPull fullRemoteBranchName "HEAD" mvCommitsToPull

  (mergeBranchToPull, mergeBranchToPush) <- getRemoteMasterMergeState mergeBase fullRemoteBranchName

  commitsToPushStr <- takeMVar mvCommitsToPush
  let commitsToPush = getCount commitsToPushStr
  commitsToPullStr <- takeMVar mvCommitsToPull
  let commitsToPull = getCount commitsToPullStr

  return repoState {
    gitRemoteTrackingBranch = remoteBranch
    , gitCommitsToPull = commitsToPull
    , gitCommitsToPush = commitsToPush
    , gitMergeBranchCommitsToPull = mergeBranchToPull
    , gitMergeBranchCommitsToPush = mergeBranchToPush
  }

getRemoteMasterMergeState :: String     -- ^ the merge base
                          -> String     -- ^ the fully qualified remote branch name
                          -> IO (Int, Int)    -- ^ tuple containing (to pull, to push)
getRemoteMasterMergeState "" _ = return (0, 0)
getRemoteMasterMergeState _ fullRemoteBranchName = do
  mvMergeBranchCommitsToPull <- newEmptyMVar
  mvMergeBranchCommitsToPush <- newEmptyMVar

  forkIO $ gitCmdRevToPush "origin/master" fullRemoteBranchName mvMergeBranchCommitsToPush
  forkIO $ gitCmdRevToPull "origin/master" fullRemoteBranchName mvMergeBranchCommitsToPull

  mergeBranchCommitsToRMasterStr <- takeMVar mvMergeBranchCommitsToPull
  let mergeBranchCommitsToRMaster = getCount mergeBranchCommitsToRMasterStr
  mergeBranchCommitsToMergeStr <- takeMVar mvMergeBranchCommitsToPush
  let mergeBranchCommitsToMerge = getCount mergeBranchCommitsToMergeStr

  return (mergeBranchCommitsToRMaster, mergeBranchCommitsToMerge)
