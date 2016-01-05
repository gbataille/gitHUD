module GitHUD.Git.Types (
  GitLocalRepoChanges(..)
  , zeroLocalRepoChanges
  , GitRepoState(..)
  , zeroGitRepoState
  , mergeGitLocalRepoChanges
  ) where

data GitLocalRepoChanges = GitLocalRepoChanges { localMod :: Int
                                 , localAdd :: Int
                                 , localDel :: Int
                                 , indexMod :: Int
                                 , indexAdd :: Int
                                 , indexDel :: Int
                                 , renamed  :: Int
                                 , conflict :: Int
                                 } deriving (Show, Eq)

zeroLocalRepoChanges :: GitLocalRepoChanges
zeroLocalRepoChanges = GitLocalRepoChanges { localMod = 0
                             , localAdd = 0
                             , localDel = 0
                             , indexMod = 0
                             , indexAdd = 0
                             , indexDel = 0
                             , renamed  = 0
                             , conflict = 0
                             }

mergeGitLocalRepoChanges :: GitLocalRepoChanges -> GitLocalRepoChanges -> GitLocalRepoChanges
mergeGitLocalRepoChanges a b =
  GitLocalRepoChanges {
    localMod   = (localMod a) + (localMod b)
    , localAdd = (localAdd a) + (localAdd b)
    , localDel = (localDel a) + (localDel b)
    , indexMod = (indexMod a) + (indexMod b)
    , indexAdd = (indexAdd a) + (indexAdd b)
    , indexDel = (indexDel a) + (indexDel b)
    , renamed  = (renamed a) + (renamed b)
    , conflict = (conflict a) + (conflict b)
  }

data GitRepoState =
  GitRepoState {
    gitLocalRepoChanges :: GitLocalRepoChanges
    , gitLocalBranch :: String
    , gitCommitShortSHA :: String
    , gitRemote :: String
    , gitRemoteTrackingBranch :: String
    , gitStashCount :: Int
    , gitCommitsToPull :: Int
    , gitCommitsToPush :: Int
    , gitMergeBranchCommitsToPull :: Int
    , gitMergeBranchCommitsToPush :: Int
  }
  deriving (Eq, Show)

zeroGitRepoState :: GitRepoState
zeroGitRepoState =
  GitRepoState {
    gitLocalRepoChanges = zeroLocalRepoChanges
    , gitLocalBranch = ""
    , gitCommitShortSHA = ""
    , gitRemote = ""
    , gitRemoteTrackingBranch = ""
    , gitStashCount = 0
    , gitCommitsToPull = 0
    , gitCommitsToPush = 0
    , gitMergeBranchCommitsToPull = 0
    , gitMergeBranchCommitsToPush = 0
  }
