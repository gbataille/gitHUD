module GitHUD.Git.Types (
  zeroLocalRepoChanges
  , GitLocalRepoChanges(..)
  ) where

data GitLocalRepoChanges = GitLocalRepoChanges { localMod :: Int
                                 , localAdd :: Int
                                 , localDel :: Int
                                 , indexMod :: Int
                                 , indexAdd :: Int
                                 , indexDel :: Int
                                 , conflict :: Int
                                 } deriving (Show, Eq)

zeroLocalRepoChanges :: GitLocalRepoChanges
zeroLocalRepoChanges = GitLocalRepoChanges { localMod = 0
                             , localAdd = 0
                             , localDel = 0
                             , indexMod = 0
                             , indexAdd = 0
                             , indexDel = 0
                             , conflict = 0
                             }

