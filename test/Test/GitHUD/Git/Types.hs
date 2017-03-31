module Test.GitHUD.Git.Types (
  gitTypesTests
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import GitHUD.Git.Types

gitTypesTests :: TestTree
gitTypesTests = testGroup "Git Types Test"
  [ testCase "Merging 2 repoChanges object should lead to the sum of its parts" $
      testMergeGitLocalRepoChanges
  ]

testMergeGitLocalRepoChanges :: Assertion
testMergeGitLocalRepoChanges =
  mergeGitLocalRepoChanges glrc1 glrc2 @?= glrcMerged

glrc1 :: GitLocalRepoChanges
glrc1 = GitLocalRepoChanges {
  localMod = 3
  , localAdd = 2
  , localDel = 4
  , indexMod = 6
  , indexAdd = 10
  , indexDel = 7
  , renamed  = 12
  , conflict = 50
}

glrc2 :: GitLocalRepoChanges
glrc2 = GitLocalRepoChanges {
  localMod = 6
  , localAdd = 20
  , localDel = 48
  , indexMod = 3
  , indexAdd = 56
  , indexDel = 10
  , renamed  = 44
  , conflict = 2
}

glrcMerged :: GitLocalRepoChanges
glrcMerged = GitLocalRepoChanges {
  localMod = 9
  , localAdd = 22
  , localDel = 52
  , indexMod = 9
  , indexAdd = 66
  , indexDel = 17
  , renamed  = 56
  , conflict = 52
}
