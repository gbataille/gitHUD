module Test.GitHUD.Git.Parse.Branch (
  branchTests
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import GitHUD.Git.Parse.Branch

branchTests :: TestTree
branchTests = testGroup "Branch Parser Test"
  [ testCase "remote branch name" $
      buildFullyQualifiedRemoteBranchName "bar" "refs/heads/foo"
      @?= "bar/foo"
  ]
