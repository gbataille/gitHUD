module Test.GitHUD.Git.Common (
  gitCommonTests
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import GitHUD.Git.Common

gitCommonTests :: TestTree
gitCommonTests = testGroup "Git Common Test"
  [ testCase "Getting the config key for the remote of a given local branch" $
      gitRemoteTrackingConfigKey "master" @?= "branch.master.remote"

    , testCase "Getting the config key for the remote tracking branch of a local branch" $
      gitRemoteBranchConfigKey "master" @?= "branch.master.merge"

    , testCase "Getting a commit span between 2 commits" $
      mergeBaseDiffFromTo "b2d35" "ef25d" @?= "b2d35...ef25d"
  ]
