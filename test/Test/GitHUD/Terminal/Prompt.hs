module Test.GitHUD.Terminal.Prompt (
  terminalPromptTests
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad.Reader (runReader)
import Control.Monad.Writer (runWriterT)

import GitHUD.Git.Types
import GitHUD.Terminal.Base
import GitHUD.Terminal.Prompt
import GitHUD.Terminal.Types

terminalPromptTests :: TestTree
terminalPromptTests = testGroup "Terminal Prompt Test"
  [ testAddGitRepoIndicator
    , testAddUpstreamIndicator
    , testAddRemoteCommits
  ]

testAddGitRepoIndicator :: TestTree
testAddGitRepoIndicator = testGroup "#addGitRepoIndicator"
  [ testCase "#addGitRepoIndicator hardcoded character" $
      testWriterWithConfig (zeroOutputConfig ZSH) addGitRepoIndicator @?= "\57504 "
  ]

testAddUpstreamIndicator :: TestTree
testAddUpstreamIndicator = testGroup "#addUpstreamIndicator"
  [ testCase "#addUpstreamIndicator with an upstream" $
      testWriterWithConfig
        (buildOutputConfig ZSH (zeroGitRepoState { gitRemoteTrackingBranch = "foo" }))
        addUpstreamIndicator
    @?= ""

  , testCase "#addUpstreamIndicator with no upstream for non ZSH shell" $
      testWriterWithConfig
        (zeroOutputConfig ZSH) addUpstreamIndicator
    @?= "upstream %{\x1b[1;31m%}\9889%{\x1b[0m%} "

  , testCase "#addUpstreamIndicator with no upstream for ZSH" $
      testWriterWithConfig
        (zeroOutputConfig Other) addUpstreamIndicator
    @?= "upstream \x1b[1;31m\9889\x1b[0m "
  ]

testAddRemoteCommits :: TestTree
testAddRemoteCommits = testGroup "#addRemoteCommits"
  [ testCase "#addRemoteCommits ZSH commits to pull" $
      testRemoteCommitsToPull ZSH @?=
      "\120366 %{\ESC[1;32m%}\8594 %{\ESC[0m%}2 "
  , testCase "#addRemoteCommits ZSH commits to push" $
      testRemoteCommitsToPush ZSH @?=
      "\120366 %{\ESC[1;32m%}\8592 %{\ESC[0m%}2 "
  , testCase "#addRemoteCommits ZSH commits to pull and to push" $
      testRemoteCommitsToPushAndPull ZSH @?=
      "\120366 4%{\ESC[1;32m%}\8644%{\ESC[0m%}4 "
  , testCase "#addRemoteCommits Other commits to pull" $
      testRemoteCommitsToPull Other @?=
      "\120366 \ESC[1;32m\8594 \ESC[0m2 "
  , testCase "#addRemoteCommits Other commits to push" $
      testRemoteCommitsToPush Other @?=
      "\120366 \ESC[1;32m\8592 \ESC[0m2 "
  , testCase "#addRemoteCommits Other commits to pull and to push" $
      testRemoteCommitsToPushAndPull Other @?=
      "\120366 4\ESC[1;32m\8644\ESC[0m4 "
  ]

-- | Utility function to test a ShellOutput function and gets the prompt built
testWriterWithConfig :: OutputConfig    -- ^ Starting reader state
                     -> ShellOutput     -- ^ Function under test
                     -> String               -- ^ Output of the function for the given config
testWriterWithConfig config functionUnderTest =
  runReader (runWriterT functionUnderTest >>= (\(_, out) -> return out)) config

zeroOutputConfig :: Shell
                 -> OutputConfig
zeroOutputConfig shell = buildOutputConfig shell zeroGitRepoState

testRemoteCommitsToPull :: Shell -> String
testRemoteCommitsToPull shell = testWriterWithConfig
  (buildOutputConfig shell (zeroGitRepoState { gitRemoteCommitsToPull = 2 }))
  addRemoteCommits

testRemoteCommitsToPush :: Shell -> String
testRemoteCommitsToPush shell = testWriterWithConfig
  (buildOutputConfig shell (zeroGitRepoState { gitRemoteCommitsToPush = 2 }))
  addRemoteCommits

testRemoteCommitsToPushAndPull :: Shell -> String
testRemoteCommitsToPushAndPull shell = testWriterWithConfig
  (buildOutputConfig shell
    (zeroGitRepoState { gitRemoteCommitsToPull = 4, gitRemoteCommitsToPush = 4 })
  )
  addRemoteCommits
