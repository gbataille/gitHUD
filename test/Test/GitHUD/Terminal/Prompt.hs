module Test.GitHUD.Terminal.Prompt (
  terminalPromptTests
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad.Reader (runReader)
import Control.Monad.Writer (runWriterT)

import GitHUD.Config.Types
import GitHUD.Git.Types
import GitHUD.Terminal.Prompt
import GitHUD.Terminal.Types

terminalPromptTests :: TestTree
terminalPromptTests = testGroup "Terminal Prompt Test"
  [ testAddGitRepoIndicator
    , testAddUpstreamIndicator
    , testAddRemoteCommits
    , testAddLocalBranchName
    , testAddLocalCommits
    , testAddRepoState
    , testAddStashes
  ]

testAddGitRepoIndicator :: TestTree
testAddGitRepoIndicator = testGroup "#addGitRepoIndicator"
  [ testCase "ZSH: default config: hardcoded character" $
      testWriterWithConfig (zeroOutputConfig ZSH) addGitRepoIndicator @?= "\57504 "

    , testCase "Other: default config: hardcoded character" $
      testWriterWithConfig (zeroOutputConfig Other) addGitRepoIndicator @?= "\57504 "

    , testCase "ZSH: custom config: hardcoded character" $
      testWriterWithConfig
        (buildOutputConfig ZSH zeroGitRepoState $ defaultConfig { confRepoIndicator = "indic" })
        addGitRepoIndicator
      @?= "indic "

    , testCase "Other: custom config: hardcoded character" $
      testWriterWithConfig
        (buildOutputConfig ZSH zeroGitRepoState $ defaultConfig { confRepoIndicator = "indic" })
        addGitRepoIndicator
      @?= "indic "
  ]

testAddUpstreamIndicator :: TestTree
testAddUpstreamIndicator = testGroup "#addUpstreamIndicator"
  [ testCase "ZSH: with an upstream" $
      testWriterWithConfig
        (buildOutputConfig ZSH (zeroGitRepoState { gitRemoteTrackingBranch = "foo" }) defaultConfig)
        addUpstreamIndicator
      @?= ""

    , testCase "Other: with an upstream" $
      testWriterWithConfig
        (buildOutputConfig Other (zeroGitRepoState { gitRemoteTrackingBranch = "foo" }) defaultConfig)
        addUpstreamIndicator
      @?= ""

  , testCase "ZSH: with no upstream" $
      testWriterWithConfig
        (zeroOutputConfig ZSH) addUpstreamIndicator
      @?= "upstream %{\x1b[1;31m%}\9889%{\x1b[0m%} "

  , testCase "Other: with no upstream" $
      testWriterWithConfig
        (zeroOutputConfig Other) addUpstreamIndicator
      @?= "upstream \x1b[1;31m\9889\x1b[0m "
  ]

testAddRemoteCommits :: TestTree
testAddRemoteCommits = testGroup "#addRemoteCommits"
  [ testCase "ZSH: commits to pull" $
      testRemoteCommitsToPull ZSH @?=
      "\120366 %{\x1b[1;32m%}\8594 %{\x1b[0m%}2 "

  , testCase "ZSH: commits to push" $
      testRemoteCommitsToPush ZSH @?=
      "\120366 %{\x1b[1;32m%}\8592 %{\x1b[0m%}2 "

  , testCase "ZSH: commits to pull and to push" $
      testRemoteCommitsToPushAndPull ZSH @?=
      "\120366 4%{\x1b[1;32m%}\8644%{\x1b[0m%}4 "

  , testCase "Other: commits to pull" $
      testRemoteCommitsToPull Other @?=
      "\120366 \x1b[1;32m\8594 \x1b[0m2 "

  , testCase "Other: commits to push" $
      testRemoteCommitsToPush Other @?=
      "\120366 \x1b[1;32m\8592 \x1b[0m2 "

  , testCase "Other: commits to pull and to push" $
      testRemoteCommitsToPushAndPull Other @?=
      "\120366 4\x1b[1;32m\8644\x1b[0m4 "
  ]

testAddLocalBranchName :: TestTree
testAddLocalBranchName = testGroup "#addLocalBranchName"
  [ testCase "ZSH: should display the name of the current branch if we are at the HEAD of any" $
      testWriterWithConfig
        (buildOutputConfig ZSH (zeroGitRepoState { gitLocalBranch = "foo" }) defaultConfig)
        addLocalBranchName
      @?= "[foo] "

    , testCase "Other: should display the name of the current branch if we are at the HEAD of any" $
      testWriterWithConfig
        (buildOutputConfig Other (zeroGitRepoState { gitLocalBranch = "foo" }) defaultConfig)
        addLocalBranchName
      @?= "[foo] "

    , testCase "ZSH: should display the current commit SHA if we are not on a branch's HEAD" $
      testWriterWithConfig
        (buildOutputConfig ZSH (zeroGitRepoState { gitCommitShortSHA = "3d25ef" }) defaultConfig)
        addLocalBranchName
      @?= "[%{\x1b[1;33m%}detached@3d25ef%{\x1b[0m%}] "

    , testCase "Other: should display the current commit SHA if we are not on a branch's HEAD" $
      testWriterWithConfig
        (buildOutputConfig Other (zeroGitRepoState { gitCommitShortSHA = "3d25ef" }) defaultConfig)
        addLocalBranchName
      @?= "[\x1b[1;33mdetached@3d25ef\x1b[0m] "
  ]

testAddLocalCommits :: TestTree
testAddLocalCommits = testGroup "#addLocalCommits"
  [ testCase "ZSH: commits to pull" $
      testCommitsToPull ZSH @?=
      "2%{\x1b[1;31m%}\8595 %{\x1b[0m%} "

  , testCase "ZSH: commits to push" $
      testCommitsToPush ZSH @?=
      "2%{\x1b[1;32m%}\8593%{\x1b[0m%} "

  , testCase "ZSH: commits to pull and to push" $
      testCommitsToPushAndPull ZSH @?=
      "4%{\x1b[1;32m%}\8645%{\x1b[0m%}4 "

  , testCase "Other: commits to pull" $
      testCommitsToPull Other @?=
      "2\x1b[1;31m\8595 \x1b[0m "

  , testCase "Other: commits to push" $
      testCommitsToPush Other @?=
      "2\x1b[1;32m\8593\x1b[0m "

  , testCase "Other: commits to pull and to push" $
      testCommitsToPushAndPull Other @?=
      "4\x1b[1;32m\8645\x1b[0m4 "
  ]

testAddRepoState :: TestTree
testAddRepoState = testGroup "#addRepoState"
  [ testCase "ZSH: with Local Add Changes" $
      testLocalAddChange ZSH @?= "2%{\x1b[1;37m%}A%{\x1b[0m%} "

    , testCase "ZSH: with Local Mod Changes" $
      testLocalModChange ZSH @?= "2%{\x1b[1;31m%}M%{\x1b[0m%} "

    , testCase "ZSH: with Local Del Changes" $
      testLocalDelChange ZSH @?= "2%{\x1b[1;31m%}D%{\x1b[0m%} "

    , testCase "ZSH: with Index Add Changes" $
      testIndexAddChange ZSH @?= "2%{\x1b[1;32m%}A%{\x1b[0m%} "

    , testCase "ZSH: with Index Mod Changes" $
      testIndexModChange ZSH @?= "2%{\x1b[1;32m%}M%{\x1b[0m%} "

    , testCase "ZSH: with Index Del Changes" $
      testIndexDelChange ZSH @?= "2%{\x1b[1;32m%}D%{\x1b[0m%} "

    , testCase "ZSH: with Conflicted Changes" $
      testConflictedChange ZSH @?= "2%{\x1b[1;32m%}C%{\x1b[0m%} "

    , testCase "ZSH: with Renamed Changes" $
      testRenamedChange ZSH @?= "2%{\x1b[1;32m%}R%{\x1b[0m%} "

    , testCase "Other: with Local Add Changes" $
      testLocalAddChange Other @?= "2\x1b[1;37mA\x1b[0m "

    , testCase "Other: with Local Mod Changes" $
      testLocalModChange Other @?= "2\x1b[1;31mM\x1b[0m "

    , testCase "Other: with Local Del Changes" $
      testLocalDelChange Other @?= "2\x1b[1;31mD\x1b[0m "

    , testCase "Other: with Index Add Changes" $
      testIndexAddChange Other @?= "2\x1b[1;32mA\x1b[0m "

    , testCase "Other: with Index Mod Changes" $
      testIndexModChange Other @?= "2\x1b[1;32mM\x1b[0m "

    , testCase "Other: with Index Del Changes" $
      testIndexDelChange Other @?= "2\x1b[1;32mD\x1b[0m "

    , testCase "Other: with Conflicted Changes" $
      testConflictedChange Other @?= "2\x1b[1;32mC\x1b[0m "

    , testCase "Other: with Renamed Changes" $
      testRenamedChange Other @?= "2\x1b[1;32mR\x1b[0m "

    , testCase "ZSH: with every kind of Changes" $
      testEveryRepoChange ZSH @?= "6%{\x1b[1;32m%}A%{\x1b[0m%}8%{\x1b[1;32m%}D%{\x1b[0m%}7%{\x1b[1;32m%}M%{\x1b[0m%}1%{\x1b[1;32m%}R%{\x1b[0m%} 5%{\x1b[1;31m%}D%{\x1b[0m%}4%{\x1b[1;31m%}M%{\x1b[0m%} 3%{\x1b[1;37m%}A%{\x1b[0m%} 2%{\x1b[1;32m%}C%{\x1b[0m%} "

    , testCase "Other: with every kind of Changes" $
      testEveryRepoChange Other @?= "6\x1b[1;32mA\x1b[0m8\x1b[1;32mD\x1b[0m7\x1b[1;32mM\x1b[0m1\x1b[1;32mR\x1b[0m 5\x1b[1;31mD\x1b[0m4\x1b[1;31mM\x1b[0m 3\x1b[1;37mA\x1b[0m 2\x1b[1;32mC\x1b[0m "
  ]

testAddStashes :: TestTree
testAddStashes = testGroup "#addStashes"
  [ testCase "ZSH: hardcoded character" $
      testWriterWithConfig
        (buildOutputConfig ZSH (zeroGitRepoState { gitStashCount = 2 }) defaultConfig) addStashes
      @?= "2%{\x1b[1;32m%}\8801 %{\x1b[0m%}"

    , testCase "Other: hardcoded character" $
      testWriterWithConfig
        (buildOutputConfig Other (zeroGitRepoState { gitStashCount = 2 }) defaultConfig) addStashes
      @?= "2\x1b[1;32m\8801 \x1b[0m"
  ]

-- | Utility function to test a ShellOutput function and gets the prompt built
testWriterWithConfig :: OutputConfig    -- ^ Starting reader state
                     -> ShellOutput     -- ^ Function under test
                     -> String               -- ^ Output of the function for the given config
testWriterWithConfig config functionUnderTest =
  runReader (runWriterT functionUnderTest >>= (\(_, out) -> return out)) config

zeroOutputConfig :: Shell
                 -> OutputConfig
zeroOutputConfig shell = buildOutputConfig shell zeroGitRepoState defaultConfig

testRemoteCommitsToPull :: Shell -> String
testRemoteCommitsToPull shell = testWriterWithConfig
  (buildOutputConfig shell (zeroGitRepoState { gitRemoteCommitsToPull = 2 }) defaultConfig)
  addRemoteCommits

testRemoteCommitsToPush :: Shell -> String
testRemoteCommitsToPush shell = testWriterWithConfig
  (buildOutputConfig shell (zeroGitRepoState { gitRemoteCommitsToPush = 2 }) defaultConfig)
  addRemoteCommits

testRemoteCommitsToPushAndPull :: Shell -> String
testRemoteCommitsToPushAndPull shell = testWriterWithConfig
  (buildOutputConfig shell
    (zeroGitRepoState { gitRemoteCommitsToPull = 4, gitRemoteCommitsToPush = 4 })
    defaultConfig
  )
  addRemoteCommits

testCommitsToPull :: Shell -> String
testCommitsToPull shell = testWriterWithConfig
  (buildOutputConfig shell (zeroGitRepoState { gitCommitsToPull = 2 }) defaultConfig)
  addLocalCommits

testCommitsToPush :: Shell -> String
testCommitsToPush shell = testWriterWithConfig
  (buildOutputConfig shell (zeroGitRepoState { gitCommitsToPush = 2 }) defaultConfig)
  addLocalCommits

testCommitsToPushAndPull :: Shell -> String
testCommitsToPushAndPull shell = testWriterWithConfig
  (buildOutputConfig shell
    (zeroGitRepoState { gitCommitsToPull = 4, gitCommitsToPush = 4 })
    defaultConfig
  )
  addLocalCommits

testLocalAddChange :: Shell -> String
testLocalAddChange shell = testWriterWithConfig
  (buildOutputConfig shell
    (zeroGitRepoState { gitLocalRepoChanges =
      (zeroLocalRepoChanges { localAdd = 2 })
    })
    defaultConfig
  )
  addRepoState

testLocalModChange :: Shell -> String
testLocalModChange shell = testWriterWithConfig
  (buildOutputConfig shell
    (zeroGitRepoState { gitLocalRepoChanges =
      (zeroLocalRepoChanges { localMod = 2 })
    })
    defaultConfig
  )
  addRepoState

testLocalDelChange :: Shell -> String
testLocalDelChange shell = testWriterWithConfig
  (buildOutputConfig shell
    (zeroGitRepoState { gitLocalRepoChanges =
      (zeroLocalRepoChanges { localDel = 2 })
    })
    defaultConfig
  )
  addRepoState

testIndexAddChange :: Shell -> String
testIndexAddChange shell = testWriterWithConfig
  (buildOutputConfig shell
    (zeroGitRepoState { gitLocalRepoChanges =
      (zeroLocalRepoChanges { indexAdd = 2 })
    })
    defaultConfig
  )
  addRepoState

testIndexModChange :: Shell -> String
testIndexModChange shell = testWriterWithConfig
  (buildOutputConfig shell
    (zeroGitRepoState { gitLocalRepoChanges =
      (zeroLocalRepoChanges { indexMod = 2 })
    })
    defaultConfig
  )
  addRepoState

testIndexDelChange :: Shell -> String
testIndexDelChange shell = testWriterWithConfig
  (buildOutputConfig shell
    (zeroGitRepoState { gitLocalRepoChanges =
      (zeroLocalRepoChanges { indexDel = 2 })
    })
    defaultConfig
  )
  addRepoState

testConflictedChange :: Shell -> String
testConflictedChange shell = testWriterWithConfig
  (buildOutputConfig shell
    (zeroGitRepoState { gitLocalRepoChanges =
      (zeroLocalRepoChanges { conflict = 2 })
    })
    defaultConfig
  )
  addRepoState

testRenamedChange :: Shell -> String
testRenamedChange shell = testWriterWithConfig
  (buildOutputConfig shell
    (zeroGitRepoState { gitLocalRepoChanges =
      (zeroLocalRepoChanges { renamed = 2 })
    })
    defaultConfig
  )
  addRepoState

testEveryRepoChange :: Shell -> String
testEveryRepoChange shell = testWriterWithConfig
  (buildOutputConfig shell
    (zeroGitRepoState { gitLocalRepoChanges =
      (zeroLocalRepoChanges { renamed = 1
        , conflict = 2
        , localAdd = 3
        , localMod = 4
        , localDel = 5
        , indexAdd = 6
        , indexMod = 7
        , indexDel = 8
      })
    })
    defaultConfig
  )
  addRepoState
