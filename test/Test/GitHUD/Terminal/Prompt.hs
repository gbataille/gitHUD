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
import GitHUD.Types

terminalPromptTests :: TestTree
terminalPromptTests = testGroup "Terminal Prompt Test"
  [ testResetPromptAtBeginning
    , testAddGitRepoIndicator
    , testAddNoTrackedUpstreamIndicator
    , testAddMergeBranchCommits
    , testAddLocalBranchName
    , testAddLocalCommits
    , testAddRepoState
    , testAddStashes
    , testPartialPrompt
  ]

testResetPromptAtBeginning :: TestTree
testResetPromptAtBeginning = testGroup "#resetPromptAtBeginning"
 [   testCase "Should start the prompt with a reset color control sequence" $
       testWriterWithConfig (zeroOutputConfig ZSH) resetPromptAtBeginning @?= "%{\x1b[39m%}"
   , testCase "Should start the prompt with a reset color control sequence" $
       testWriterWithConfig (zeroOutputConfig Other) resetPromptAtBeginning @?= "\x1b[39m"
 ]

testAddGitRepoIndicator :: TestTree
testAddGitRepoIndicator = testGroup "#addGitRepoIndicator"
  [ testGroup "Default Config"
      [ testCase "ZSH: default config: hardcoded character" $
          testWriterWithConfig (zeroOutputConfig ZSH) addGitRepoIndicator @?= "ᚴ "

        , testCase "Other: default config: hardcoded character" $
          testWriterWithConfig (zeroOutputConfig Other) addGitRepoIndicator @?= "ᚴ "
      ]
    , testGroup "Custom Config"
      [ testCase "ZSH: custom config: hardcoded character" $
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
  ]

customConfigNoTrackedUpstreamIndicator :: Config
customConfigNoTrackedUpstreamIndicator = defaultConfig {
  confNoTrackedUpstreamString = "foo"
  , confNoTrackedUpstreamStringColor = Cyan
  , confNoTrackedUpstreamStringIntensity = Dull
  , confNoTrackedUpstreamIndicator = "bar"
  , confNoTrackedUpstreamIndicatorColor = Green
  , confNoTrackedUpstreamIndicatorIntensity = Dull
}

testAddNoTrackedUpstreamIndicator :: TestTree
testAddNoTrackedUpstreamIndicator = testGroup "#addTrackedUpstreamIndicator"
  [ testGroup "Default Config"
      [ testCase "ZSH: with an upstream" $
          testWriterWithConfig
            (buildOutputConfig ZSH (zeroGitRepoState { gitRemoteTrackingBranch = "foo" }) defaultConfig)
            addNoTrackedUpstreamIndicator
          @?= ""

        , testCase "Other: with an upstream" $
          testWriterWithConfig
            (buildOutputConfig Other (zeroGitRepoState { gitRemoteTrackingBranch = "foo" }) defaultConfig)
            addNoTrackedUpstreamIndicator
          @?= ""

      , testCase "ZSH: with no upstream" $
          testWriterWithConfig
            (zeroOutputConfig ZSH) addNoTrackedUpstreamIndicator
          @?= "%{\x1b[1;31m%}upstream%{\x1b[39m%} %{\x1b[1;31m%}\9889%{\x1b[39m%} "

      , testCase "Other: with no upstream" $
          testWriterWithConfig
            (zeroOutputConfig Other) addNoTrackedUpstreamIndicator
          @?= "\x1b[1;31mupstream\x1b[39m \x1b[1;31m\9889\x1b[39m "
      ]
    , testGroup "Custom Config"
      [ testCase "ZSH: with an upstream" $
          testWriterWithConfig
            (buildOutputConfig ZSH (zeroGitRepoState { gitRemoteTrackingBranch = "foo" }) customConfigNoTrackedUpstreamIndicator)
            addNoTrackedUpstreamIndicator
          @?= ""

        , testCase "Other: with an upstream" $
          testWriterWithConfig
            (buildOutputConfig Other (zeroGitRepoState { gitRemoteTrackingBranch = "foo" }) customConfigNoTrackedUpstreamIndicator)
            addNoTrackedUpstreamIndicator
          @?= ""

      , testCase "ZSH: with no upstream" $
          testWriterWithConfig
            (buildOutputConfig ZSH (zeroGitRepoState) customConfigNoTrackedUpstreamIndicator)
            addNoTrackedUpstreamIndicator
          @?= "%{\x1b[36m%}foo%{\x1b[39m%} %{\x1b[32m%}bar%{\x1b[39m%} "

      , testCase "Other: with no upstream" $
          testWriterWithConfig
            (buildOutputConfig Other (zeroGitRepoState) customConfigNoTrackedUpstreamIndicator)
            addNoTrackedUpstreamIndicator
          @?= "\x1b[36mfoo\x1b[39m \x1b[32mbar\x1b[39m "
      ]
  ]

customConfigMergeBranchCommits :: Config
customConfigMergeBranchCommits = defaultConfig {
  confMergeBranchCommitsIndicator = "foo"
  , confMergeBranchCommitsOnlyPull = "pull"
  , confMergeBranchCommitsOnlyPush = "push"
  , confMergeBranchCommitsBothPullPush = "pull-push"
}

testAddMergeBranchCommits :: TestTree
testAddMergeBranchCommits = testGroup "#addMergeBranchCommits"
  [ testGroup "Default Config"
    [ testCase "ZSH: commits to pull" $
        testMergeBranchCommitsToPull ZSH defaultConfig @?=
        "\120366 %{\x1b[1;32m%}\8594%{\x1b[39m%} 2 "

    , testCase "ZSH: commits to push" $
        testMergeBranchCommitsToPush ZSH defaultConfig @?=
        "\120366 %{\x1b[1;32m%}\8592%{\x1b[39m%} 2 "

    , testCase "ZSH: commits to pull and to push" $
        testMergeBranchCommitsToPushAndPull ZSH defaultConfig @?=
        "\120366 4%{\x1b[1;32m%}\8644%{\x1b[39m%}4 "

    , testCase "Other: commits to pull" $
        testMergeBranchCommitsToPull Other defaultConfig @?=
        "\120366 \x1b[1;32m\8594\x1b[39m 2 "

    , testCase "Other: commits to push" $
        testMergeBranchCommitsToPush Other defaultConfig @?=
        "\120366 \x1b[1;32m\8592\x1b[39m 2 "

    , testCase "Other: commits to pull and to push" $
        testMergeBranchCommitsToPushAndPull Other defaultConfig @?=
        "\120366 4\x1b[1;32m\8644\x1b[39m4 "
    ]

    , testGroup "Custom Config"
        [ testCase "ZSH: commits to pull" $
            testMergeBranchCommitsToPull ZSH customConfigMergeBranchCommits @?=
            "foo %{\x1b[1;32m%}pull%{\x1b[39m%} 2 "

        , testCase "ZSH: commits to push" $
            testMergeBranchCommitsToPush ZSH customConfigMergeBranchCommits @?=
            "foo %{\x1b[1;32m%}push%{\x1b[39m%} 2 "

        , testCase "ZSH: commits to pull and to push" $
            testMergeBranchCommitsToPushAndPull ZSH customConfigMergeBranchCommits @?=
            "foo 4%{\x1b[1;32m%}pull-push%{\x1b[39m%}4 "

        , testCase "Other: commits to pull" $
            testMergeBranchCommitsToPull Other customConfigMergeBranchCommits @?=
            "foo \x1b[1;32mpull\x1b[39m 2 "

        , testCase "Other: commits to push" $
            testMergeBranchCommitsToPush Other customConfigMergeBranchCommits @?=
            "foo \x1b[1;32mpush\x1b[39m 2 "

        , testCase "Other: commits to pull and to push" $
            testMergeBranchCommitsToPushAndPull Other customConfigMergeBranchCommits @?=
            "foo 4\x1b[1;32mpull-push\x1b[39m4 "
        ]
  ]

customConfigLocalBranchName :: Config
customConfigLocalBranchName = defaultConfig {
  confLocalBranchColor = Cyan
  , confLocalDetachedColor = Magenta
  , confLocalBranchIntensity = Dull
  , confLocalDetachedIntensity = Dull
  , confLocalBranchNamePrefix = "{"
  , confLocalBranchNameSuffix = "}"
  , confLocalDetachedPrefix = "det#!"
}

testAddLocalBranchName :: TestTree
testAddLocalBranchName = testGroup "#addLocalBranchName"
  [ testGroup "Default Config"
    [   testCase "ZSH: should display the name of the current branch if we are at the HEAD of any" $
          testWriterWithConfig
            (buildOutputConfig ZSH (zeroGitRepoState { gitLocalBranch = "foo" }) defaultConfig)
            addLocalBranchName
          @?= "[%{\x1b[39m%}foo%{\x1b[39m%}] "

        , testCase "Other: should display the name of the current branch if we are at the HEAD of any" $
          testWriterWithConfig
            (buildOutputConfig Other (zeroGitRepoState { gitLocalBranch = "foo" }) defaultConfig)
            addLocalBranchName
          @?= "[\x1b[39mfoo\x1b[39m] "

        , testCase "ZSH: should display the current commit tag if we are not on one" $
          testWriterWithConfig
            (buildOutputConfig ZSH (zeroGitRepoState { gitCommitTag = "v1.1.1" }) defaultConfig)
            addLocalBranchName
          @?= "[%{\x1b[1;33m%}detached@v1.1.1%{\x1b[39m%}] "

        , testCase "Other: should display the current commit tog if we are on one" $
          testWriterWithConfig
            (buildOutputConfig Other (zeroGitRepoState { gitCommitTag = "v1.1.1" }) defaultConfig)
            addLocalBranchName
          @?= "[\x1b[1;33mdetached@v1.1.1\x1b[39m] "

        , testCase "ZSH: should display the current commit SHA if we are not on a branch's HEAD" $
          testWriterWithConfig
            (buildOutputConfig ZSH (zeroGitRepoState { gitCommitShortSHA = "3d25ef" }) defaultConfig)
            addLocalBranchName
          @?= "[%{\x1b[1;33m%}detached@3d25ef%{\x1b[39m%}] "

        , testCase "Other: should display the current commit SHA if we are not on a branch's HEAD" $
          testWriterWithConfig
            (buildOutputConfig Other (zeroGitRepoState { gitCommitShortSHA = "3d25ef" }) defaultConfig)
            addLocalBranchName
          @?= "[\x1b[1;33mdetached@3d25ef\x1b[39m] "
    ]
    , testGroup "Custom Config"
    [   testCase "ZSH: should display the name of the current branch if we are at the HEAD of any" $
          testWriterWithConfig
            (buildOutputConfig ZSH (zeroGitRepoState { gitLocalBranch = "foo" }) customConfigLocalBranchName)
            addLocalBranchName
          @?= "{%{\x1b[36m%}foo%{\x1b[39m%}} "

        , testCase "Other: should display the name of the current branch if we are at the HEAD of any" $
          testWriterWithConfig
            (buildOutputConfig Other (zeroGitRepoState { gitLocalBranch = "foo" }) customConfigLocalBranchName)
            addLocalBranchName
          @?= "{\x1b[36mfoo\x1b[39m} "

        , testCase "ZSH: should display the current commit tag if we are not on one" $
          testWriterWithConfig
            (buildOutputConfig ZSH (zeroGitRepoState { gitCommitTag = "v1.1.1" }) customConfigLocalBranchName)
            addLocalBranchName
          @?= "{%{\x1b[35m%}det#!v1.1.1%{\x1b[39m%}} "

        , testCase "Other: should display the current commit tog if we are on one" $
          testWriterWithConfig
            (buildOutputConfig Other (zeroGitRepoState { gitCommitTag = "v1.1.1" }) customConfigLocalBranchName)
            addLocalBranchName
          @?= "{\x1b[35mdet#!v1.1.1\x1b[39m} "

        , testCase "ZSH: should display the current commit SHA if we are not on a branch's HEAD" $
          testWriterWithConfig
            (buildOutputConfig ZSH (zeroGitRepoState { gitCommitShortSHA = "3d25ef" }) customConfigLocalBranchName)
            addLocalBranchName
          @?= "{%{\x1b[35m%}det#!3d25ef%{\x1b[39m%}} "

        , testCase "Other: should display the current commit SHA if we are not on a branch's HEAD" $
          testWriterWithConfig
            (buildOutputConfig Other (zeroGitRepoState { gitCommitShortSHA = "3d25ef" }) customConfigLocalBranchName)
            addLocalBranchName
          @?= "{\x1b[35mdet#!3d25ef\x1b[39m} "
    ]
  ]

customConfigLocalCommits :: Config
customConfigLocalCommits = defaultConfig {
    confLocalCommitsPushSuffix = "push"
  , confLocalCommitsPushSuffixColor = Cyan
  , confLocalCommitsPushSuffixIntensity = Dull
  , confLocalCommitsPullSuffix = "pull"
  , confLocalCommitsPullSuffixColor = Magenta
  , confLocalCommitsPullSuffixIntensity = Dull
  , confLocalCommitsPushPullInfix = "push-pull"
  , confLocalCommitsPushPullInfixColor = White
  , confLocalCommitsPushPullInfixIntensity = Dull
}

testAddLocalCommits :: TestTree
testAddLocalCommits = testGroup "#addLocalCommits"
  [   testGroup "Default Config"
      [ testCase "ZSH: commits to pull" $
          testCommitsToPull ZSH defaultConfig @?=
          "2%{\x1b[1;31m%}\8595%{\x1b[39m%} "

      , testCase "ZSH: commits to push" $
          testCommitsToPush ZSH defaultConfig @?=
          "2%{\x1b[1;32m%}\8593%{\x1b[39m%} "

      , testCase "ZSH: commits to pull and to push" $
          testCommitsToPushAndPull ZSH defaultConfig @?=
          "4%{\x1b[1;32m%}⥯%{\x1b[39m%}4 "

      , testCase "Other: commits to pull" $
          testCommitsToPull Other defaultConfig @?=
          "2\x1b[1;31m\8595\x1b[39m "

      , testCase "Other: commits to push" $
          testCommitsToPush Other defaultConfig @?=
          "2\x1b[1;32m\8593\x1b[39m "

      , testCase "Other: commits to pull and to push" $
          testCommitsToPushAndPull Other defaultConfig @?=
          "4\x1b[1;32m⥯\x1b[39m4 "
      ]
    , testGroup "Custom Config"
      [ testCase "ZSH: commits to pull" $
          testCommitsToPull ZSH customConfigLocalCommits @?=
          "2%{\x1b[35m%}pull%{\x1b[39m%} "

      , testCase "ZSH: commits to push" $
          testCommitsToPush ZSH customConfigLocalCommits @?=
          "2%{\x1b[36m%}push%{\x1b[39m%} "

      , testCase "ZSH: commits to pull and to push" $
          testCommitsToPushAndPull ZSH customConfigLocalCommits @?=
          "4%{\x1b[37m%}push-pull%{\x1b[39m%}4 "

      , testCase "Other: commits to pull" $
          testCommitsToPull Other customConfigLocalCommits @?=
          "2\x1b[35mpull\x1b[39m "

      , testCase "Other: commits to push" $
          testCommitsToPush Other customConfigLocalCommits @?=
          "2\x1b[36mpush\x1b[39m "

      , testCase "Other: commits to pull and to push" $
          testCommitsToPushAndPull Other customConfigLocalCommits @?=
          "4\x1b[37mpush-pull\x1b[39m4 "
      ]
  ]

customChangeConfig :: Config
customChangeConfig = defaultConfig {
    confChangeIndexAddSuffix = "B"
  , confChangeIndexAddSuffixColor = Cyan
  , confChangeIndexAddSuffixIntensity = Dull
  , confChangeIndexModSuffix = "N"
  , confChangeIndexModSuffixColor = Cyan
  , confChangeIndexModSuffixIntensity = Dull
  , confChangeIndexDelSuffix = "E"
  , confChangeIndexDelSuffixColor = Cyan
  , confChangeIndexDelSuffixIntensity = Dull
  , confChangeLocalAddSuffix = "B"
  , confChangeLocalAddSuffixColor = Magenta
  , confChangeLocalAddSuffixIntensity = Dull
  , confChangeLocalModSuffix = "N"
  , confChangeLocalModSuffixColor = Blue
  , confChangeLocalModSuffixIntensity = Dull
  , confChangeLocalDelSuffix = "E"
  , confChangeLocalDelSuffixColor = Blue
  , confChangeLocalDelSuffixIntensity = Dull
  , confChangeRenamedSuffix = "S"
  , confChangeRenamedSuffixColor = Cyan
  , confChangeRenamedSuffixIntensity = Dull
  , confChangeConflictedSuffix = "D"
  , confChangeConflictedSuffixColor = Cyan
  , confChangeConflictedSuffixIntensity = Dull
}

testAddRepoState :: TestTree
testAddRepoState = testGroup "#addRepoState"
  [   testGroup "Default Config"
        [ testCase "ZSH: with Local Add Changes" $
            testLocalAddChange ZSH defaultConfig @?= "2%{\x1b[1;37m%}A%{\x1b[39m%} "

          , testCase "ZSH: with Local Mod Changes" $
            testLocalModChange ZSH defaultConfig @?= "2%{\x1b[1;31m%}M%{\x1b[39m%} "

          , testCase "ZSH: with Local Del Changes" $
            testLocalDelChange ZSH defaultConfig @?= "2%{\x1b[1;31m%}D%{\x1b[39m%} "

          , testCase "ZSH: with Index Add Changes" $
            testIndexAddChange ZSH defaultConfig @?= "2%{\x1b[1;32m%}A%{\x1b[39m%} "

          , testCase "ZSH: with Index Mod Changes" $
            testIndexModChange ZSH defaultConfig @?= "2%{\x1b[1;32m%}M%{\x1b[39m%} "

          , testCase "ZSH: with Index Del Changes" $
            testIndexDelChange ZSH defaultConfig @?= "2%{\x1b[1;32m%}D%{\x1b[39m%} "

          , testCase "ZSH: with Conflicted Changes" $
            testConflictedChange ZSH defaultConfig @?= "2%{\x1b[1;32m%}C%{\x1b[39m%} "

          , testCase "ZSH: with Renamed Changes" $
            testRenamedChange ZSH defaultConfig @?= "2%{\x1b[1;32m%}R%{\x1b[39m%} "

          , testCase "Other: with Local Add Changes" $
            testLocalAddChange Other defaultConfig @?= "2\x1b[1;37mA\x1b[39m "

          , testCase "Other: with Local Mod Changes" $
            testLocalModChange Other defaultConfig @?= "2\x1b[1;31mM\x1b[39m "

          , testCase "Other: with Local Del Changes" $
            testLocalDelChange Other defaultConfig @?= "2\x1b[1;31mD\x1b[39m "

          , testCase "Other: with Index Add Changes" $
            testIndexAddChange Other defaultConfig @?= "2\x1b[1;32mA\x1b[39m "

          , testCase "Other: with Index Mod Changes" $
            testIndexModChange Other defaultConfig @?= "2\x1b[1;32mM\x1b[39m "

          , testCase "Other: with Index Del Changes" $
            testIndexDelChange Other defaultConfig @?= "2\x1b[1;32mD\x1b[39m "

          , testCase "Other: with Conflicted Changes" $
            testConflictedChange Other defaultConfig @?= "2\x1b[1;32mC\x1b[39m "

          , testCase "Other: with Renamed Changes" $
            testRenamedChange Other defaultConfig @?= "2\x1b[1;32mR\x1b[39m "

          , testCase "ZSH: with every kind of Changes" $
            testEveryRepoChange ZSH defaultConfig @?= "6%{\x1b[1;32m%}A%{\x1b[39m%}8%{\x1b[1;32m%}D%{\x1b[39m%}7%{\x1b[1;32m%}M%{\x1b[39m%}1%{\x1b[1;32m%}R%{\x1b[39m%} 5%{\x1b[1;31m%}D%{\x1b[39m%}4%{\x1b[1;31m%}M%{\x1b[39m%} 3%{\x1b[1;37m%}A%{\x1b[39m%} 2%{\x1b[1;32m%}C%{\x1b[39m%} "

          , testCase "Other: with every kind of Changes" $
            testEveryRepoChange Other defaultConfig @?= "6\x1b[1;32mA\x1b[39m8\x1b[1;32mD\x1b[39m7\x1b[1;32mM\x1b[39m1\x1b[1;32mR\x1b[39m 5\x1b[1;31mD\x1b[39m4\x1b[1;31mM\x1b[39m 3\x1b[1;37mA\x1b[39m 2\x1b[1;32mC\x1b[39m "
        ]
    , testGroup "Custom Config"
        [ testCase "ZSH: with Local Add Changes" $
            testLocalAddChange ZSH customChangeConfig @?= "2%{\x1b[35m%}B%{\x1b[39m%} "

          , testCase "ZSH: with Local Mod Changes" $
            testLocalModChange ZSH customChangeConfig @?= "2%{\x1b[34m%}N%{\x1b[39m%} "

          , testCase "ZSH: with Local Del Changes" $
            testLocalDelChange ZSH customChangeConfig @?= "2%{\x1b[34m%}E%{\x1b[39m%} "

          , testCase "ZSH: with Index Add Changes" $
            testIndexAddChange ZSH customChangeConfig @?= "2%{\x1b[36m%}B%{\x1b[39m%} "

          , testCase "ZSH: with Index Mod Changes" $
            testIndexModChange ZSH customChangeConfig @?= "2%{\x1b[36m%}N%{\x1b[39m%} "

          , testCase "ZSH: with Index Del Changes" $
            testIndexDelChange ZSH customChangeConfig @?= "2%{\x1b[36m%}E%{\x1b[39m%} "

          , testCase "ZSH: with Conflicted Changes" $
            testConflictedChange ZSH customChangeConfig @?= "2%{\x1b[36m%}D%{\x1b[39m%} "

          , testCase "ZSH: with Renamed Changes" $
            testRenamedChange ZSH customChangeConfig @?= "2%{\x1b[36m%}S%{\x1b[39m%} "

          , testCase "Other: with Local Add Changes" $
            testLocalAddChange Other customChangeConfig @?= "2\x1b[35mB\x1b[39m "

          , testCase "Other: with Local Mod Changes" $
            testLocalModChange Other customChangeConfig @?= "2\x1b[34mN\x1b[39m "

          , testCase "Other: with Local Del Changes" $
            testLocalDelChange Other customChangeConfig @?= "2\x1b[34mE\x1b[39m "

          , testCase "Other: with Index Add Changes" $
            testIndexAddChange Other customChangeConfig @?= "2\x1b[36mB\x1b[39m "

          , testCase "Other: with Index Mod Changes" $
            testIndexModChange Other customChangeConfig @?= "2\x1b[36mN\x1b[39m "

          , testCase "Other: with Index Del Changes" $
            testIndexDelChange Other customChangeConfig @?= "2\x1b[36mE\x1b[39m "

          , testCase "Other: with Conflicted Changes" $
            testConflictedChange Other customChangeConfig @?= "2\x1b[36mD\x1b[39m "

          , testCase "Other: with Renamed Changes" $
            testRenamedChange Other customChangeConfig @?= "2\x1b[36mS\x1b[39m "

          , testCase "ZSH: with every kind of Changes" $
            testEveryRepoChange ZSH customChangeConfig @?= "6%{\x1b[36m%}B%{\x1b[39m%}8%{\x1b[36m%}E%{\x1b[39m%}7%{\x1b[36m%}N%{\x1b[39m%}1%{\x1b[36m%}S%{\x1b[39m%} 5%{\x1b[34m%}E%{\x1b[39m%}4%{\x1b[34m%}N%{\x1b[39m%} 3%{\x1b[35m%}B%{\x1b[39m%} 2%{\x1b[36m%}D%{\x1b[39m%} "

          , testCase "Other: with every kind of Changes" $
            testEveryRepoChange Other customChangeConfig @?= "6\x1b[36mB\x1b[39m8\x1b[36mE\x1b[39m7\x1b[36mN\x1b[39m1\x1b[36mS\x1b[39m 5\x1b[34mE\x1b[39m4\x1b[34mN\x1b[39m 3\x1b[35mB\x1b[39m 2\x1b[36mD\x1b[39m "
        ]
  ]

customStashConfig :: Config
customStashConfig = defaultConfig {
  confStashSuffix = "stash"
  , confStashSuffixColor = Cyan
  , confStashSuffixIntensity = Dull
}

testAddStashes :: TestTree
testAddStashes = testGroup "#addStashes"
  [ testGroup "Default Config"
      [ testCase "ZSH: hardcoded character" $
          testWriterWithConfig
            (buildOutputConfig ZSH (zeroGitRepoState { gitStashCount = 2 }) defaultConfig) addStashes
          @?= "2%{\x1b[1;32m%}\8801%{\x1b[39m%} "

        , testCase "Other: hardcoded character" $
          testWriterWithConfig
            (buildOutputConfig Other (zeroGitRepoState { gitStashCount = 2 }) defaultConfig) addStashes
          @?= "2\x1b[1;32m\8801\x1b[39m "
      ]
  , testGroup "Custom Config"
      [ testCase "ZSH: hardcoded character" $
          testWriterWithConfig
            (buildOutputConfig ZSH (zeroGitRepoState { gitStashCount = 2 }) customStashConfig) addStashes
          @?= "2%{\x1b[36m%}stash%{\x1b[39m%} "

        , testCase "Other: hardcoded character" $
          testWriterWithConfig
            (buildOutputConfig Other (zeroGitRepoState { gitStashCount = 2 }) customStashConfig) addStashes
          @?= "2\x1b[36mstash\x1b[39m "
      ]
  ]

localChangesForPartialPrompt :: GitLocalRepoChanges
localChangesForPartialPrompt = GitLocalRepoChanges {
    localMod = 1
    , localAdd = 2
    , localDel = 3
    , indexMod = 4
    , indexAdd = 5
    , indexDel = 6
    , renamed  = 7
    , conflict = 8
  }

repoStateForPartialPrompt :: GitRepoState
repoStateForPartialPrompt = GitRepoState {
    gitLocalRepoChanges = localChangesForPartialPrompt
    , gitLocalBranch = "branch"
    , gitCommitShortSHA = "3de6ef"
    , gitRemote = "origin"
    , gitRemoteTrackingBranch = "origin/branch"
    , gitStashCount = 3
    , gitCommitsToPull = 5
    , gitCommitsToPush = 6
    , gitMergeBranchCommitsToPull = 2
    , gitMergeBranchCommitsToPush = 1
  }

{- For reference here, the full prompt would be

 "%{\ESC[39m%}\5812 \120366 2%{\ESC[1;32m%}\8644%{\ESC[39m%}1 [%{\ESC[39m%}branch%{\ESC[39m%}] 5%{\ESC[1;32m%}⥯%{\ESC[39m%}6 5%{\ESC[1;32m%}A%{\ESC[39m%}6%{\ESC[1;32m%}D%{\ESC[39m%}4%{\ESC[1;32m%}M%{\ESC[39m%}7%{\ESC[1;32m%}R%{\ESC[39m%} 3%{\ESC[1;31m%}D%{\ESC[39m%}1%{\ESC[1;31m%}M%{\ESC[39m%} 2%{\ESC[1;37m%}A%{\ESC[39m%} 8%{\ESC[1;32m%}C%{\ESC[39m%} 3%{\ESC[1;32m%}\8801%{\ESC[39m%} "

-}
testPartialPrompt :: TestTree
testPartialPrompt = testGroup "Partial prompt display"
  [   testCase "w/out repo indicator" $
        testWriterWithConfig
          (buildOutputConfig ZSH repoStateForPartialPrompt defaultConfig { confShowPartRepoIndicator = False })
          buildPrompt
        @?= "%{\ESC[39m%}\120366 2%{\ESC[1;32m%}\8644%{\ESC[39m%}1 [%{\ESC[39m%}branch%{\ESC[39m%}] 5%{\ESC[1;32m%}⥯%{\ESC[39m%}6 5%{\ESC[1;32m%}A%{\ESC[39m%}6%{\ESC[1;32m%}D%{\ESC[39m%}4%{\ESC[1;32m%}M%{\ESC[39m%}7%{\ESC[1;32m%}R%{\ESC[39m%} 3%{\ESC[1;31m%}D%{\ESC[39m%}1%{\ESC[1;31m%}M%{\ESC[39m%} 2%{\ESC[1;37m%}A%{\ESC[39m%} 8%{\ESC[1;32m%}C%{\ESC[39m%} 3%{\ESC[1;32m%}\8801%{\ESC[39m%} "

    , testCase "w/out merge branch commits info" $
        testWriterWithConfig
          (buildOutputConfig ZSH repoStateForPartialPrompt defaultConfig { confShowPartMergeBranchCommitsDiff = False })
          buildPrompt
        @?= "%{\ESC[39m%}\5812 [%{\ESC[39m%}branch%{\ESC[39m%}] 5%{\ESC[1;32m%}⥯%{\ESC[39m%}6 5%{\ESC[1;32m%}A%{\ESC[39m%}6%{\ESC[1;32m%}D%{\ESC[39m%}4%{\ESC[1;32m%}M%{\ESC[39m%}7%{\ESC[1;32m%}R%{\ESC[39m%} 3%{\ESC[1;31m%}D%{\ESC[39m%}1%{\ESC[1;31m%}M%{\ESC[39m%} 2%{\ESC[1;37m%}A%{\ESC[39m%} 8%{\ESC[1;32m%}C%{\ESC[39m%} 3%{\ESC[1;32m%}\8801%{\ESC[39m%} "

    , testCase "w/out local branch info" $
        testWriterWithConfig
          (buildOutputConfig ZSH repoStateForPartialPrompt defaultConfig { confShowPartLocalBranch = False })
          buildPrompt
        @?= "%{\ESC[39m%}\5812 \120366 2%{\ESC[1;32m%}\8644%{\ESC[39m%}1 5%{\ESC[1;32m%}⥯%{\ESC[39m%}6 5%{\ESC[1;32m%}A%{\ESC[39m%}6%{\ESC[1;32m%}D%{\ESC[39m%}4%{\ESC[1;32m%}M%{\ESC[39m%}7%{\ESC[1;32m%}R%{\ESC[39m%} 3%{\ESC[1;31m%}D%{\ESC[39m%}1%{\ESC[1;31m%}M%{\ESC[39m%} 2%{\ESC[1;37m%}A%{\ESC[39m%} 8%{\ESC[1;32m%}C%{\ESC[39m%} 3%{\ESC[1;32m%}\8801%{\ESC[39m%} "

    , testCase "with a branch set to ignore its merge branch" $
        testWriterWithConfig
          (buildOutputConfig ZSH repoStateForPartialPrompt defaultConfig { confMergeBranchIgnoreBranches = ["branch"] })
          buildPrompt
        @?= "%{\ESC[39m%}\5812 [%{\ESC[39m%}branch%{\ESC[39m%}] 5%{\ESC[1;32m%}⥯%{\ESC[39m%}6 5%{\ESC[1;32m%}A%{\ESC[39m%}6%{\ESC[1;32m%}D%{\ESC[39m%}4%{\ESC[1;32m%}M%{\ESC[39m%}7%{\ESC[1;32m%}R%{\ESC[39m%} 3%{\ESC[1;31m%}D%{\ESC[39m%}1%{\ESC[1;31m%}M%{\ESC[39m%} 2%{\ESC[1;37m%}A%{\ESC[39m%} 8%{\ESC[1;32m%}C%{\ESC[39m%} 3%{\ESC[1;32m%}\8801%{\ESC[39m%} "

    , testCase "w/out commits push/pull info" $
        testWriterWithConfig
          (buildOutputConfig ZSH repoStateForPartialPrompt defaultConfig { confShowPartCommitsToOrigin = False })
          buildPrompt
        @?= "%{\ESC[39m%}\5812 \120366 2%{\ESC[1;32m%}\8644%{\ESC[39m%}1 [%{\ESC[39m%}branch%{\ESC[39m%}] 5%{\ESC[1;32m%}A%{\ESC[39m%}6%{\ESC[1;32m%}D%{\ESC[39m%}4%{\ESC[1;32m%}M%{\ESC[39m%}7%{\ESC[1;32m%}R%{\ESC[39m%} 3%{\ESC[1;31m%}D%{\ESC[39m%}1%{\ESC[1;31m%}M%{\ESC[39m%} 2%{\ESC[1;37m%}A%{\ESC[39m%} 8%{\ESC[1;32m%}C%{\ESC[39m%} 3%{\ESC[1;32m%}\8801%{\ESC[39m%} "

    , testCase "w/out local repo changes" $
        testWriterWithConfig
          (buildOutputConfig ZSH repoStateForPartialPrompt defaultConfig { confShowPartLocalChangesState = False })
          buildPrompt
        @?= "%{\ESC[39m%}\5812 \120366 2%{\ESC[1;32m%}\8644%{\ESC[39m%}1 [%{\ESC[39m%}branch%{\ESC[39m%}] 5%{\ESC[1;32m%}⥯%{\ESC[39m%}6 3%{\ESC[1;32m%}\8801%{\ESC[39m%} "

    , testCase "w/out stashes" $
        testWriterWithConfig
          (buildOutputConfig ZSH repoStateForPartialPrompt defaultConfig { confShowPartStashes = False })
          buildPrompt
        @?= "%{\ESC[39m%}\5812 \120366 2%{\ESC[1;32m%}\8644%{\ESC[39m%}1 [%{\ESC[39m%}branch%{\ESC[39m%}] 5%{\ESC[1;32m%}⥯%{\ESC[39m%}6 5%{\ESC[1;32m%}A%{\ESC[39m%}6%{\ESC[1;32m%}D%{\ESC[39m%}4%{\ESC[1;32m%}M%{\ESC[39m%}7%{\ESC[1;32m%}R%{\ESC[39m%} 3%{\ESC[1;31m%}D%{\ESC[39m%}1%{\ESC[1;31m%}M%{\ESC[39m%} 2%{\ESC[1;37m%}A%{\ESC[39m%} 8%{\ESC[1;32m%}C%{\ESC[39m%} "
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

testMergeBranchCommitsToPull :: Shell -> Config -> String
testMergeBranchCommitsToPull shell config = testWriterWithConfig
  (buildOutputConfig shell (zeroGitRepoState { gitMergeBranchCommitsToPull = 2 }) config)
  addMergeBranchCommits

testMergeBranchCommitsToPush :: Shell -> Config -> String
testMergeBranchCommitsToPush shell config = testWriterWithConfig
  (buildOutputConfig shell (zeroGitRepoState { gitMergeBranchCommitsToPush = 2 }) config)
  addMergeBranchCommits

testMergeBranchCommitsToPushAndPull :: Shell -> Config -> String
testMergeBranchCommitsToPushAndPull shell config = testWriterWithConfig
  (buildOutputConfig shell
    (zeroGitRepoState { gitMergeBranchCommitsToPull = 4, gitMergeBranchCommitsToPush = 4 })
    config
  )
  addMergeBranchCommits

testCommitsToPull :: Shell -> Config -> String
testCommitsToPull shell config = testWriterWithConfig
  (buildOutputConfig shell (zeroGitRepoState { gitCommitsToPull = 2 }) config)
  addLocalCommits

testCommitsToPush :: Shell -> Config -> String
testCommitsToPush shell config = testWriterWithConfig
  (buildOutputConfig shell (zeroGitRepoState { gitCommitsToPush = 2 }) config)
  addLocalCommits

testCommitsToPushAndPull :: Shell -> Config -> String
testCommitsToPushAndPull shell config = testWriterWithConfig
  (buildOutputConfig shell
    (zeroGitRepoState { gitCommitsToPull = 4, gitCommitsToPush = 4 })
    config
  )
  addLocalCommits

testLocalAddChange :: Shell -> Config -> String
testLocalAddChange shell config = testWriterWithConfig
  (buildOutputConfig shell
    (zeroGitRepoState { gitLocalRepoChanges =
      (zeroLocalRepoChanges { localAdd = 2 })
    })
    config
  )
  addRepoState

testLocalModChange :: Shell -> Config -> String
testLocalModChange shell config = testWriterWithConfig
  (buildOutputConfig shell
    (zeroGitRepoState { gitLocalRepoChanges =
      (zeroLocalRepoChanges { localMod = 2 })
    })
    config
  )
  addRepoState

testLocalDelChange :: Shell -> Config -> String
testLocalDelChange shell config = testWriterWithConfig
  (buildOutputConfig shell
    (zeroGitRepoState { gitLocalRepoChanges =
      (zeroLocalRepoChanges { localDel = 2 })
    })
    config
  )
  addRepoState

testIndexAddChange :: Shell -> Config -> String
testIndexAddChange shell config = testWriterWithConfig
  (buildOutputConfig shell
    (zeroGitRepoState { gitLocalRepoChanges =
      (zeroLocalRepoChanges { indexAdd = 2 })
    })
    config
  )
  addRepoState

testIndexModChange :: Shell -> Config -> String
testIndexModChange shell config = testWriterWithConfig
  (buildOutputConfig shell
    (zeroGitRepoState { gitLocalRepoChanges =
      (zeroLocalRepoChanges { indexMod = 2 })
    })
    config
  )
  addRepoState

testIndexDelChange :: Shell -> Config -> String
testIndexDelChange shell config = testWriterWithConfig
  (buildOutputConfig shell
    (zeroGitRepoState { gitLocalRepoChanges =
      (zeroLocalRepoChanges { indexDel = 2 })
    })
    config
  )
  addRepoState

testConflictedChange :: Shell -> Config -> String
testConflictedChange shell config = testWriterWithConfig
  (buildOutputConfig shell
    (zeroGitRepoState { gitLocalRepoChanges =
      (zeroLocalRepoChanges { conflict = 2 })
    })
    config
  )
  addRepoState

testRenamedChange :: Shell -> Config -> String
testRenamedChange shell config = testWriterWithConfig
  (buildOutputConfig shell
    (zeroGitRepoState { gitLocalRepoChanges =
      (zeroLocalRepoChanges { renamed = 2 })
    })
    config
  )
  addRepoState

testEveryRepoChange :: Shell -> Config -> String
testEveryRepoChange shell config = testWriterWithConfig
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
    config
  )
  addRepoState
