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

allPrompts :: [Shell]
allPrompts = [ZSH, BASH, TMUX, NONE, Other]

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
 [   testCase "ZSH: Should start the prompt with a reset color control sequence" $
       testWriterWithConfig (zeroOutputConfig ZSH) resetPromptAtBeginning @?= "%{\x1b[0;39m%}"
   , testCase "Other: Should start the prompt with a reset color control sequence" $
       testWriterWithConfig (zeroOutputConfig Other) resetPromptAtBeginning @?= "\x1b[0;39m"
   , testCase "TMUX: Should start the prompt with a reset color control sequence" $
       testWriterWithConfig (zeroOutputConfig TMUX) resetPromptAtBeginning @?= "#[fg=default]"
   , testCase "NONE: Should start the prompt with a reset color control sequence" $
       testWriterWithConfig (zeroOutputConfig NONE) resetPromptAtBeginning @?= ""
 ]

_testAddGitDefaultRepoIndicator :: Shell -> TestTree
_testAddGitDefaultRepoIndicator shell =
  testCase ((show shell) ++ ": default config: hardcoded character") $
    testWriterWithConfig (zeroOutputConfig shell) addGitRepoIndicator @?= "ᚴ "

_testAddGitCustomRepoIndicator :: Shell -> TestTree
_testAddGitCustomRepoIndicator shell =
  testCase ((show shell) ++ ": custom config: hardcoded character") $
    testWriterWithConfig
      (buildOutputConfig shell zeroGitRepoState $ defaultConfig { confRepoIndicator = "indic" })
      addGitRepoIndicator
    @?= "indic "

testAddGitRepoIndicator :: TestTree
testAddGitRepoIndicator = testGroup "#addGitRepoIndicator"
  [ testGroup "Default Config" $ fmap _testAddGitDefaultRepoIndicator allPrompts
    , testGroup "Custom Config" $ fmap _testAddGitCustomRepoIndicator allPrompts
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

_testUpstreamIndicatorWithUpstreamDefaultConfig :: Shell -> TestTree
_testUpstreamIndicatorWithUpstreamDefaultConfig shell =
  testCase ((show shell) ++ ": with an upstream") $
    testWriterWithConfig
            (buildOutputConfig shell (zeroGitRepoState { gitRemoteTrackingBranch = "foo" }) defaultConfig)
            addNoTrackedUpstreamIndicator
          @?= ""

_testUpstreamIndicatorWithUpstreamCustomConfig :: Shell -> TestTree
_testUpstreamIndicatorWithUpstreamCustomConfig shell =
  testCase ((show shell) ++ ": with an upstream") $
    testWriterWithConfig
            (buildOutputConfig shell (zeroGitRepoState { gitRemoteTrackingBranch = "foo" }) customConfigNoTrackedUpstreamIndicator)
            addNoTrackedUpstreamIndicator
          @?= ""

testAddNoTrackedUpstreamIndicator :: TestTree
testAddNoTrackedUpstreamIndicator = testGroup "#addTrackedUpstreamIndicator"
  [ testGroup "Default Config - upstream" $ fmap _testUpstreamIndicatorWithUpstreamDefaultConfig allPrompts
    , testGroup "Custom Config - upstream" $ fmap _testUpstreamIndicatorWithUpstreamCustomConfig allPrompts
    , testGroup "Default Config - no upstream"
      [ testCase "ZSH: with no upstream" $
          testWriterWithConfig
            (zeroOutputConfig ZSH) addNoTrackedUpstreamIndicator
          @?= "%{\x1b[1;31m%}upstream%{\x1b[0;39m%} %{\x1b[1;31m%}\9889%{\x1b[0;39m%} "

      , testCase "Other: with no upstream" $
          testWriterWithConfig
            (zeroOutputConfig Other) addNoTrackedUpstreamIndicator
          @?= "\x1b[1;31mupstream\x1b[0;39m \x1b[1;31m\9889\x1b[0;39m "

      , testCase "TMUX: with no upstream" $
          testWriterWithConfig
            (zeroOutputConfig TMUX) addNoTrackedUpstreamIndicator
          @?= "#[fg=brightred]upstream#[fg=default] #[fg=brightred]\9889#[fg=default] "

      , testCase "NONE: with no upstream" $
          testWriterWithConfig
            (zeroOutputConfig NONE) addNoTrackedUpstreamIndicator
          @?= "upstream \9889 "
      ]
    , testGroup "Custom Config - no upstream"
      [ testCase "ZSH: with no upstream" $
          testWriterWithConfig
            (buildOutputConfig ZSH (zeroGitRepoState) customConfigNoTrackedUpstreamIndicator)
            addNoTrackedUpstreamIndicator
          @?= "%{\x1b[36m%}foo%{\x1b[0;39m%} %{\x1b[32m%}bar%{\x1b[0;39m%} "

      , testCase "Other: with no upstream" $
          testWriterWithConfig
            (buildOutputConfig Other (zeroGitRepoState) customConfigNoTrackedUpstreamIndicator)
            addNoTrackedUpstreamIndicator
          @?= "\x1b[36mfoo\x1b[0;39m \x1b[32mbar\x1b[0;39m "

      , testCase "TMUX: with no upstream" $
          testWriterWithConfig
            (buildOutputConfig TMUX (zeroGitRepoState) customConfigNoTrackedUpstreamIndicator)
            addNoTrackedUpstreamIndicator
          @?= "#[fg=cyan]foo#[fg=default] #[fg=green]bar#[fg=default] "

      , testCase "NONE: with no upstream" $
          testWriterWithConfig
            (buildOutputConfig NONE (zeroGitRepoState) customConfigNoTrackedUpstreamIndicator)
            addNoTrackedUpstreamIndicator
          @?= "foo bar "
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
        "\120366 %{\x1b[1;32m%}\8594%{\x1b[0;39m%} 2 "

    , testCase "ZSH: commits to push" $
        testMergeBranchCommitsToPush ZSH defaultConfig @?=
        "\120366 %{\x1b[1;32m%}\8592%{\x1b[0;39m%} 2 "

    , testCase "ZSH: commits to pull and to push" $
        testMergeBranchCommitsToPushAndPull ZSH defaultConfig @?=
        "\120366 4%{\x1b[1;32m%}\8644%{\x1b[0;39m%}4 "

    , testCase "Other: commits to pull" $
        testMergeBranchCommitsToPull Other defaultConfig @?=
        "\120366 \x1b[1;32m\8594\x1b[0;39m 2 "

    , testCase "Other: commits to push" $
        testMergeBranchCommitsToPush Other defaultConfig @?=
        "\120366 \x1b[1;32m\8592\x1b[0;39m 2 "

    , testCase "Other: commits to pull and to push" $
        testMergeBranchCommitsToPushAndPull Other defaultConfig @?=
        "\120366 4\x1b[1;32m\8644\x1b[0;39m4 "

    , testCase "TMUX: commits to pull" $
        testMergeBranchCommitsToPull TMUX defaultConfig @?=
        "\120366 #[fg=brightgreen]\8594#[fg=default] 2 "

    , testCase "TMUX: commits to push" $
        testMergeBranchCommitsToPush TMUX defaultConfig @?=
        "\120366 #[fg=brightgreen]\8592#[fg=default] 2 "

    , testCase "TMUX: commits to pull and to push" $
        testMergeBranchCommitsToPushAndPull TMUX defaultConfig @?=
        "\120366 4#[fg=brightgreen]\8644#[fg=default]4 "

    , testCase "NONE: commits to pull" $
        testMergeBranchCommitsToPull NONE defaultConfig @?=
        "\120366 \8594 2 "

    , testCase "NONE: commits to push" $
        testMergeBranchCommitsToPush NONE defaultConfig @?=
        "\120366 \8592 2 "

    , testCase "NONE: commits to pull and to push" $
        testMergeBranchCommitsToPushAndPull NONE defaultConfig @?=
        "\120366 4\8644\&4 "
    ]

    , testGroup "Custom Config"
        [ testCase "ZSH: commits to pull" $
            testMergeBranchCommitsToPull ZSH customConfigMergeBranchCommits @?=
            "foo %{\x1b[1;32m%}pull%{\x1b[0;39m%} 2 "

        , testCase "ZSH: commits to push" $
            testMergeBranchCommitsToPush ZSH customConfigMergeBranchCommits @?=
            "foo %{\x1b[1;32m%}push%{\x1b[0;39m%} 2 "

        , testCase "ZSH: commits to pull and to push" $
            testMergeBranchCommitsToPushAndPull ZSH customConfigMergeBranchCommits @?=
            "foo 4%{\x1b[1;32m%}pull-push%{\x1b[0;39m%}4 "

        , testCase "Other: commits to pull" $
            testMergeBranchCommitsToPull Other customConfigMergeBranchCommits @?=
            "foo \x1b[1;32mpull\x1b[0;39m 2 "

        , testCase "Other: commits to push" $
            testMergeBranchCommitsToPush Other customConfigMergeBranchCommits @?=
            "foo \x1b[1;32mpush\x1b[0;39m 2 "

        , testCase "Other: commits to pull and to push" $
            testMergeBranchCommitsToPushAndPull Other customConfigMergeBranchCommits @?=
            "foo 4\x1b[1;32mpull-push\x1b[0;39m4 "

        , testCase "TMUX: commits to pull" $
            testMergeBranchCommitsToPull TMUX customConfigMergeBranchCommits @?=
            "foo #[fg=brightgreen]pull#[fg=default] 2 "

        , testCase "TMUX: commits to push" $
            testMergeBranchCommitsToPush TMUX customConfigMergeBranchCommits @?=
            "foo #[fg=brightgreen]push#[fg=default] 2 "

        , testCase "TMUX: commits to pull and to push" $
            testMergeBranchCommitsToPushAndPull TMUX customConfigMergeBranchCommits @?=
            "foo 4#[fg=brightgreen]pull-push#[fg=default]4 "

        , testCase "NONE: commits to pull" $
            testMergeBranchCommitsToPull NONE customConfigMergeBranchCommits @?=
            "foo pull 2 "

        , testCase "NONE: commits to push" $
            testMergeBranchCommitsToPush NONE customConfigMergeBranchCommits @?=
            "foo push 2 "

        , testCase "NONE: commits to pull and to push" $
            testMergeBranchCommitsToPushAndPull NONE customConfigMergeBranchCommits @?=
            "foo 4pull-push4 "
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
          @?= "[%{\x1b[0;39m%}foo%{\x1b[0;39m%}] "

        , testCase "ZSH: should display the current commit tag if we are not on one" $
          testWriterWithConfig
            (buildOutputConfig ZSH (zeroGitRepoState { gitCommitTag = "v1.1.1" }) defaultConfig)
            addLocalBranchName
          @?= "[%{\x1b[1;33m%}detached@v1.1.1%{\x1b[0;39m%}] "

        , testCase "ZSH: should display the current commit SHA if we are not on a branch's HEAD" $
          testWriterWithConfig
            (buildOutputConfig ZSH (zeroGitRepoState { gitCommitShortSHA = "3d25ef" }) defaultConfig)
            addLocalBranchName
          @?= "[%{\x1b[1;33m%}detached@3d25ef%{\x1b[0;39m%}] "

        , testCase "ZSH: should prefer the current tag over the commit SHA if we are not on a branch's HEAD" $
          testWriterWithConfig
            (buildOutputConfig ZSH (zeroGitRepoState { gitCommitShortSHA = "3d25ef", gitCommitTag = "v1.2.3" }) defaultConfig)
            addLocalBranchName
          @?= "[%{\x1b[1;33m%}detached@v1.2.3%{\x1b[0;39m%}] "

        , testCase "Other: should display the name of the current branch if we are at the HEAD of any" $
          testWriterWithConfig
            (buildOutputConfig Other (zeroGitRepoState { gitLocalBranch = "foo" }) defaultConfig)
            addLocalBranchName
          @?= "[\x1b[0;39mfoo\x1b[0;39m] "

        , testCase "Other: should display the current commit tog if we are on one" $
          testWriterWithConfig
            (buildOutputConfig Other (zeroGitRepoState { gitCommitTag = "v1.1.1" }) defaultConfig)
            addLocalBranchName
          @?= "[\x1b[1;33mdetached@v1.1.1\x1b[0;39m] "

        , testCase "Other: should display the current commit SHA if we are not on a branch's HEAD" $
          testWriterWithConfig
            (buildOutputConfig Other (zeroGitRepoState { gitCommitShortSHA = "3d25ef" }) defaultConfig)
            addLocalBranchName
          @?= "[\x1b[1;33mdetached@3d25ef\x1b[0;39m] "

        , testCase "Other: should prefer the current tag over the commit SHA if we are not on a branch's HEAD" $
          testWriterWithConfig
            (buildOutputConfig Other (zeroGitRepoState { gitCommitShortSHA = "3d25ef", gitCommitTag = "v1.2.3" }) defaultConfig)
            addLocalBranchName
          @?= "[\x1b[1;33mdetached@v1.2.3\x1b[0;39m] "

        , testCase "TMUX: should display the name of the current branch if we are at the HEAD of any" $
          testWriterWithConfig
            (buildOutputConfig TMUX (zeroGitRepoState { gitLocalBranch = "foo" }) defaultConfig)
            addLocalBranchName
          @?= "[#[fg=default]foo#[fg=default]] "

        , testCase "TMUX: should display the current commit tog if we are on one" $
          testWriterWithConfig
            (buildOutputConfig TMUX (zeroGitRepoState { gitCommitTag = "v1.1.1" }) defaultConfig)
            addLocalBranchName
          @?= "[#[fg=brightyellow]detached@v1.1.1#[fg=default]] "

        , testCase "TMUX: should display the current commit SHA if we are not on a branch's HEAD" $
          testWriterWithConfig
            (buildOutputConfig TMUX (zeroGitRepoState { gitCommitShortSHA = "3d25ef" }) defaultConfig)
            addLocalBranchName
          @?= "[#[fg=brightyellow]detached@3d25ef#[fg=default]] "

        , testCase "TMUX: should prefer the current tag over the commit SHA if we are not on a branch's HEAD" $
          testWriterWithConfig
            (buildOutputConfig TMUX (zeroGitRepoState { gitCommitShortSHA = "3d25ef", gitCommitTag = "v1.2.3" }) defaultConfig)
            addLocalBranchName
          @?= "[#[fg=brightyellow]detached@v1.2.3#[fg=default]] "

        , testCase "NONE: should display the name of the current branch if we are at the HEAD of any" $
          testWriterWithConfig
            (buildOutputConfig NONE (zeroGitRepoState { gitLocalBranch = "foo" }) defaultConfig)
            addLocalBranchName
          @?= "[foo] "

        , testCase "NONE: should display the current commit tog if we are on one" $
          testWriterWithConfig
            (buildOutputConfig NONE (zeroGitRepoState { gitCommitTag = "v1.1.1" }) defaultConfig)
            addLocalBranchName
          @?= "[detached@v1.1.1] "

        , testCase "NONE: should display the current commit SHA if we are not on a branch's HEAD" $
          testWriterWithConfig
            (buildOutputConfig NONE (zeroGitRepoState { gitCommitShortSHA = "3d25ef" }) defaultConfig)
            addLocalBranchName
          @?= "[detached@3d25ef] "

        , testCase "NONE: should prefer the current tag over the commit SHA if we are not on a branch's HEAD" $
          testWriterWithConfig
            (buildOutputConfig NONE (zeroGitRepoState { gitCommitShortSHA = "3d25ef", gitCommitTag = "v1.2.3" }) defaultConfig)
            addLocalBranchName
          @?= "[detached@v1.2.3] "
    ]
    , testGroup "Custom Config"
    [   testCase "ZSH: should display the name of the current branch if we are at the HEAD of any" $
          testWriterWithConfig
            (buildOutputConfig ZSH (zeroGitRepoState { gitLocalBranch = "foo" }) customConfigLocalBranchName)
            addLocalBranchName
          @?= "{%{\x1b[36m%}foo%{\x1b[0;39m%}} "

        , testCase "ZSH: should display the current commit tag if we are not on one" $
          testWriterWithConfig
            (buildOutputConfig ZSH (zeroGitRepoState { gitCommitTag = "v1.1.1" }) customConfigLocalBranchName)
            addLocalBranchName
          @?= "{%{\x1b[35m%}det#!v1.1.1%{\x1b[0;39m%}} "

        , testCase "ZSH: should display the current commit SHA if we are not on a branch's HEAD" $
          testWriterWithConfig
            (buildOutputConfig ZSH (zeroGitRepoState { gitCommitShortSHA = "3d25ef" }) customConfigLocalBranchName)
            addLocalBranchName
          @?= "{%{\x1b[35m%}det#!3d25ef%{\x1b[0;39m%}} "

        , testCase "Other: should display the name of the current branch if we are at the HEAD of any" $
          testWriterWithConfig
            (buildOutputConfig Other (zeroGitRepoState { gitLocalBranch = "foo" }) customConfigLocalBranchName)
            addLocalBranchName
          @?= "{\x1b[36mfoo\x1b[0;39m} "

        , testCase "Other: should display the current commit tog if we are on one" $
          testWriterWithConfig
            (buildOutputConfig Other (zeroGitRepoState { gitCommitTag = "v1.1.1" }) customConfigLocalBranchName)
            addLocalBranchName
          @?= "{\x1b[35mdet#!v1.1.1\x1b[0;39m} "

        , testCase "Other: should display the current commit SHA if we are not on a branch's HEAD" $
          testWriterWithConfig
            (buildOutputConfig Other (zeroGitRepoState { gitCommitShortSHA = "3d25ef" }) customConfigLocalBranchName)
            addLocalBranchName
          @?= "{\x1b[35mdet#!3d25ef\x1b[0;39m} "

        , testCase "TMUX: should display the name of the current branch if we are at the HEAD of any" $
          testWriterWithConfig
            (buildOutputConfig TMUX (zeroGitRepoState { gitLocalBranch = "foo" }) customConfigLocalBranchName)
            addLocalBranchName
          @?= "{#[fg=cyan]foo#[fg=default]} "

        , testCase "TMUX: should display the current commit tog if we are on one" $
          testWriterWithConfig
            (buildOutputConfig TMUX (zeroGitRepoState { gitCommitTag = "v1.1.1" }) customConfigLocalBranchName)
            addLocalBranchName
          @?= "{#[fg=magenta]det#!v1.1.1#[fg=default]} "

        , testCase "TMUX: should display the current commit SHA if we are not on a branch's HEAD" $
          testWriterWithConfig
            (buildOutputConfig TMUX (zeroGitRepoState { gitCommitShortSHA = "3d25ef" }) customConfigLocalBranchName)
            addLocalBranchName
          @?= "{#[fg=magenta]det#!3d25ef#[fg=default]} "

        , testCase "NONE: should display the name of the current branch if we are at the HEAD of any" $
          testWriterWithConfig
            (buildOutputConfig NONE (zeroGitRepoState { gitLocalBranch = "foo" }) customConfigLocalBranchName)
            addLocalBranchName
          @?= "{foo} "

        , testCase "NONE: should display the current commit tog if we are on one" $
          testWriterWithConfig
            (buildOutputConfig NONE (zeroGitRepoState { gitCommitTag = "v1.1.1" }) customConfigLocalBranchName)
            addLocalBranchName
          @?= "{det#!v1.1.1} "

        , testCase "NONE: should display the current commit SHA if we are not on a branch's HEAD" $
          testWriterWithConfig
            (buildOutputConfig NONE (zeroGitRepoState { gitCommitShortSHA = "3d25ef" }) customConfigLocalBranchName)
            addLocalBranchName
          @?= "{det#!3d25ef} "
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
          "2%{\x1b[1;31m%}\8595%{\x1b[0;39m%} "

      , testCase "ZSH: commits to push" $
          testCommitsToPush ZSH defaultConfig @?=
          "2%{\x1b[1;32m%}\8593%{\x1b[0;39m%} "

      , testCase "ZSH: commits to pull and to push" $
          testCommitsToPushAndPull ZSH defaultConfig @?=
          "4%{\x1b[1;32m%}⥯%{\x1b[0;39m%}4 "

      , testCase "Other: commits to pull" $
          testCommitsToPull Other defaultConfig @?=
          "2\x1b[1;31m\8595\x1b[0;39m "

      , testCase "Other: commits to push" $
          testCommitsToPush Other defaultConfig @?=
          "2\x1b[1;32m\8593\x1b[0;39m "

      , testCase "Other: commits to pull and to push" $
          testCommitsToPushAndPull Other defaultConfig @?=
          "4\x1b[1;32m⥯\x1b[0;39m4 "

      , testCase "TMUX: commits to pull" $
          testCommitsToPull TMUX defaultConfig @?=
          "2#[fg=brightred]\8595#[fg=default] "

      , testCase "TMUX: commits to push" $
          testCommitsToPush TMUX defaultConfig @?=
          "2#[fg=brightgreen]\8593#[fg=default] "

      , testCase "TMUX: commits to pull and to push" $
          testCommitsToPushAndPull TMUX defaultConfig @?=
          "4#[fg=brightgreen]⥯#[fg=default]4 "

      , testCase "NONE: commits to pull" $
          testCommitsToPull NONE defaultConfig @?=
          "2\8595 "

      , testCase "NONE: commits to push" $
          testCommitsToPush NONE defaultConfig @?=
          "2\8593 "

      , testCase "NONE: commits to pull and to push" $
          testCommitsToPushAndPull NONE defaultConfig @?=
          "4⥯4 "
      ]
    , testGroup "Custom Config"
      [ testCase "ZSH: commits to pull" $
          testCommitsToPull ZSH customConfigLocalCommits @?=
          "2%{\x1b[35m%}pull%{\x1b[0;39m%} "

      , testCase "ZSH: commits to push" $
          testCommitsToPush ZSH customConfigLocalCommits @?=
          "2%{\x1b[36m%}push%{\x1b[0;39m%} "

      , testCase "ZSH: commits to pull and to push" $
          testCommitsToPushAndPull ZSH customConfigLocalCommits @?=
          "4%{\x1b[37m%}push-pull%{\x1b[0;39m%}4 "

      , testCase "Other: commits to pull" $
          testCommitsToPull Other customConfigLocalCommits @?=
          "2\x1b[35mpull\x1b[0;39m "

      , testCase "Other: commits to push" $
          testCommitsToPush Other customConfigLocalCommits @?=
          "2\x1b[36mpush\x1b[0;39m "

      , testCase "Other: commits to pull and to push" $
          testCommitsToPushAndPull Other customConfigLocalCommits @?=
          "4\x1b[37mpush-pull\x1b[0;39m4 "

      , testCase "TMUX: commits to pull" $
          testCommitsToPull TMUX customConfigLocalCommits @?=
          "2#[fg=magenta]pull#[fg=default] "

      , testCase "TMUX: commits to push" $
          testCommitsToPush TMUX customConfigLocalCommits @?=
          "2#[fg=cyan]push#[fg=default] "

      , testCase "TMUX: commits to pull and to push" $
          testCommitsToPushAndPull TMUX customConfigLocalCommits @?=
          "4#[fg=white]push-pull#[fg=default]4 "

      , testCase "NONE: commits to pull" $
          testCommitsToPull NONE customConfigLocalCommits @?=
          "2pull "

      , testCase "NONE: commits to push" $
          testCommitsToPush NONE customConfigLocalCommits @?=
          "2push "

      , testCase "NONE: commits to pull and to push" $
          testCommitsToPushAndPull NONE customConfigLocalCommits @?=
          "4push-pull4 "
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
            testLocalAddChange ZSH defaultConfig @?= "2%{\x1b[1;37m%}A%{\x1b[0;39m%} "

          , testCase "ZSH: with Local Mod Changes" $
            testLocalModChange ZSH defaultConfig @?= "2%{\x1b[1;31m%}M%{\x1b[0;39m%} "

          , testCase "ZSH: with Local Del Changes" $
            testLocalDelChange ZSH defaultConfig @?= "2%{\x1b[1;31m%}D%{\x1b[0;39m%} "

          , testCase "ZSH: with Index Add Changes" $
            testIndexAddChange ZSH defaultConfig @?= "2%{\x1b[1;32m%}A%{\x1b[0;39m%} "

          , testCase "ZSH: with Index Mod Changes" $
            testIndexModChange ZSH defaultConfig @?= "2%{\x1b[1;32m%}M%{\x1b[0;39m%} "

          , testCase "ZSH: with Index Del Changes" $
            testIndexDelChange ZSH defaultConfig @?= "2%{\x1b[1;32m%}D%{\x1b[0;39m%} "

          , testCase "ZSH: with Conflicted Changes" $
            testConflictedChange ZSH defaultConfig @?= "2%{\x1b[1;32m%}C%{\x1b[0;39m%} "

          , testCase "ZSH: with Renamed Changes" $
            testRenamedChange ZSH defaultConfig @?= "2%{\x1b[1;32m%}R%{\x1b[0;39m%} "

          , testCase "ZSH: with every kind of Changes" $
            testEveryRepoChange ZSH defaultConfig @?= "6%{\x1b[1;32m%}A%{\x1b[0;39m%}8%{\x1b[1;32m%}D%{\x1b[0;39m%}7%{\x1b[1;32m%}M%{\x1b[0;39m%}1%{\x1b[1;32m%}R%{\x1b[0;39m%} 5%{\x1b[1;31m%}D%{\x1b[0;39m%}4%{\x1b[1;31m%}M%{\x1b[0;39m%} 3%{\x1b[1;37m%}A%{\x1b[0;39m%} 2%{\x1b[1;32m%}C%{\x1b[0;39m%} "

          , testCase "Other: with Local Add Changes" $
            testLocalAddChange Other defaultConfig @?= "2\x1b[1;37mA\x1b[0;39m "

          , testCase "Other: with Local Mod Changes" $
            testLocalModChange Other defaultConfig @?= "2\x1b[1;31mM\x1b[0;39m "

          , testCase "Other: with Local Del Changes" $
            testLocalDelChange Other defaultConfig @?= "2\x1b[1;31mD\x1b[0;39m "

          , testCase "Other: with Index Add Changes" $
            testIndexAddChange Other defaultConfig @?= "2\x1b[1;32mA\x1b[0;39m "

          , testCase "Other: with Index Mod Changes" $
            testIndexModChange Other defaultConfig @?= "2\x1b[1;32mM\x1b[0;39m "

          , testCase "Other: with Index Del Changes" $
            testIndexDelChange Other defaultConfig @?= "2\x1b[1;32mD\x1b[0;39m "

          , testCase "Other: with Conflicted Changes" $
            testConflictedChange Other defaultConfig @?= "2\x1b[1;32mC\x1b[0;39m "

          , testCase "Other: with Renamed Changes" $
            testRenamedChange Other defaultConfig @?= "2\x1b[1;32mR\x1b[0;39m "

          , testCase "Other: with every kind of Changes" $
            testEveryRepoChange Other defaultConfig @?= "6\x1b[1;32mA\x1b[0;39m8\x1b[1;32mD\x1b[0;39m7\x1b[1;32mM\x1b[0;39m1\x1b[1;32mR\x1b[0;39m 5\x1b[1;31mD\x1b[0;39m4\x1b[1;31mM\x1b[0;39m 3\x1b[1;37mA\x1b[0;39m 2\x1b[1;32mC\x1b[0;39m "

          , testCase "TMUX: with Local Add Changes" $
            testLocalAddChange TMUX defaultConfig @?= "2#[fg=brightwhite]A#[fg=default] "

          , testCase "TMUX: with Local Mod Changes" $
            testLocalModChange TMUX defaultConfig @?= "2#[fg=brightred]M#[fg=default] "

          , testCase "TMUX: with Local Del Changes" $
            testLocalDelChange TMUX defaultConfig @?= "2#[fg=brightred]D#[fg=default] "

          , testCase "TMUX: with Index Add Changes" $
            testIndexAddChange TMUX defaultConfig @?= "2#[fg=brightgreen]A#[fg=default] "

          , testCase "TMUX: with Index Mod Changes" $
            testIndexModChange TMUX defaultConfig @?= "2#[fg=brightgreen]M#[fg=default] "

          , testCase "TMUX: with Index Del Changes" $
            testIndexDelChange TMUX defaultConfig @?= "2#[fg=brightgreen]D#[fg=default] "

          , testCase "TMUX: with Conflicted Changes" $
            testConflictedChange TMUX defaultConfig @?= "2#[fg=brightgreen]C#[fg=default] "

          , testCase "TMUX: with Renamed Changes" $
            testRenamedChange TMUX defaultConfig @?= "2#[fg=brightgreen]R#[fg=default] "

          , testCase "TMUX: with every kind of Changes" $
            testEveryRepoChange TMUX defaultConfig @?= "6#[fg=brightgreen]A#[fg=default]8#[fg=brightgreen]D#[fg=default]7#[fg=brightgreen]M#[fg=default]1#[fg=brightgreen]R#[fg=default] 5#[fg=brightred]D#[fg=default]4#[fg=brightred]M#[fg=default] 3#[fg=brightwhite]A#[fg=default] 2#[fg=brightgreen]C#[fg=default] "

          , testCase "NONE: with Local Add Changes" $
            testLocalAddChange NONE defaultConfig @?= "2A "

          , testCase "NONE: with Local Mod Changes" $
            testLocalModChange NONE defaultConfig @?= "2M "

          , testCase "NONE: with Local Del Changes" $
            testLocalDelChange NONE defaultConfig @?= "2D "

          , testCase "NONE: with Index Add Changes" $
            testIndexAddChange NONE defaultConfig @?= "2A "

          , testCase "NONE: with Index Mod Changes" $
            testIndexModChange NONE defaultConfig @?= "2M "

          , testCase "NONE: with Index Del Changes" $
            testIndexDelChange NONE defaultConfig @?= "2D "

          , testCase "NONE: with Conflicted Changes" $
            testConflictedChange NONE defaultConfig @?= "2C "

          , testCase "NONE: with Renamed Changes" $
            testRenamedChange NONE defaultConfig @?= "2R "

          , testCase "NONE: with every kind of Changes" $
            testEveryRepoChange NONE defaultConfig @?= "6A8D7M1R 5D4M 3A 2C "
        ]
    , testGroup "Custom Config"
        [ testCase "ZSH: with Local Add Changes" $
            testLocalAddChange ZSH customChangeConfig @?= "2%{\x1b[35m%}B%{\x1b[0;39m%} "

          , testCase "ZSH: with Local Mod Changes" $
            testLocalModChange ZSH customChangeConfig @?= "2%{\x1b[34m%}N%{\x1b[0;39m%} "

          , testCase "ZSH: with Local Del Changes" $
            testLocalDelChange ZSH customChangeConfig @?= "2%{\x1b[34m%}E%{\x1b[0;39m%} "

          , testCase "ZSH: with Index Add Changes" $
            testIndexAddChange ZSH customChangeConfig @?= "2%{\x1b[36m%}B%{\x1b[0;39m%} "

          , testCase "ZSH: with Index Mod Changes" $
            testIndexModChange ZSH customChangeConfig @?= "2%{\x1b[36m%}N%{\x1b[0;39m%} "

          , testCase "ZSH: with Index Del Changes" $
            testIndexDelChange ZSH customChangeConfig @?= "2%{\x1b[36m%}E%{\x1b[0;39m%} "

          , testCase "ZSH: with Conflicted Changes" $
            testConflictedChange ZSH customChangeConfig @?= "2%{\x1b[36m%}D%{\x1b[0;39m%} "

          , testCase "ZSH: with Renamed Changes" $
            testRenamedChange ZSH customChangeConfig @?= "2%{\x1b[36m%}S%{\x1b[0;39m%} "

          , testCase "ZSH: with every kind of Changes" $
            testEveryRepoChange ZSH customChangeConfig @?= "6%{\x1b[36m%}B%{\x1b[0;39m%}8%{\x1b[36m%}E%{\x1b[0;39m%}7%{\x1b[36m%}N%{\x1b[0;39m%}1%{\x1b[36m%}S%{\x1b[0;39m%} 5%{\x1b[34m%}E%{\x1b[0;39m%}4%{\x1b[34m%}N%{\x1b[0;39m%} 3%{\x1b[35m%}B%{\x1b[0;39m%} 2%{\x1b[36m%}D%{\x1b[0;39m%} "

          , testCase "Other: with Local Add Changes" $
            testLocalAddChange Other customChangeConfig @?= "2\x1b[35mB\x1b[0;39m "

          , testCase "Other: with Local Mod Changes" $
            testLocalModChange Other customChangeConfig @?= "2\x1b[34mN\x1b[0;39m "

          , testCase "Other: with Local Del Changes" $
            testLocalDelChange Other customChangeConfig @?= "2\x1b[34mE\x1b[0;39m "

          , testCase "Other: with Index Add Changes" $
            testIndexAddChange Other customChangeConfig @?= "2\x1b[36mB\x1b[0;39m "

          , testCase "Other: with Index Mod Changes" $
            testIndexModChange Other customChangeConfig @?= "2\x1b[36mN\x1b[0;39m "

          , testCase "Other: with Index Del Changes" $
            testIndexDelChange Other customChangeConfig @?= "2\x1b[36mE\x1b[0;39m "

          , testCase "Other: with Conflicted Changes" $
            testConflictedChange Other customChangeConfig @?= "2\x1b[36mD\x1b[0;39m "

          , testCase "Other: with Renamed Changes" $
            testRenamedChange Other customChangeConfig @?= "2\x1b[36mS\x1b[0;39m "

          , testCase "Other: with every kind of Changes" $
            testEveryRepoChange Other customChangeConfig @?= "6\x1b[36mB\x1b[0;39m8\x1b[36mE\x1b[0;39m7\x1b[36mN\x1b[0;39m1\x1b[36mS\x1b[0;39m 5\x1b[34mE\x1b[0;39m4\x1b[34mN\x1b[0;39m 3\x1b[35mB\x1b[0;39m 2\x1b[36mD\x1b[0;39m "

          , testCase "TMUX: with Local Add Changes" $
            testLocalAddChange TMUX customChangeConfig @?= "2#[fg=magenta]B#[fg=default] "

          , testCase "TMUX: with Local Mod Changes" $
            testLocalModChange TMUX customChangeConfig @?= "2#[fg=blue]N#[fg=default] "

          , testCase "TMUX: with Local Del Changes" $
            testLocalDelChange TMUX customChangeConfig @?= "2#[fg=blue]E#[fg=default] "

          , testCase "TMUX: with Index Add Changes" $
            testIndexAddChange TMUX customChangeConfig @?= "2#[fg=cyan]B#[fg=default] "

          , testCase "TMUX: with Index Mod Changes" $
            testIndexModChange TMUX customChangeConfig @?= "2#[fg=cyan]N#[fg=default] "

          , testCase "TMUX: with Index Del Changes" $
            testIndexDelChange TMUX customChangeConfig @?= "2#[fg=cyan]E#[fg=default] "

          , testCase "TMUX: with Conflicted Changes" $
            testConflictedChange TMUX customChangeConfig @?= "2#[fg=cyan]D#[fg=default] "

          , testCase "TMUX: with Renamed Changes" $
            testRenamedChange TMUX customChangeConfig @?= "2#[fg=cyan]S#[fg=default] "

          , testCase "TMUX: with every kind of Changes" $
            testEveryRepoChange TMUX customChangeConfig @?= "6#[fg=cyan]B#[fg=default]8#[fg=cyan]E#[fg=default]7#[fg=cyan]N#[fg=default]1#[fg=cyan]S#[fg=default] 5#[fg=blue]E#[fg=default]4#[fg=blue]N#[fg=default] 3#[fg=magenta]B#[fg=default] 2#[fg=cyan]D#[fg=default] "

          , testCase "NONE: with Local Add Changes" $
            testLocalAddChange NONE customChangeConfig @?= "2B "

          , testCase "NONE: with Local Mod Changes" $
            testLocalModChange NONE customChangeConfig @?= "2N "

          , testCase "NONE: with Local Del Changes" $
            testLocalDelChange NONE customChangeConfig @?= "2E "

          , testCase "NONE: with Index Add Changes" $
            testIndexAddChange NONE customChangeConfig @?= "2B "

          , testCase "NONE: with Index Mod Changes" $
            testIndexModChange NONE customChangeConfig @?= "2N "

          , testCase "NONE: with Index Del Changes" $
            testIndexDelChange NONE customChangeConfig @?= "2E "

          , testCase "NONE: with Conflicted Changes" $
            testConflictedChange NONE customChangeConfig @?= "2D "

          , testCase "NONE: with Renamed Changes" $
            testRenamedChange NONE customChangeConfig @?= "2S "

          , testCase "NONE: with every kind of Changes" $
            testEveryRepoChange NONE customChangeConfig @?= "6B8E7N1S 5E4N 3B 2D "
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
          @?= "2%{\x1b[1;32m%}\8801%{\x1b[0;39m%} "

        , testCase "Other: hardcoded character" $
          testWriterWithConfig
            (buildOutputConfig Other (zeroGitRepoState { gitStashCount = 2 }) defaultConfig) addStashes
          @?= "2\x1b[1;32m\8801\x1b[0;39m "

        , testCase "TMUX: hardcoded character" $
          testWriterWithConfig
            (buildOutputConfig TMUX (zeroGitRepoState { gitStashCount = 2 }) defaultConfig) addStashes
          @?= "2#[fg=brightgreen]\8801#[fg=default] "

        , testCase "NONE: hardcoded character" $
          testWriterWithConfig
            (buildOutputConfig NONE (zeroGitRepoState { gitStashCount = 2 }) defaultConfig) addStashes
          @?= "2\8801 "
      ]
  , testGroup "Custom Config"
      [ testCase "ZSH: hardcoded character" $
          testWriterWithConfig
            (buildOutputConfig ZSH (zeroGitRepoState { gitStashCount = 2 }) customStashConfig) addStashes
          @?= "2%{\x1b[36m%}stash%{\x1b[0;39m%} "

        , testCase "Other: hardcoded character" $
          testWriterWithConfig
            (buildOutputConfig Other (zeroGitRepoState { gitStashCount = 2 }) customStashConfig) addStashes
          @?= "2\x1b[36mstash\x1b[0;39m "

        , testCase "TMUX: hardcoded character" $
          testWriterWithConfig
            (buildOutputConfig TMUX (zeroGitRepoState { gitStashCount = 2 }) customStashConfig) addStashes
          @?= "2#[fg=cyan]stash#[fg=default] "

        , testCase "NONE: hardcoded character" $
          testWriterWithConfig
            (buildOutputConfig NONE (zeroGitRepoState { gitStashCount = 2 }) customStashConfig) addStashes
          @?= "2stash "
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
    {- The branch supercedes both the commit SHA and the tag -}
    , gitCommitShortSHA = "3de6ef"
    , gitCommitTag = "v1.3"
    , gitRemote = "origin"
    , gitRemoteTrackingBranch = "origin/branch"
    , gitStashCount = 3
    , gitCommitsToPull = 5
    , gitCommitsToPush = 6
    , gitMergeBranchCommitsToPull = 2
    , gitMergeBranchCommitsToPush = 1
  }

{- For reference here, the full prompt would be

 "%{\ESC[0;39m%}\5812 \120366 2%{\ESC[1;32m%}\8644%{\ESC[0;39m%}1 [%{\ESC[0;39m%}branch%{\ESC[0;39m%}] 5%{\ESC[1;32m%}⥯%{\ESC[0;39m%}6 5%{\ESC[1;32m%}A%{\ESC[0;39m%}6%{\ESC[1;32m%}D%{\ESC[0;39m%}4%{\ESC[1;32m%}M%{\ESC[0;39m%}7%{\ESC[1;32m%}R%{\ESC[0;39m%} 3%{\ESC[1;31m%}D%{\ESC[0;39m%}1%{\ESC[1;31m%}M%{\ESC[0;39m%} 2%{\ESC[1;37m%}A%{\ESC[0;39m%} 8%{\ESC[1;32m%}C%{\ESC[0;39m%} 3%{\ESC[1;32m%}\8801%{\ESC[0;39m%} "

-}
testPartialPrompt :: TestTree
testPartialPrompt = testGroup "Partial prompt display"
  [   testCase "w/out repo indicator" $
        testWriterWithConfig
          (buildOutputConfig ZSH repoStateForPartialPrompt defaultConfig { confShowPartRepoIndicator = False })
          buildPrompt
        @?= "%{\ESC[0;39m%}\120366 2%{\ESC[1;32m%}\8644%{\ESC[0;39m%}1 [%{\ESC[0;39m%}branch%{\ESC[0;39m%}] 5%{\ESC[1;32m%}⥯%{\ESC[0;39m%}6 5%{\ESC[1;32m%}A%{\ESC[0;39m%}6%{\ESC[1;32m%}D%{\ESC[0;39m%}4%{\ESC[1;32m%}M%{\ESC[0;39m%}7%{\ESC[1;32m%}R%{\ESC[0;39m%} 3%{\ESC[1;31m%}D%{\ESC[0;39m%}1%{\ESC[1;31m%}M%{\ESC[0;39m%} 2%{\ESC[1;37m%}A%{\ESC[0;39m%} 8%{\ESC[1;32m%}C%{\ESC[0;39m%} 3%{\ESC[1;32m%}\8801%{\ESC[0;39m%} "

    , testCase "w/out merge branch commits info" $
        testWriterWithConfig
          (buildOutputConfig ZSH repoStateForPartialPrompt defaultConfig { confShowPartMergeBranchCommitsDiff = False })
          buildPrompt
        @?= "%{\ESC[0;39m%}\5812 [%{\ESC[0;39m%}branch%{\ESC[0;39m%}] 5%{\ESC[1;32m%}⥯%{\ESC[0;39m%}6 5%{\ESC[1;32m%}A%{\ESC[0;39m%}6%{\ESC[1;32m%}D%{\ESC[0;39m%}4%{\ESC[1;32m%}M%{\ESC[0;39m%}7%{\ESC[1;32m%}R%{\ESC[0;39m%} 3%{\ESC[1;31m%}D%{\ESC[0;39m%}1%{\ESC[1;31m%}M%{\ESC[0;39m%} 2%{\ESC[1;37m%}A%{\ESC[0;39m%} 8%{\ESC[1;32m%}C%{\ESC[0;39m%} 3%{\ESC[1;32m%}\8801%{\ESC[0;39m%} "

    , testCase "w/out local branch info" $
        testWriterWithConfig
          (buildOutputConfig ZSH repoStateForPartialPrompt defaultConfig { confShowPartLocalBranch = False })
          buildPrompt
        @?= "%{\ESC[0;39m%}\5812 \120366 2%{\ESC[1;32m%}\8644%{\ESC[0;39m%}1 5%{\ESC[1;32m%}⥯%{\ESC[0;39m%}6 5%{\ESC[1;32m%}A%{\ESC[0;39m%}6%{\ESC[1;32m%}D%{\ESC[0;39m%}4%{\ESC[1;32m%}M%{\ESC[0;39m%}7%{\ESC[1;32m%}R%{\ESC[0;39m%} 3%{\ESC[1;31m%}D%{\ESC[0;39m%}1%{\ESC[1;31m%}M%{\ESC[0;39m%} 2%{\ESC[1;37m%}A%{\ESC[0;39m%} 8%{\ESC[1;32m%}C%{\ESC[0;39m%} 3%{\ESC[1;32m%}\8801%{\ESC[0;39m%} "

    , testCase "with a branch set to ignore its merge branch" $
        testWriterWithConfig
          (buildOutputConfig ZSH repoStateForPartialPrompt defaultConfig { confMergeBranchIgnoreBranches = ["branch"] })
          buildPrompt
        @?= "%{\ESC[0;39m%}\5812 [%{\ESC[0;39m%}branch%{\ESC[0;39m%}] 5%{\ESC[1;32m%}⥯%{\ESC[0;39m%}6 5%{\ESC[1;32m%}A%{\ESC[0;39m%}6%{\ESC[1;32m%}D%{\ESC[0;39m%}4%{\ESC[1;32m%}M%{\ESC[0;39m%}7%{\ESC[1;32m%}R%{\ESC[0;39m%} 3%{\ESC[1;31m%}D%{\ESC[0;39m%}1%{\ESC[1;31m%}M%{\ESC[0;39m%} 2%{\ESC[1;37m%}A%{\ESC[0;39m%} 8%{\ESC[1;32m%}C%{\ESC[0;39m%} 3%{\ESC[1;32m%}\8801%{\ESC[0;39m%} "

    , testCase "w/out commits push/pull info" $
        testWriterWithConfig
          (buildOutputConfig ZSH repoStateForPartialPrompt defaultConfig { confShowPartCommitsToOrigin = False })
          buildPrompt
        @?= "%{\ESC[0;39m%}\5812 \120366 2%{\ESC[1;32m%}\8644%{\ESC[0;39m%}1 [%{\ESC[0;39m%}branch%{\ESC[0;39m%}] 5%{\ESC[1;32m%}A%{\ESC[0;39m%}6%{\ESC[1;32m%}D%{\ESC[0;39m%}4%{\ESC[1;32m%}M%{\ESC[0;39m%}7%{\ESC[1;32m%}R%{\ESC[0;39m%} 3%{\ESC[1;31m%}D%{\ESC[0;39m%}1%{\ESC[1;31m%}M%{\ESC[0;39m%} 2%{\ESC[1;37m%}A%{\ESC[0;39m%} 8%{\ESC[1;32m%}C%{\ESC[0;39m%} 3%{\ESC[1;32m%}\8801%{\ESC[0;39m%} "

    , testCase "w/out local repo changes" $
        testWriterWithConfig
          (buildOutputConfig ZSH repoStateForPartialPrompt defaultConfig { confShowPartLocalChangesState = False })
          buildPrompt
        @?= "%{\ESC[0;39m%}\5812 \120366 2%{\ESC[1;32m%}\8644%{\ESC[0;39m%}1 [%{\ESC[0;39m%}branch%{\ESC[0;39m%}] 5%{\ESC[1;32m%}⥯%{\ESC[0;39m%}6 3%{\ESC[1;32m%}\8801%{\ESC[0;39m%} "

    , testCase "w/out stashes" $
        testWriterWithConfig
          (buildOutputConfig ZSH repoStateForPartialPrompt defaultConfig { confShowPartStashes = False })
          buildPrompt
        @?= "%{\ESC[0;39m%}\5812 \120366 2%{\ESC[1;32m%}\8644%{\ESC[0;39m%}1 [%{\ESC[0;39m%}branch%{\ESC[0;39m%}] 5%{\ESC[1;32m%}⥯%{\ESC[0;39m%}6 5%{\ESC[1;32m%}A%{\ESC[0;39m%}6%{\ESC[1;32m%}D%{\ESC[0;39m%}4%{\ESC[1;32m%}M%{\ESC[0;39m%}7%{\ESC[1;32m%}R%{\ESC[0;39m%} 3%{\ESC[1;31m%}D%{\ESC[0;39m%}1%{\ESC[1;31m%}M%{\ESC[0;39m%} 2%{\ESC[1;37m%}A%{\ESC[0;39m%} 8%{\ESC[1;32m%}C%{\ESC[0;39m%} "
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
