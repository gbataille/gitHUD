module Test.GitHUD.Config.Parse (
  configParserTests
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import Text.Parsec (parse)
import Text.Parsec.String (Parser)

import GitHUD.Config.Parse
import GitHUD.Config.Types
import GitHUD.Terminal.Types

configParserTests :: TestTree
configParserTests = testGroup "Config Parser Test"
  [ testItemParser
    , testCommentParser
    , testConfigItemFolder
    , testColorConfigToColor
    , testIntensityConfigToIntensity
  ]

testItemParser :: TestTree
testItemParser = testGroup "#itemParser"
  [ testCase "properly formed config item" $
      utilConfigItemParser itemParser "some_test_key=some Complex ⚡ value"
      @?= Item "some_test_key" "some Complex ⚡ value"

    , testCase "dash characters are not allowed in keys" $
        utilConfigItemParser itemParser "some-key=dash"
        @?= ErrorLine

    , testCase "num characters are not allowed in keys" $
        utilConfigItemParser itemParser "some123=dash"
        @?= ErrorLine

    , testCase "empty keys are not allowed" $
        utilConfigItemParser itemParser "=dash"
        @?= ErrorLine

    , testCase "Comment should not work" $
        utilConfigItemParser itemParser "#some comment"
        @?= ErrorLine
  ]

testCommentParser :: TestTree
testCommentParser = testGroup "#commentParser"
  [ testCase "proper comment" $
      utilConfigItemParser commentParser "#some comment\n"
      @?= Comment

    , testCase "not a comment if start with a space" $
        utilConfigItemParser commentParser " #some non comment\n"
        @?= ErrorLine
  ]

testConfigItemFolder :: TestTree
testConfigItemFolder = testGroup "#configItemFolder"
  [   testCase "Comment should have no impact on the config" $
        configItemsFolder defaultConfig (Comment)
        @?= defaultConfig

    , testCase "ErrorLines should have no impact on the config" $
        configItemsFolder defaultConfig (ErrorLine)
        @?= defaultConfig

    , testCase "Key: git_repo_indicator" $
        expectValue "foo" $
          toBeInField confRepoIndicator $
            forConfigItemKey "git_repo_indicator" $
              withValue "foo"

    , testCase "Key: no_upstream_text" $
        expectValue "foo" $
          toBeInField confNoUpstreamString $
            forConfigItemKey "no_upstream_text" $
              withValue "foo"

    , testCase "Key: no_upstream_text_color" $
        expectValue Green $
          toBeInField confNoUpstreamStringColor $
            forConfigItemKey "no_upstream_text_color" $
              withValue "Green"

    , testCase "Key: no_upstream_text_intensity" $
        expectValue Dull $
          toBeInField confNoUpstreamStringIntensity $
            forConfigItemKey "no_upstream_text_intensity" $
              withValue "Dull"

    , testCase "Key: no_upstream_indicator" $
        expectValue "foo" $
          toBeInField confNoUpstreamIndicator $
            forConfigItemKey "no_upstream_indicator" $
              withValue "foo"

    , testCase "Key: no_upstream_indicator_color" $
        expectValue Black $
          toBeInField confNoUpstreamIndicatorColor $
            forConfigItemKey "no_upstream_indicator_color" $
              withValue "Black"

    , testCase "Key: no_upstream_indicator_color - invalid color" $
        expectValue Blue $
          toBeInField confNoUpstreamIndicatorColor $
            forConfigItemKey "no_upstream_indicator_color" $
              withValue "FOO"

    , testCase "Key: no_upstream_indicator_intensity" $
        expectValue Dull $
          toBeInField confNoUpstreamIndicatorIntensity $
            forConfigItemKey "no_upstream_indicator_intensity" $
              withValue "Dull"

    , testCase "Key: no_upstream_indicator_intensity - invalid intensity" $
        expectValue Vivid $
          toBeInField confNoUpstreamIndicatorIntensity $
            forConfigItemKey "no_upstream_indicator_intensity" $
              withValue "FOO"

    , testCase "Key: remote_commits_indicator" $
        expectValue "FOO" $
          toBeInField confRemoteCommitsIndicator $
            forConfigItemKey "remote_commits_indicator" $
              withValue "FOO"

    , testCase "Key: remote_commits_pull_prefix" $
        expectValue "FOO" $
          toBeInField confRemoteCommitsOnlyPull $
            forConfigItemKey "remote_commits_pull_prefix" $
              withValue "FOO"

    , testCase "Key: remote_commits_push_prefix" $
        expectValue "FOO" $
          toBeInField confRemoteCommitsOnlyPush $
            forConfigItemKey "remote_commits_push_prefix" $
              withValue "FOO"

    , testCase "Key: remote_commits_push_pull_infix" $
        expectValue "FOO" $
          toBeInField confRemoteCommitsBothPullPush $
            forConfigItemKey "remote_commits_push_pull_infix" $
              withValue "FOO"

    , testCase "Key: local_branch_prefix" $
        expectValue "FOO" $
          toBeInField confLocalBranchNamePrefix $
            forConfigItemKey "local_branch_prefix" $
              withValue "FOO"

    , testCase "Key: local_branch_suffix" $
        expectValue "FOO" $
          toBeInField confLocalBranchNameSuffix $
            forConfigItemKey "local_branch_suffix" $
              withValue "FOO"

    , testCase "Key: local_branch_color" $
        expectValue Cyan $
          toBeInField confLocalBranchColor $
            forConfigItemKey "local_branch_color" $
              withValue "Cyan"

    , testCase "Key: local_branch_intensity" $
        expectValue Dull $
          toBeInField confLocalBranchIntensity $
            forConfigItemKey "local_branch_intensity" $
              withValue "Dull"

    , testCase "Key: local_detached_prefix" $
        expectValue "FOO" $
          toBeInField confLocalDetachedPrefix $
            forConfigItemKey "local_detached_prefix" $
              withValue "FOO"

    , testCase "Key: local_detached_color" $
        expectValue Cyan $
          toBeInField confLocalDetachedColor $
            forConfigItemKey "local_detached_color" $
              withValue "Cyan"

    , testCase "Key: local_detached_intensity" $
        expectValue Dull $
          toBeInField confLocalDetachedIntensity $
            forConfigItemKey "local_detached_intensity" $
              withValue "Dull"

    , testCase "Key: local_commits_push_suffix" $
        expectValue "FOO" $
          toBeInField confLocalCommitsPushSuffix $
            forConfigItemKey "local_commits_push_suffix" $
              withValue "FOO"

    , testCase "Key: local_commits_push_suffix_color" $
        expectValue Cyan $
          toBeInField confLocalCommitsPushSuffixColor $
            forConfigItemKey "local_commits_push_suffix_color" $
              withValue "Cyan"

    , testCase "Key: local_commits_push_suffix_intensity" $
        expectValue Dull $
          toBeInField confLocalCommitsPushSuffixIntensity $
            forConfigItemKey "local_commits_push_suffix_intensity" $
              withValue "Dull"

    , testCase "Key: local_commits_pull_suffix" $
        expectValue "FOO" $
          toBeInField confLocalCommitsPullSuffix $
            forConfigItemKey "local_commits_pull_suffix" $
              withValue "FOO"

    , testCase "Key: local_commits_pull_suffix_color" $
        expectValue Cyan $
          toBeInField confLocalCommitsPullSuffixColor $
            forConfigItemKey "local_commits_pull_suffix_color" $
              withValue "Cyan"

    , testCase "Key: local_commits_pull_suffix_intensity" $
        expectValue Dull $
          toBeInField confLocalCommitsPullSuffixIntensity $
            forConfigItemKey "local_commits_pull_suffix_intensity" $
              withValue "Dull"

    , testCase "Key: local_commits_push_pull_infix" $
        expectValue "FOO" $
          toBeInField confLocalCommitsPushPullInfix $
            forConfigItemKey "local_commits_push_pull_infix" $
              withValue "FOO"

    , testCase "Key: local_commits_push_pull_infix_color" $
        expectValue Cyan $
          toBeInField confLocalCommitsPushPullInfixColor $
            forConfigItemKey "local_commits_push_pull_infix_color" $
              withValue "Cyan"

    , testCase "Key: local_commits_push_pull_infix_intensity" $
        expectValue Dull $
          toBeInField confLocalCommitsPushPullInfixIntensity $
            forConfigItemKey "local_commits_push_pull_infix_intensity" $
              withValue "Dull"

    , testCase "Key: change_index_add_suffix" $
        expectValue "FOO" $
          toBeInField confChangeIndexAddSuffix $
            forConfigItemKey "change_index_add_suffix" $
              withValue "FOO"

    , testCase "Key: change_index_add_suffix_color" $
        expectValue Cyan $
          toBeInField confChangeIndexAddSuffixColor $
            forConfigItemKey "change_index_add_suffix_color" $
              withValue "Cyan"

    , testCase "Key: change_index_add_suffix_intensity" $
        expectValue Dull $
          toBeInField confChangeIndexAddSuffixIntensity $
            forConfigItemKey "change_index_add_suffix_intensity" $
              withValue "Dull"

    , testCase "Key: change_index_mod_suffix" $
        expectValue "FOO" $
          toBeInField confChangeIndexModSuffix $
            forConfigItemKey "change_index_mod_suffix" $
              withValue "FOO"

    , testCase "Key: change_index_mod_suffix_color" $
        expectValue Cyan $
          toBeInField confChangeIndexModSuffixColor $
            forConfigItemKey "change_index_mod_suffix_color" $
              withValue "Cyan"

    , testCase "Key: change_index_mod_suffix_intensity" $
        expectValue Dull $
          toBeInField confChangeIndexModSuffixIntensity $
            forConfigItemKey "change_index_mod_suffix_intensity" $
              withValue "Dull"

    , testCase "Key: change_index_del_suffix" $
        expectValue "FOO" $
          toBeInField confChangeIndexDelSuffix $
            forConfigItemKey "change_index_del_suffix" $
              withValue "FOO"

    , testCase "Key: change_index_del_suffix_color" $
        expectValue Cyan $
          toBeInField confChangeIndexDelSuffixColor $
            forConfigItemKey "change_index_del_suffix_color" $
              withValue "Cyan"

    , testCase "Key: change_index_del_suffix_intensity" $
        expectValue Dull $
          toBeInField confChangeIndexDelSuffixIntensity $
            forConfigItemKey "change_index_del_suffix_intensity" $
              withValue "Dull"

    , testCase "Key: change_local_add_suffix" $
        expectValue "FOO" $
          toBeInField confChangeLocalAddSuffix $
            forConfigItemKey "change_local_add_suffix" $
              withValue "FOO"

    , testCase "Key: change_local_add_suffix_color" $
        expectValue Cyan $
          toBeInField confChangeLocalAddSuffixColor $
            forConfigItemKey "change_local_add_suffix_color" $
              withValue "Cyan"

    , testCase "Key: change_local_add_suffix_intensity" $
        expectValue Dull $
          toBeInField confChangeLocalAddSuffixIntensity $
            forConfigItemKey "change_local_add_suffix_intensity" $
              withValue "Dull"

    , testCase "Key: change_local_mod_suffix" $
        expectValue "FOO" $
          toBeInField confChangeLocalModSuffix $
            forConfigItemKey "change_local_mod_suffix" $
              withValue "FOO"

    , testCase "Key: change_local_mod_suffix_color" $
        expectValue Cyan $
          toBeInField confChangeLocalModSuffixColor $
            forConfigItemKey "change_local_mod_suffix_color" $
              withValue "Cyan"

    , testCase "Key: change_local_mod_suffix_intensity" $
        expectValue Dull $
          toBeInField confChangeLocalModSuffixIntensity $
            forConfigItemKey "change_local_mod_suffix_intensity" $
              withValue "Dull"

    , testCase "Key: change_local_del_suffix" $
        expectValue "FOO" $
          toBeInField confChangeLocalDelSuffix $
            forConfigItemKey "change_local_del_suffix" $
              withValue "FOO"

    , testCase "Key: change_local_del_suffix_color" $
        expectValue Cyan $
          toBeInField confChangeLocalDelSuffixColor $
            forConfigItemKey "change_local_del_suffix_color" $
              withValue "Cyan"

    , testCase "Key: change_local_del_suffix_intensity" $
        expectValue Dull $
          toBeInField confChangeLocalDelSuffixIntensity $
            forConfigItemKey "change_local_del_suffix_intensity" $
              withValue "Dull"

    , testCase "Key: change_renamed_suffix" $
        expectValue "FOO" $
          toBeInField confChangeRenamedSuffix $
            forConfigItemKey "change_renamed_suffix" $
              withValue "FOO"

    , testCase "Key: change_renamed_suffix_color" $
        expectValue Cyan $
          toBeInField confChangeRenamedSuffixColor $
            forConfigItemKey "change_renamed_suffix_color" $
              withValue "Cyan"

    , testCase "Key: change_renamed_suffix_intensity" $
        expectValue Dull $
          toBeInField confChangeRenamedSuffixIntensity $
            forConfigItemKey "change_renamed_suffix_intensity" $
              withValue "Dull"

    , testCase "Key: change_conflicted_suffix" $
        expectValue "FOO" $
          toBeInField confChangeConflictedSuffix $
            forConfigItemKey "change_conflicted_suffix" $
              withValue "FOO"

    , testCase "Key: change_conflicted_suffix_color" $
        expectValue Cyan $
          toBeInField confChangeConflictedSuffixColor $
            forConfigItemKey "change_conflicted_suffix_color" $
              withValue "Cyan"

    , testCase "Key: change_conflicted_suffix_intensity" $
        expectValue Dull $
          toBeInField confChangeConflictedSuffixIntensity $
            forConfigItemKey "change_conflicted_suffix_intensity" $
              withValue "Dull"

    , testCase "Key: stash_suffix" $
        expectValue "FOO" $
          toBeInField confStashSuffix $
            forConfigItemKey "stash_suffix" $
              withValue "FOO"

    , testCase "Key: stash_suffix_color" $
        expectValue Cyan $
          toBeInField confStashSuffixColor $
            forConfigItemKey "stash_suffix_color" $
              withValue "Cyan"

    , testCase "Key: stash_suffix_intensity" $
        expectValue Dull $
          toBeInField confStashSuffixIntensity $
            forConfigItemKey "stash_suffix_intensity" $
              withValue "Dull"
  ]

expectValue :: (Eq a, Show a) => a -> a -> Assertion
expectValue expected actual = actual @?= expected

toBeInField :: (Config -> a) -> Config -> a
toBeInField accessor config = accessor config

forConfigItemKey :: String -> String -> Config
forConfigItemKey key value =
  configItemsFolder defaultConfig (Item key value)

withValue :: a -> a
withValue = id

utilConfigItemParser :: Parser ConfigItem -> String -> ConfigItem
utilConfigItemParser parser str =
  either
    (const ErrorLine)
    id
    (parse parser "" str)

testIntensityConfigToIntensity :: TestTree
testIntensityConfigToIntensity = testGroup "#intensityConfigToIntensity"
  [   testCase "valid intensity - return it" $
        intensityConfigToIntensity "Dull" @?= Dull

    , testCase "invalid intensity - default to Vivid" $
        intensityConfigToIntensity "Foo" @?= Vivid
  ]

testColorConfigToColor :: TestTree
testColorConfigToColor = testGroup "#colorConfigToColor"
  [   testCase "valid color - return it" $
        colorConfigToColor "Cyan" @?= Cyan

    , testCase "invalid color - default to Blue" $
        colorConfigToColor "Foo" @?= Blue
  ]
