module Test.GitHUD.Config.Parse (
  configParserTests
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import System.Posix.Daemon (Redirection(DevNull, ToFile))
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
    , testStringConfigToStringList
    , testStringConfigToRedirection
    , testBoolConfigToBool
    , testIntConfigToInt
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
  [   testCase "Key: show_part_repo_indicator" $
        expectValue False $
          toBeInField confShowPartRepoIndicator $
            forConfigItemKey "show_part_repo_indicator" $
              withValue "False"

    , testCase "Key: show_part_merge_branch_commits_diff" $
        expectValue False $
          toBeInField confShowPartMergeBranchCommitsDiff $
            forConfigItemKey "show_part_merge_branch_commits_diff" $
              withValue "False"

    , testCase "Key: show_part_local_branch" $
        expectValue False $
          toBeInField confShowPartLocalBranch $
            forConfigItemKey "show_part_local_branch" $
              withValue "False"

    , testCase "Key: show_part_commits_to_origin" $
        expectValue False $
          toBeInField confShowPartCommitsToOrigin $
            forConfigItemKey "show_part_commits_to_origin" $
              withValue "False"

    , testCase "Key: show_part_local_changes_state" $
        expectValue False $
          toBeInField confShowPartLocalChangesState $
            forConfigItemKey "show_part_local_changes_state" $
              withValue "False"

    , testCase "Key: show_part_stashes" $
        expectValue False $
          toBeInField confShowPartStashes $
            forConfigItemKey "show_part_stashes" $
              withValue "False"

    , testCase "Comment should have no impact on the config" $
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

    , testCase "Key: no_tracked_upstream_text" $
        expectValue "foo" $
          toBeInField confNoTrackedUpstreamString $
            forConfigItemKey "no_tracked_upstream_text" $
              withValue "foo"

    , testCase "Key: no_tracked_upstream_text_color" $
        expectValue Green $
          toBeInField confNoTrackedUpstreamStringColor $
            forConfigItemKey "no_tracked_upstream_text_color" $
              withValue "Green"

    , testCase "Key: no_tracked_upstream_text_intensity" $
        expectValue Dull $
          toBeInField confNoTrackedUpstreamStringIntensity $
            forConfigItemKey "no_tracked_upstream_text_intensity" $
              withValue "Dull"

    , testCase "Key: no_tracked_upstream_indicator" $
        expectValue "foo" $
          toBeInField confNoTrackedUpstreamIndicator $
            forConfigItemKey "no_tracked_upstream_indicator" $
              withValue "foo"

    , testCase "Key: no_tracked_upstream_indicator_color" $
        expectValue Black $
          toBeInField confNoTrackedUpstreamIndicatorColor $
            forConfigItemKey "no_tracked_upstream_indicator_color" $
              withValue "Black"

    , testCase "Key: no_tracked_upstream_indicator_color - invalid color" $
        expectValue NoColor $
          toBeInField confNoTrackedUpstreamIndicatorColor $
            forConfigItemKey "no_tracked_upstream_indicator_color" $
              withValue "FOO"

    , testCase "Key: no_tracked_upstream_indicator_intensity" $
        expectValue Dull $
          toBeInField confNoTrackedUpstreamIndicatorIntensity $
            forConfigItemKey "no_tracked_upstream_indicator_intensity" $
              withValue "Dull"

    , testCase "Key: no_tracked_upstream_indicator_intensity - invalid intensity" $
        expectValue Vivid $
          toBeInField confNoTrackedUpstreamIndicatorIntensity $
            forConfigItemKey "no_tracked_upstream_indicator_intensity" $
              withValue "FOO"

    , testCase "Key: merge_branch_commits_indicator" $
        expectValue "FOO" $
          toBeInField confMergeBranchCommitsIndicator $
            forConfigItemKey "merge_branch_commits_indicator" $
              withValue "FOO"

    , testCase "Key: merge_branch_commits_pull_prefix" $
        expectValue "FOO" $
          toBeInField confMergeBranchCommitsOnlyPull $
            forConfigItemKey "merge_branch_commits_pull_prefix" $
              withValue "FOO"

    , testCase "Key: merge_branch_commits_push_prefix" $
        expectValue "FOO" $
          toBeInField confMergeBranchCommitsOnlyPush $
            forConfigItemKey "merge_branch_commits_push_prefix" $
              withValue "FOO"

    , testCase "Key: merge_branch_commits_push_pull_infix" $
        expectValue "FOO" $
          toBeInField confMergeBranchCommitsBothPullPush $
            forConfigItemKey "merge_branch_commits_push_pull_infix" $
              withValue "FOO"

    , testCase "Key: merge_branch_ignore_branches" $
        expectValue ["gh-pages", "FOO"] $
          toBeInField confMergeBranchIgnoreBranches $
            forConfigItemKey "merge_branch_ignore_branches" $
              withValue "gh-pages, FOO"

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

    , testCase "Key: run_fetcher_daemon" $
        expectValue True $
          toBeInField confRunFetcherDaemon $
            forConfigItemKey "run_fetcher_daemon" $
              withValue "True"

    , testCase "Key: githudd_sleep_seconds" $
        expectValue 5 $
          toBeInField confGithuddSleepSeconds $
            forConfigItemKey "githudd_sleep_seconds" $
              withValue "5"

    , testCase "Key: githudd_pid_file_path" $
        expectValue "/tmp" $
          toBeInField confGithuddPidFilePath $
            forConfigItemKey "githudd_pid_file_path" $
              withValue "/tmp"

    , testCase "Key: githudd_socket_file_path" $
        expectValue "/tmp" $
          toBeInField confGithuddSocketFilePath $
            forConfigItemKey "githudd_socket_file_path" $
              withValue "/tmp"

    , testCase "Key: githudd_log_file_path" $
        expectValue DevNull $
          toBeInField confGithuddLogFilePath $
            forConfigItemKey "githudd_log_file_path" $
              withValue "/dev/null"
  ]

expectValue :: (Eq a, Show a) => a -> a -> Assertion
expectValue expected actual = actual @?= expected

toBeInField :: (Config -> a) -> Config -> a
toBeInField = id

forConfigItemKey :: String -> String -> Config
forConfigItemKey key value =
  configItemsFolder defaultConfig (Item key value)

withValue :: String -> String
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
        colorConfigToColor "Foo" @?= NoColor
  ]

testStringConfigToStringList :: TestTree
testStringConfigToStringList = testGroup "#stringConfigToStringList"
  [   testCase "valid string list, comma separated, no spaces" $
        stringConfigToStringList "foo,bar" @?= ["foo", "bar"]

    , testCase "valid string list, comma separated, spaces" $
        stringConfigToStringList "foo, bar ,  baz " @?= ["foo", "bar", "baz"]

    , testCase "valid string list, comma separated, finish with comma" $
        stringConfigToStringList "foo,bar, " @?= ["foo", "bar"]
  ]

testStringConfigToRedirection :: TestTree
testStringConfigToRedirection = testGroup "#strConfigToRedirection"
  [   testCase "dev null" $
        strConfigToRedirection "/dev/null" @?= DevNull

    , testCase "other path" $
        strConfigToRedirection "/foo/bar" @?= ToFile "/foo/bar"
  ]

testBoolConfigToBool :: TestTree
testBoolConfigToBool = testGroup "#boolConfigToBool"
  [   testCase "True" $
        boolConfigToBool "True" @?= True

    , testCase "true" $
        boolConfigToBool "true" @?= True

    , testCase "yes" $
        boolConfigToBool "yes" @?= True

    , testCase "False" $
        boolConfigToBool "False" @?= False

    , testCase "Defaults to False on failed parsing" $
        boolConfigToBool "foo" @?= False
  ]

testIntConfigToInt :: TestTree
testIntConfigToInt = testGroup "#intConfigToInt"
  [   testCase "standard valid int" $
        intConfigToInt "12" @?= 12

    , testCase "Cuts to the integer part found" $
        intConfigToInt "12.5" @?= 12

    , testCase "any bad value defaults to 5" $
        intConfigToInt "foo" @?= 5
  ]
