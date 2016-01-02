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
        expectValue White $
          toBeInField confNoUpstreamIndicatorColor $
            forConfigItemKey "no_upstream_indicator_color" $
              withValue "FOO"

    , testCase "Key: no_upstream_indicator_intensity" $
        expectValue Dull $
          toBeInField confNoUpstreamIndicatorIntensity $
            forConfigItemKey "no_upstream_indicator_intensity" $
              withValue "Dull"

    , testCase "Key: no_upstream_indicator_intensity - invalid intensity" $
        expectValue Dull $
          toBeInField confNoUpstreamIndicatorIntensity $
            forConfigItemKey "no_upstream_indicator_intensity" $
              withValue "FOO"

    , testCase "Key: remote_commits_indicator" $
        expectValue "FOO" $
          toBeInField confRemoteCommitsIndicator $
            forConfigItemKey "remote_commits_indicator" $
              withValue "FOO"
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
        intensityConfigToIntensity "Vivid" @?= Vivid

    , testCase "invalid intensity - default to Dull" $
        intensityConfigToIntensity "Foo" @?= Dull
  ]

testColorConfigToColor :: TestTree
testColorConfigToColor = testGroup "#colorConfigToColor"
  [   testCase "valid color - return it" $
        colorConfigToColor "Cyan" @?= Cyan

    , testCase "invalid color - default to White" $
        colorConfigToColor "Foo" @?= White
  ]
