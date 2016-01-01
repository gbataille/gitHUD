module Test.GitHUD.Config.Parse (
  configParserTests
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import Text.Parsec (parse)
import Text.Parsec.String (Parser)

import GitHUD.Config.Parse

configParserTests :: TestTree
configParserTests = testGroup "Config Parser Test"
  [ testItemParser
    , testCommentParser
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

utilConfigItemParser :: Parser ConfigItem -> String -> ConfigItem
utilConfigItemParser parser str =
  either
    (const ErrorLine)
    id
    (parse parser "" str)

