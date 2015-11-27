module Test.GitHUD.Parse.Status (
  statusTests
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import GitHUD.Parse.Status

statusTests :: TestTree
statusTests = testGroup "Status Parser Test"
  [ testCase "with an empty input, should return 0 for all categories" $
      gitParseStatus ""  @?= zeroRepoState

    , testCase "with one locally modified file" $
      gitParseStatus " M some random foo bar stuff\n" @?= (zeroRepoState { localMod = 1 })

    , testCase "with one locally deleted file" $
      gitParseStatus " D some random foo bar stuff\n" @?= (zeroRepoState { localDel = 1 })

    , testCase "with one locally added file" $
      gitParseStatus "?? some random foo bar stuff\n" @?= (zeroRepoState { localAdd = 1 })

    , testCase "with one added file to the index" $
      gitParseStatus "A  some random foo bar stuff\n" @?= (zeroRepoState { indexAdd = 1 })

    , testCase "with one modified file to the index" $
      gitParseStatus "M  some random foo bar stuff\n" @?= (zeroRepoState { indexMod = 1 })

    , testCase "with one added file to the index" $
      gitParseStatus "D  some random foo bar stuff\n" @?= (zeroRepoState { indexDel = 1 })

    , testCase "with a complex mix" $
      gitParseStatus 
      complexStatusString
      @?= (zeroRepoState { localAdd = 1, localDel = 1, localMod = 1,
                           indexAdd = 1, indexMod = 1, indexDel = 1 })
  ]

complexStatusString :: String
complexStatusString =
  " M foo\n\
  \ D bar\n\
  \?? add\n\
  \A  add\n\
  \M  mod\n\
  \D  del\n"
