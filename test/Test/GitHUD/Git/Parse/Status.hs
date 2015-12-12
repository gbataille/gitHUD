module Test.GitHUD.Git.Parse.Status (
  statusTests
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import GitHUD.Git.Parse.Status
import GitHUD.Git.Types

statusTests :: TestTree
statusTests = testGroup "Status Parser Test"
  [ testCase "with an empty input, should return 0 for all categories" $
      gitParseStatus ""  @?= zeroLocalRepoChanges

    , testCase "with one locally modified file" $
      gitParseStatus " M some random foo bar stuff\n" @?= (zeroLocalRepoChanges { localMod = 1 })

    , testCase "with one locally deleted file" $
      gitParseStatus " D some random foo bar stuff\n" @?= (zeroLocalRepoChanges { localDel = 1 })

    , testCase "with one locally added file" $
      gitParseStatus "?? some random foo bar stuff\n" @?= (zeroLocalRepoChanges { localAdd = 1 })

    , testCase "with one added file to the index" $
      gitParseStatus "A  some random foo bar stuff\n" @?= (zeroLocalRepoChanges { indexAdd = 1 })

    , testCase "with one modified file to the index" $
      gitParseStatus "M  some random foo bar stuff\n" @?= (zeroLocalRepoChanges { indexMod = 1 })

    , testCase "with one added file to the index" $
      gitParseStatus "D  some random foo bar stuff\n" @?= (zeroLocalRepoChanges { indexDel = 1 })

    , testCase "with a conflict with both sides changed" $
      gitParseStatus "UU test\n" @?= (zeroLocalRepoChanges { conflict = 1 })

    , testCase "with a conflict with us side changed" $
      gitParseStatus "DU test\n" @?= (zeroLocalRepoChanges { conflict = 1 })

    , testCase "with a conflict with them side changed" $
      gitParseStatus "UD test\n" @?= (zeroLocalRepoChanges { conflict = 1 })

    , testCase "with a complex mix" $
      gitParseStatus
      complexStatusString
      @?= (zeroLocalRepoChanges { localAdd = 1, localDel = 1, localMod = 1,
                           indexAdd = 1, indexMod = 1, indexDel = 1, conflict = 3 })

    , testCase "with a renamed file in the index" $
      gitParseStatus "R  test\n" @?= (zeroLocalRepoChanges { renamed = 1 })

    , testCase "with a file renamed in the index and modified locally" $
      gitParseStatus "RM test\n" @?= (zeroLocalRepoChanges { localMod = 1, renamed = 1 })

    , testCase "with a file renamed in the index and deleted locally" $
      gitParseStatus "RD test\n" @?= (zeroLocalRepoChanges { localDel = 1, renamed = 1 })

    , testCase "with a file modified in the index and deleted locally" $
      gitParseStatus "MD test\n" @?= (zeroLocalRepoChanges { localDel = 1, indexMod = 1 })

    , testCase "with a file changed in the index AND locally" $
      gitParseStatus "MM test\n" @?= (zeroLocalRepoChanges { localMod = 1, indexMod = 1 })

    , testCase "with a file added in the index AND modified locally" $
      gitParseStatus "AM test\n" @?= (zeroLocalRepoChanges { localMod = 1, indexAdd = 1 })

    , testCase "with a file added in the index AND deleted locally" $
      gitParseStatus "AD test\n" @?= (zeroLocalRepoChanges { localDel = 1, indexAdd = 1 })

  ]

complexStatusString :: String
complexStatusString =
  " M foo\n\
  \ D bar\n\
  \?? add\n\
  \A  add\n\
  \M  mod\n\
  \D  del\n\
  \UU conflict\n\
  \DU conflict\n\
  \UD conflict\n"
