module Test.GitHUD.Terminal.Base (
  terminalTests
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import GitHUD.Terminal.Base
import GitHUD.Terminal.Types

terminalTests :: TestTree
terminalTests = testGroup "Terminal Base Tests"
  [ testCase "#applyShellMarkers should not do anything for non ZSH shell" $
      applyShellMarkers Other "foo" @?= "foo"

    , testCase "#applyShellMarkers should add 0-width markers for ZSH shell" $
      applyShellMarkers ZSH "foo" @?= "%{foo%}"

  ]
