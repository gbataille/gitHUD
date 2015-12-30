module Test.GitHUD.Terminal.Prompt (
  terminalPromptTests
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad.Reader (runReader)
import Control.Monad.Writer (runWriterT)

import GitHUD.Git.Types
import GitHUD.Terminal.Base
import GitHUD.Terminal.Prompt
import GitHUD.Terminal.Types

terminalPromptTests :: TestTree
terminalPromptTests = testGroup "Terminal Prompt Test"
  [ testCase "#addGitRepoIndicator" $
      testWriterWithConfig (zeroOutputConfig ZSH) addGitRepoIndicator @?= "\57504 "

    , testCase "#addUpstreamIndicator with an upstream" $
      testWriterWithConfig
        (buildOutputConfig ZSH (zeroGitRepoState { gitRemoteTrackingBranch = "foo" }))
        addUpstreamIndicator
      @?= ""

    , testCase "#addUpstreamIndicator with no upstream for non ZSH shell" $
      testWriterWithConfig
        (zeroOutputConfig ZSH) addUpstreamIndicator
      @?= "upstream %{\x1b[1;31m%}\9889%{\x1b[0m%} "

    , testCase "#addUpstreamIndicator with no upstream for ZSH" $
      testWriterWithConfig
        (zeroOutputConfig Other) addUpstreamIndicator
      @?= "upstream \x1b[1;31m\9889\x1b[0m "
  ]

-- | Utility function to test a ShellOutput function and gets the prompt built
testWriterWithConfig :: OutputConfig    -- ^ Starting reader state
                     -> ShellOutput     -- ^ Function under test
                     -> String               -- ^ Output of the function for the given config
testWriterWithConfig config functionUnderTest =
  runReader (runWriterT functionUnderTest >>= (\(_, out) -> return out)) config

zeroOutputConfig :: Shell
                 -> OutputConfig
zeroOutputConfig shell = buildOutputConfig shell zeroGitRepoState
