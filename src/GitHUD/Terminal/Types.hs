module GitHUD.Terminal.Types (
  Color(..)
  , ColorIntensity(..)
  , Shell(..)
  , OutputConfig(..)
  , buildOutputConfig
  , Prompt
  , TerminalState
  , ShellOutput
  ) where

import Control.Monad.Reader (Reader)
import Control.Monad.Writer (WriterT)

import GitHUD.Git.Types

data Color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
data ColorIntensity = Dull | Vivid
data Shell = ZSH | Other deriving (Eq, Show)
data OutputConfig = OutputConfig { getShell :: Shell, getRepoState :: GitRepoState }

buildOutputConfig :: Shell
                  -> GitRepoState
                  -> OutputConfig
buildOutputConfig shell repoState = OutputConfig {
  getShell = shell
  , getRepoState = repoState
}

type Prompt = String

type TerminalState = Reader OutputConfig String

type ShellOutput = WriterT Prompt (Reader OutputConfig) ()

