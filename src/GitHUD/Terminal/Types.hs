{-# Language FlexibleContexts #-}

module GitHUD.Terminal.Types (
  Color(..)
  , ColorIntensity(..)
  , Shell(..)
  , OutputConfig(..)
  , buildOutputConfig
  , Prompt
  , TerminalState
  , ShellOutput
  , getShell
  , getRepoState
  ) where

import Control.Monad.Reader (Reader, MonadReader, ask, liftM)
import Control.Monad.Writer (WriterT)

import GitHUD.Git.Types

data Color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
data ColorIntensity = Dull | Vivid
data Shell = ZSH | Other deriving (Eq, Show)
data OutputConfig = OutputConfig { _shell :: Shell, _repoState :: GitRepoState }

buildOutputConfig :: Shell
                  -> GitRepoState
                  -> OutputConfig
buildOutputConfig shell repoState = OutputConfig {
  _shell = shell
  , _repoState = repoState
}

getShell :: MonadReader OutputConfig m => m Shell
getShell = liftM _shell $ ask

getRepoState :: MonadReader OutputConfig m => m GitRepoState
getRepoState = liftM _repoState $ ask

type Prompt = String

type TerminalState = Reader OutputConfig String

type ShellOutput = WriterT Prompt (Reader OutputConfig) ()

