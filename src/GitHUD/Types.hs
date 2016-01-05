{-# Language FlexibleContexts #-}

module GitHUD.Types (
  OutputConfig(..)
  , buildOutputConfig
  , Prompt
  , TerminalState
  , ShellOutput
  , askShell
  , askRepoState
  , askConfig
  ) where

import Control.Monad.Reader (Reader, MonadReader, asks)
import Control.Monad.Writer (WriterT)

import GitHUD.Config.Types
import GitHUD.Git.Types
import GitHUD.Terminal.Types

data OutputConfig = OutputConfig {
  _shell :: Shell
  , _repoState :: GitRepoState
  , _config :: Config
}

buildOutputConfig :: Shell
                  -> GitRepoState
                  -> Config
                  -> OutputConfig
buildOutputConfig shell repoState config = OutputConfig {
  _shell = shell
  , _repoState = repoState
  , _config = config
}

askShell :: MonadReader OutputConfig m => m Shell
askShell = asks _shell

askRepoState :: MonadReader OutputConfig m => m GitRepoState
askRepoState = asks _repoState

askConfig :: MonadReader OutputConfig m => m Config
askConfig = asks _config

type Prompt = String

type TerminalState = Reader OutputConfig String

type ShellOutput = WriterT Prompt (Reader OutputConfig) ()

