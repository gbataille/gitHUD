module GitHUD.Terminal.Types (
  Color(..)
  , ColorIntensity(..)
  , Shell(..)
  , Prompt(..)
  , ShellOutput
  ) where

import Control.Monad.Reader

data Color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
data ColorIntensity = Dull | Vivid
data Shell = ZSH | Other deriving (Eq)

data Prompt = Prompt { shellType :: Shell, prompt :: String }

type ShellOutput = ReaderT Shell IO ()

