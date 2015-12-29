module GitHUD.Terminal.Types (
  Color(..)
  , ColorIntensity(..)
  , Shell(..)
  , Prompt(..)
  , emptyPromptForShell
  , ShellOutput
  ) where

import Control.Monad.State

data Color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
data ColorIntensity = Dull | Vivid
data Shell = ZSH | Other deriving (Eq)
data Prompt = Prompt { shellType :: Shell, prompt :: String }

emptyPromptForShell :: Shell -> Prompt
emptyPromptForShell shell = Prompt { shellType = shell, prompt = "" }

type ShellOutput = StateT Prompt IO ()

