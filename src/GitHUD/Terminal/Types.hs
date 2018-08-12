module GitHUD.Terminal.Types (
  Color(..)
  , ColorIntensity(..)
  , Shell(..)
  ) where

data Color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White | NoColor deriving (Eq, Show, Read)
data ColorIntensity = Dull | Vivid deriving (Eq, Show, Read)
data Shell = ZSH | BASH | TMUX | NONE | Other deriving (Eq, Show)
