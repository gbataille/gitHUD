module GitHUD.Terminal.Types (
  Color(..)
  , ColorIntensity(..)
  , Shell(..)
  ) where

data Color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White deriving (Eq, Show)
data ColorIntensity = Dull | Vivid deriving (Eq, Show)
data Shell = ZSH | Other deriving (Eq, Show)
