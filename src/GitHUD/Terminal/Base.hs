module GitHUD.Terminal.Base (
  showStrInColor
  ) where

import System.Console.ANSI (ColorIntensity(..), Color(..))

import GitHUD.Terminal.ANSI (outputInANSITerminalColor)

showStrInColor :: Color
               -> ColorIntensity
               -> String
               -> IO ()
showStrInColor color intensity str = do
  outputInANSITerminalColor color intensity str
