module GitHUD.Terminal.ANSI (
  outputInANSITerminalColor
  ) where

import System.Console.ANSI (setSGR, SGR(Reset,SetColor), ConsoleLayer(..), ColorIntensity(..), Color(..))

outputInANSITerminalColor :: Color
                          -> ColorIntensity
                          -> String
                          -> IO ()
outputInANSITerminalColor color intensity str = do
    setSGR [SetColor Foreground intensity color]
    putStr str
    setSGR [Reset]

