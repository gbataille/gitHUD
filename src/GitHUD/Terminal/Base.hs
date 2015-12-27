{-# LANGUAGE MultiParamTypeClasses #-}

module GitHUD.Terminal.Base (
  showStrInColor
  -- , Color(..)
  -- , ColorIntensity(..)
  -- , TerminalColorCoder
  ) where

import qualified System.Console.ANSI as ANSI

import GitHUD.Terminal.ANSI (outputInANSITerminalColor)

-- data Color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
-- data ColorIntensity = Dull | Vivid
--
-- class TerminalColorCoder where
--   codeStartFromColor :: Color -> String
--   codeEndFromColor :: Color -> String

showStrInColor :: ANSI.Color
               -> ANSI.ColorIntensity
               -> String
               -> IO ()
showStrInColor color intensity str = do
  outputInANSITerminalColor color intensity str
