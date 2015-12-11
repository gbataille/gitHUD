module GitHUD.Terminal.Base (
  showStrInColor
  ) where

import Control.Monad.Reader

import GitHUD.Terminal.Types

showStrInColor :: Color               -- ^ The terminal color to use
               -> ColorIntensity      -- ^ The intensity to use
               -> String              -- ^ The string to output
               -> ShellOutput
showStrInColor color intensity str = do
  shell <- ask
  liftIO $ outputStrInColor color intensity str shell

outputStrInColor :: Color
                 -> ColorIntensity
                 -> String
                 -> Shell
                 -> IO()
outputStrInColor color intensity str shell = do
  let startCode = terminalStartCode color intensity
  if (shell == ZSH)
    then putStr $ zshMarkZeroWidth startCode
    else putStr $ startCode

  putStr str
  if (shell == ZSH)
    then putStr $ zshMarkZeroWidth terminalEndCode
    else putStr $ terminalEndCode

zshMarkZeroWidth :: String
                 -> String
zshMarkZeroWidth str = "%{" `mappend` str `mappend` "%}"

terminalStartCode :: Color
                  -> ColorIntensity
                  -> String
terminalStartCode  Black    Vivid  = "\x1b[1;30m"
terminalStartCode  Red      Vivid  = "\x1b[1;31m"
terminalStartCode  Green    Vivid  = "\x1b[1;32m"
terminalStartCode  Yellow   Vivid  = "\x1b[1;33m"
terminalStartCode  Blue     Vivid  = "\x1b[1;34m"
terminalStartCode  Magenta  Vivid  = "\x1b[1;35m"
terminalStartCode  Cyan     Vivid  = "\x1b[1;36m"
terminalStartCode  White    Vivid  = "\x1b[1;37m"
terminalStartCode  Black    Dull   = "\x1b[30m"
terminalStartCode  Red      Dull   = "\x1b[31m"
terminalStartCode  Green    Dull   = "\x1b[32m"
terminalStartCode  Yellow   Dull   = "\x1b[33m"
terminalStartCode  Blue     Dull   = "\x1b[34m"
terminalStartCode  Magenta  Dull   = "\x1b[35m"
terminalStartCode  Cyan     Dull   = "\x1b[36m"
terminalStartCode  White    Dull   = "\x1b[37m"

terminalEndCode :: String
terminalEndCode = "\x1b[0m"
