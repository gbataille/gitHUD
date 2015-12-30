module GitHUD.Terminal.Base (
  tellStringInColor
  , applyShellMarkers
  , terminalEndCode
  , terminalStartCode
  ) where

import Control.Monad.Writer (tell)
import Data.Monoid (mappend)

import GitHUD.Terminal.Types

tellStringInColor :: Color               -- ^ The terminal color to use
                  -> ColorIntensity      -- ^ The intensity to use
                  -> String              -- ^ The string to output
                  -> ShellOutput
tellStringInColor color intensity str = do
  shell <- getShell
  tell $ startColorMarker color intensity shell
  tell $ str
  tell $ endColorMarker shell

startColorMarker :: Color
                 -> ColorIntensity
                 -> Shell
                 -> String
startColorMarker color intensity shell =
  applyShellMarkers shell $ terminalStartCode color intensity

endColorMarker :: Shell
               -> String
endColorMarker shell =
  applyShellMarkers shell $ terminalEndCode

applyShellMarkers :: Shell
                  -> String
                  -> String
applyShellMarkers ZSH = zshMarkZeroWidth
applyShellMarkers _ = id

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
