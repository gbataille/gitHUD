module GitHUD.Terminal.Base (
  tellStringInColor
  , applyShellMarkers
  , terminalStartCode
  , endColorMarker
  ) where

import Control.Monad.Writer (tell)
import Data.Monoid (mappend)

import GitHUD.Types
import GitHUD.Terminal.Types

tellStringInColor :: Color               -- ^ The terminal color to use
                  -> ColorIntensity      -- ^ The intensity to use
                  -> String              -- ^ The string to output
                  -> ShellOutput
tellStringInColor color intensity str = do
  shell <- askShell
  tell $ startColorMarker color intensity shell
  tell $ str
  tell $ endColorMarker shell

startColorMarker :: Color
                 -> ColorIntensity
                 -> Shell
                 -> String
startColorMarker color intensity shell
  | shell == TMUX = tmuxStartCode color intensity
  | otherwise = applyShellMarkers shell $ terminalStartCode color intensity

endColorMarker :: Shell
               -> String
endColorMarker shell
  | shell == TMUX = tmuxEndCode
  | otherwise = applyShellMarkers shell $ terminalEndCode

applyShellMarkers :: Shell
                  -> String
                  -> String
applyShellMarkers ZSH = zshMarkZeroWidth
applyShellMarkers BASH = bashMarkZeroWidth
applyShellMarkers _ = id

zshMarkZeroWidth :: String
                 -> String
zshMarkZeroWidth str = "%{" `mappend` str `mappend` "%}"

bashMarkZeroWidth :: String
                 -> String
bashMarkZeroWidth str = "\001" `mappend` str `mappend` "\002"

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
terminalStartCode  NoColor  _      = terminalEndCode

terminalEndCode :: String
terminalEndCode = "\x1b[0;39m"

tmuxStartCode :: Color
              -> ColorIntensity
              -> String
tmuxStartCode  Black    Vivid  = "#[fg=brightblack]"
tmuxStartCode  Red      Vivid  = "#[fg=brightred]"
tmuxStartCode  Green    Vivid  = "#[fg=brightgreen]"
tmuxStartCode  Yellow   Vivid  = "#[fg=brightyellow]"
tmuxStartCode  Blue     Vivid  = "#[fg=brightblue]"
tmuxStartCode  Magenta  Vivid  = "#[fg=brightmagenta]"
tmuxStartCode  Cyan     Vivid  = "#[fg=brightcyan]"
tmuxStartCode  White    Vivid  = "#[fg=brightwhite]"
tmuxStartCode  Black    Dull   = "#[fg=black]"
tmuxStartCode  Red      Dull   = "#[fg=red]"
tmuxStartCode  Green    Dull   = "#[fg=green]"
tmuxStartCode  Yellow   Dull   = "#[fg=yellow]"
tmuxStartCode  Blue     Dull   = "#[fg=blue]"
tmuxStartCode  Magenta  Dull   = "#[fg=magenta]"
tmuxStartCode  Cyan     Dull   = "#[fg=cyan]"
tmuxStartCode  White    Dull   = "#[fg=white]"
tmuxStartCode  NoColor  _      = tmuxEndCode

tmuxEndCode :: String
tmuxEndCode = "#[fg=default]"
