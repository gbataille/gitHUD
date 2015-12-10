module GitHUD.Terminal.Base (
  showStrInColor
  ) where

import GitHUD.Terminal.Types (Color(..), ColorIntensity(..))

showStrInColor :: Color
               -> ColorIntensity
               -> String
               -> IO ()
showStrInColor color intensity str = do
  putStr $ terminalStartCode color intensity
  putStr str
  putStr terminalEndCode

terminalStartCode :: Color
                  -> ColorIntensity
                  -> String
terminalStartCode  Black    Vivid  = "%{\x1b[1;30m%}"
terminalStartCode  Red      Vivid  = "%{\x1b[1;31m%}"
terminalStartCode  Green    Vivid  = "%{\x1b[1;32m%}"
terminalStartCode  Yellow   Vivid  = "%{\x1b[1;33m%}"
terminalStartCode  Blue     Vivid  = "%{\x1b[1;34m%}"
terminalStartCode  Magenta  Vivid  = "%{\x1b[1;35m%}"
terminalStartCode  Cyan     Vivid  = "%{\x1b[1;36m%}"
terminalStartCode  White    Vivid  = "%{\x1b[1;37m%}"
terminalStartCode  Black    Dull   = "%{\x1b[30m%}"
terminalStartCode  Red      Dull   = "%{\x1b[31m%}"
terminalStartCode  Green    Dull   = "%{\x1b[32m%}"
terminalStartCode  Yellow   Dull   = "%{\x1b[33m%}"
terminalStartCode  Blue     Dull   = "%{\x1b[34m%}"
terminalStartCode  Magenta  Dull   = "%{\x1b[35m%}"
terminalStartCode  Cyan     Dull   = "%{\x1b[36m%}"
terminalStartCode  White    Dull   = "%{\x1b[37m%}"

terminalEndCode :: String
terminalEndCode = "%{\x1b[0m%}"
