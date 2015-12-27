module GitHUD (
    githud
    ) where

import System.Process (readProcess)
import System.Console.ANSI (setSGR, SGR(Reset,SetColor), ConsoleLayer(..), ColorIntensity(..), Color(..))
import GitHUD.Parse

githud :: IO ()
githud = do
  -- TODO: gbataille - Check that we are in a git repo
  out <- gitPorcelainStatus
  let repoState = gitParse out
  outputRepoState repoState

-- | Assumes that we are in a git repo
gitPorcelainStatus :: IO String
gitPorcelainStatus = readProcess "git" ["status", "--porcelain"] ""

outputRepoState :: GitRepoState -> IO ()
outputRepoState repoState = do
  ia <- showElem indexAdd repoState Green Vivid "A"
  id <- showElem indexDel repoState Green Vivid "D"
  im <- showElem indexMod repoState Green Vivid "M"
  if (ia || id || im)
    then putStr " "
    else return ()
  ld <- showElem localDel repoState Red Vivid "D"
  lm <- showElem localMod repoState Red Vivid "M"
  if (ld || lm)
    then putStr " "
    else return ()
  la <- showElem localAdd repoState White Vivid "A"
  if (la)
    then putStr " "
    else return ()

showElem :: (GitRepoState -> Int)
         -> GitRepoState
         -> Color
         -> ColorIntensity
         -> String
         -> IO Bool
showElem elem repoState color intensity letter = do
  let num = elem repoState
  if num > 0
    then ((showNumState num color intensity letter) >> (return True))
    else (return False)

showNumState :: Int
         -> Color
         -> ColorIntensity
         -> String
         -> IO ()
showNumState num color intensity letter = do
    putStr (show num)
    setSGR [SetColor Foreground intensity color]
    putStr letter
    setSGR [Reset]

