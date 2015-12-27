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
  showElem indexAdd repoState Green Vivid "A"
  showElem indexDel repoState Green Vivid "D"
  showElem indexMod repoState Green Vivid "M"
  putStr " "
  showElem localDel repoState Red Vivid "D"
  showElem localMod repoState Red Vivid "M"
  putStr " "
  showElem localAdd repoState White Vivid "A"
  putStr " "

showElem :: (GitRepoState -> Int)
         -> GitRepoState
         -> Color
         -> ColorIntensity
         -> String
         -> IO ()
showElem elem repoState color intensity letter = do
  let num = elem repoState
  if num > 0
    then showNumState num color intensity letter
    else putStr ""

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

