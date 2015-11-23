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

gitRepoStateToString :: GitRepoState -> String
gitRepoStateToString repoState =
    " " ++
    show (localMod repoState) ++ "M " ++
    show (localAdd repoState) ++ "A " ++
    show (localDel repoState) ++ "D "

outputRepoState :: GitRepoState -> IO ()
outputRepoState repoState = do
  showElem localDel repoState Red Vivid "D" False
  showElem localMod repoState Red Vivid "M" True
  showElem localAdd repoState White Vivid "A" True

showElem :: (GitRepoState -> Int)
         -> GitRepoState
         -> Color
         -> ColorIntensity
         -> String
         -> Bool                 -- ^ whether to add a space
         -> IO ()
showElem elem repoState color intensity letter space = do
  putStr (show (elem repoState))
  setSGR [SetColor Foreground intensity color]
  putStr letter
  if space then (putStr " ") else (putStr "")
  setSGR [Reset]
