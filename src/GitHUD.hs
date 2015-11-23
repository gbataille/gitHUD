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
  putStr (show (localDel repoState))
  setSGR [SetColor Foreground Vivid Red]
  putStr "D"
  setSGR [Reset]
  putStr (show (localMod repoState))
  setSGR [SetColor Foreground Vivid Red]
  putStr "M "
  setSGR [Reset]
  putStr (show (localAdd repoState))
  setSGR [SetColor Foreground Vivid White]
  putStr "A "
  setSGR [Reset]
