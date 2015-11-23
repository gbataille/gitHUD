module GitHUD (
    githud
    ) where

import System.Process (readProcess)
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
outputRepoState repoState = print . gitRepoStateToString $ repoState
