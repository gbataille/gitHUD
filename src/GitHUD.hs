module GitHUD (
    githud
    ) where

import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(ExitSuccess))
import Control.Monad (when)

import GitHUD.Terminal.Types (ColorIntensity(..), Color(..))
import GitHUD.Git.Types
import GitHUD.Terminal.Base (showStrInColor)
import GitHUD.Git.Parse.Base

githud :: IO ()
githud = do
  isGit <- checkInGitDirectory
  when isGit $ do
    repoState <- getGitRepoState
    outputGitRepoIndicator
    outputRCommits (gitRemoteCommitsToPull repoState) (gitRemoteCommitsToPush repoState)
    outputLocalBranchName (gitLocalBranch repoState)
    outputCommitsToPullPush (gitCommitsToPull repoState) (gitCommitsToPush repoState)
    outputRepoState (gitLocalRepoChanges repoState)
    outputStashCount (gitStashCount repoState)

    -- Necessary to properly terminate the output
    putStrLn ""

checkInGitDirectory :: IO Bool
checkInGitDirectory = do
  (exCode, _, _) <- readProcessWithExitCode "git" ["rev-parse", "--git-dir"] ""
  return (exCode == ExitSuccess)

-- | Requires patched fonts for Powerline (Monaco Powerline)
outputGitRepoIndicator :: IO ()
outputGitRepoIndicator = do
  putChar '\57504'
  putChar ' '

outputLocalBranchName :: String -> IO ()
outputLocalBranchName localBranchName = do
  putStr "["
  mapM_ putStr (lines localBranchName)
  putStr "]"
  putStr " "

outputcommitsToPush :: Int
                    -> IO ()
outputcommitsToPush commitCount = do
  when (commitCount > 0) $ do
    putStr . show $ commitCount
    showStrInColor Green Vivid "\8593"

outputcommitsToPull :: Int
                    -> IO ()
outputcommitsToPull commitCount = do
  when (commitCount > 0) $ do
    putStr . show $ commitCount
    showStrInColor Red Vivid "\8595"

outputRCommits :: Int          -- ^ commits to pull
               -> Int          -- ^ commits to push
               -> IO ()
outputRCommits pull push = do
  if (pull > 0) && (push > 0)
    then do
      putStr "m "
      putStr (show pull)
      showStrInColor Green Vivid "\8644"
      putStr (show push)
    else (
      if (pull > 0)
        then do
          putStr "m "
          showStrInColor Green Vivid "\8592"
          putStr " "
          (putStr . show) pull
        else (
          when (push > 0) $ do
            putStr "m "
            showStrInColor Green Vivid "\8594"
            putStr " "
            (putStr . show) push
        )
    )

  when ((pull > 0) || (push > 0)) $ putStr " "

outputCommitsToPullPush :: Int          -- ^ commits to pull
                        -> Int          -- ^ commits to push
                        -> IO ()
outputCommitsToPullPush pull push = do
  if (pull > 0) && (push > 0)
    then do
      putStr (show pull)
      showStrInColor Green Vivid "\8645"
      putStr (show push)
    else
      if (pull > 0)
        then outputcommitsToPull pull
        else
          when (push > 0) $ do outputcommitsToPush push

  when ((pull > 0) || (push > 0)) $ putStr " "

outputStashCount :: Int
                 -> IO ()
outputStashCount stashCount = do
  when (stashCount /= 0) $ do
    putStr . show $ stashCount
    showStrInColor Green Vivid "≡ "

outputRepoState :: GitLocalRepoChanges
                -> IO ()
outputRepoState repoState = do
  inda <- showElem indexAdd repoState Green Vivid "A"
  indd <- showElem indexDel repoState Green Vivid "D"
  indm <- showElem indexMod repoState Green Vivid "M"
  when (inda || indd || indm) $ putStr " "

  ld <- showElem localDel repoState Red Vivid "D"
  lm <- showElem localMod repoState Red Vivid "M"
  when (ld || lm) $ putStr " "

  la <- showElem localAdd repoState White Vivid "A"
  when (la) $ putStr " "

  confl <- showElem conflict repoState Green Vivid "C"
  when (confl) $ putStr " "

showElem :: (GitLocalRepoChanges -> Int)
         -> GitLocalRepoChanges
         -> Color
         -> ColorIntensity
         -> String
         -> IO Bool
showElem elemFunc repoState color intensity letter = do
  let num = elemFunc repoState
  if num > 0
    then ((showNumState num color intensity letter ) >> (return True))
    else (return False)

showNumState :: Int
         -> Color
         -> ColorIntensity
         -> String
         -> IO ()
showNumState num color intensity letter = do
    putStr (show num)
    showStrInColor color intensity letter
