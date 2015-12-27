module GitHUD (
    githud
    ) where

import System.Process (readProcess)
import System.Console.ANSI (setSGR, SGR(Reset,SetColor), ConsoleLayer(..), ColorIntensity(..), Color(..))

import GitHUD.Parse.Status
import GitHUD.Parse.Branch

githud :: IO ()
githud = do
  -- TODO: gbataille - Check that we are in a git repo
  --
  -- Running git commands
  localBranchName <- removeEndingNewline <$> gitLocalBranchName
  porcelainStatus <- removeEndingNewline <$> gitPorcelainStatus
  remoteName <- removeEndingNewline <$> gitRemoteName localBranchName
  remoteBranch <- removeEndingNewline <$> gitRemoteBranchName localBranchName

  -- Parsing git command output
  let fullRemoteBranchName = buildFullyQualifiedRemoteBranchName remoteName remoteBranch
  let repoState = gitParseStatus porcelainStatus

  -- Output
  putStrLn fullRemoteBranchName
  outputGitRepoIndicator
  outputLocalBranchName localBranchName
  outputRepoState repoState

removeEndingNewline :: String -> String
removeEndingNewline str = concat . lines $ str

gitLocalBranchName :: IO String
gitLocalBranchName = readProcess "git" ["symbolic-ref", "--short", "HEAD"] ""

gitRemoteTrackingConfigKey :: String -> String
gitRemoteTrackingConfigKey localBranchName = "branch." ++ localBranchName ++ ".remote"

gitRemoteBranchConfigKey :: String -> String
gitRemoteBranchConfigKey localBranchName = "branch." ++ localBranchName ++ ".merge"

gitRemoteName :: String         -- ^ local branch name
              -> IO String
gitRemoteName localBranchName =
  readProcess "git" ["config", "--get", gitRemoteTrackingConfigKey localBranchName] ""

gitRemoteBranchName :: String     -- ^ remote name
                    -> IO String
gitRemoteBranchName remoteName =
  readProcess "git" ["config", "--get", gitRemoteBranchConfigKey remoteName] ""


-- | Assumes that we are in a git repo
gitPorcelainStatus :: IO String
gitPorcelainStatus = readProcess "git" ["status", "--porcelain"] ""

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

outputRepoState :: GitRepoState -> IO ()
outputRepoState repoState = do
  inda <- showElem indexAdd repoState Green Vivid "A"
  indd <- showElem indexDel repoState Green Vivid "D"
  indm <- showElem indexMod repoState Green Vivid "M"
  if (inda || indd || indm)
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
  confl <- showElem conflict repoState Green Vivid "C"
  if (confl)
    then putStr " "
    else return ()

showElem :: (GitRepoState -> Int)
         -> GitRepoState
         -> Color
         -> ColorIntensity
         -> String
         -> IO Bool
showElem elemFunc repoState color intensity letter = do
  let num = elemFunc repoState
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

