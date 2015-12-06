module GitHUD (
    githud
    ) where

import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(ExitSuccess))
import System.Console.ANSI (setSGR, SGR(Reset,SetColor), ConsoleLayer(..), ColorIntensity(..), Color(..))

import GitHUD.Parse.Status
import GitHUD.Parse.Branch
import GitHUD.Parse.Count

githud :: IO ()
githud = do
  -- TODO: gbataille - Check that we are in a git repo
  isGit <- checkInGitDirectory
  if isGit
    then do
      --
      -- Running git commands
      localBranchName <- removeEndingNewline <$> gitLocalBranchName
      porcelainStatus <- gitPorcelainStatus
      let repoState = gitParseStatus porcelainStatus

      outputGitRepoIndicator
      outputLocalBranchName localBranchName

      remoteName <- removeEndingNewline <$> gitRemoteName localBranchName
      if (remoteName == "")
        then return ()
        else do
          remoteBranch <- removeEndingNewline <$> gitRemoteBranchName localBranchName

          let fullRemoteBranchName = buildFullyQualifiedRemoteBranchName remoteName remoteBranch
          commitsToPushStr <- gitRevToPush fullRemoteBranchName
          let commitsToPush = getCount commitsToPushStr
          commitsToPullStr <- gitRevToPull fullRemoteBranchName
          let commitsToPull = getCount commitsToPullStr

          outputCommitsToPullPush commitsToPull commitsToPush

      outputRepoState repoState

      -- Necessary to properly terminate the output
      putStrLn ""

    else return ()

checkInGitDirectory :: IO Bool
checkInGitDirectory = do
  (exCode, _, _) <- readProcessWithExitCode "git" ["rev-parse", "--git-dir"] ""
  return (exCode == ExitSuccess)

readProcessWithIgnoreExitCode :: FilePath -> [String] -> String -> IO String
readProcessWithIgnoreExitCode command options stdin = do
  (exCode, stdout, _) <- readProcessWithExitCode command options stdin
  if (exCode == ExitSuccess)
    then return stdout
    else return ""

removeEndingNewline :: String -> String
removeEndingNewline str = concat . lines $ str

gitLocalBranchName :: IO String
gitLocalBranchName = readProcessWithIgnoreExitCode "git" ["symbolic-ref", "--short", "HEAD"] ""

gitRemoteTrackingConfigKey :: String -> String
gitRemoteTrackingConfigKey localBranchName = "branch." ++ localBranchName ++ ".remote"

gitRemoteBranchConfigKey :: String -> String
gitRemoteBranchConfigKey localBranchName = "branch." ++ localBranchName ++ ".merge"

gitRemoteName :: String         -- ^ local branch name
              -> IO String
gitRemoteName localBranchName =
  readProcessWithIgnoreExitCode "git" ["config", "--get", gitRemoteTrackingConfigKey localBranchName] ""

gitRemoteBranchName :: String     -- ^ remote name
                    -> IO String
gitRemoteBranchName remoteName =
  readProcessWithIgnoreExitCode "git" ["config", "--get", gitRemoteBranchConfigKey remoteName] ""


-- | Assumes that we are in a git repo
gitPorcelainStatus :: IO String
gitPorcelainStatus = readProcessWithIgnoreExitCode "git" ["status", "--porcelain"] ""

gitRevToPush :: String -> IO String
gitRevToPush remoteBranchName =
  readProcessWithIgnoreExitCode "git" ["rev-list", "--right-only", "--count", branchTillHEAD remoteBranchName] ""

gitRevToPull :: String -> IO String
gitRevToPull remoteBranchName =
  readProcessWithIgnoreExitCode "git" ["rev-list", "--left-only", "--count", branchTillHEAD remoteBranchName] ""

branchTillHEAD :: String -> String
branchTillHEAD remoteBranchName = remoteBranchName ++ "...HEAD"

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

outputcommitsToPush :: Int -> IO ()
outputcommitsToPush commitCount = do
  if commitCount > 0
    then do
      putStr . show $ commitCount
      showStrInColor Green Vivid "\8593"
    else return ()

outputcommitsToPull :: Int -> IO ()
outputcommitsToPull commitCount = do
  if commitCount > 0
    then do
      putStr . show $ commitCount
      showStrInColor Red Vivid "\8595"
    else return ()

outputCommitsToPullPush :: Int          -- ^ commits to pull
                        -> Int          -- ^ commits to push
                        -> IO ()
outputCommitsToPullPush pull push = do
  if (pull > 0) && (push > 0)
    then do
      putStr (show pull)
      showStrInColor Green Vivid "\8645"
      putStr (show push)
    else (
      if (pull > 0)
        then outputcommitsToPull pull
        else (
          if (push > 0)
            then outputcommitsToPush push
            else return ()
        )
    )

  if (pull > 0) || (push > 0)
    then putStr " "
    else return ()

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
    showStrInColor color intensity letter

showStrInColor :: Color
               -> ColorIntensity
               -> String
               -> IO ()
showStrInColor color intensity str = do
    setSGR [SetColor Foreground intensity color]
    putStr str
    setSGR [Reset]
