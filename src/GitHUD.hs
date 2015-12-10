module GitHUD (
    githud
    ) where

import System.Process (readProcessWithExitCode, proc, StdStream(CreatePipe, UseHandle), createProcess, CreateProcess(..))
import GHC.IO.Handle (hGetLine)
import System.Exit (ExitCode(ExitSuccess))
import System.Console.ANSI (setSGR, SGR(Reset,SetColor), ConsoleLayer(..), ColorIntensity(..), Color(..))
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar)

import GitHUD.Parse.Status
import GitHUD.Parse.Branch
import GitHUD.Parse.Count

githud :: IO ()
githud = do
  isGit <- checkInGitDirectory
  if isGit
    then do
      -- TODO - gbataille : build a datastructure
      -- Preparing MVars
      ivLocalBranch <- newEmptyMVar
      ivGitStatus <- newEmptyMVar
      ivRemoteName <- newEmptyMVar
      ivRemoteBranchName <- newEmptyMVar
      ivCommitsToPull <- newEmptyMVar
      ivCommitsToPush <- newEmptyMVar
      ivRemoteCommitsToPull <- newEmptyMVar
      ivRemoteCommitsToPush <- newEmptyMVar
      ivStashCount <- newEmptyMVar

      --
      -- Running git commands with the concurrent API
      forkIO $ gitLocalBranchName ivLocalBranch
      forkIO $ gitPorcelainStatus ivGitStatus
      forkIO $ gitStashCount ivStashCount

      -- Retrieving the values of the git commands
      repoState <- gitParseStatus <$> takeMVar ivGitStatus
      localBranchName <- removeEndingNewline <$> (takeMVar ivLocalBranch)

      forkIO $ gitRemoteName localBranchName ivRemoteName
      remoteName <- removeEndingNewline <$> (takeMVar ivRemoteName)

      outputGitRepoIndicator


      if (remoteName == "")
        then outputLocalBranchName localBranchName
        else do
          forkIO $ gitRemoteBranchName localBranchName ivRemoteBranchName
          remoteBranch <- removeEndingNewline <$> (takeMVar ivRemoteBranchName)

          let fullRemoteBranchName = buildFullyQualifiedRemoteBranchName remoteName remoteBranch

          forkIO $ gitRevToPush "origin/master" fullRemoteBranchName ivRemoteCommitsToPush
          forkIO $ gitRevToPull "origin/master" fullRemoteBranchName ivRemoteCommitsToPull
          forkIO $ gitRevToPush fullRemoteBranchName "HEAD" ivCommitsToPush
          forkIO $ gitRevToPull fullRemoteBranchName "HEAD" ivCommitsToPull

          rCommitsToMergeStr <- takeMVar ivRemoteCommitsToPush
          let rCommitsToMerge = getCount rCommitsToMergeStr
          rCommitsToRMaserStr <- takeMVar ivRemoteCommitsToPull
          let rCommitsToRMaser = getCount rCommitsToRMaserStr

          commitsToPushStr <- takeMVar ivCommitsToPush
          let commitsToPush = getCount commitsToPushStr
          commitsToPullStr <- takeMVar ivCommitsToPull
          let commitsToPull = getCount commitsToPullStr

          outputRCommits rCommitsToMerge rCommitsToRMaser
          outputLocalBranchName localBranchName
          outputCommitsToPullPush commitsToPull commitsToPush

      outputRepoState repoState

      stashCountStr <- takeMVar ivStashCount
      outputStashCount stashCountStr

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

gitLocalBranchName :: MVar String -> IO ()
gitLocalBranchName out = do
  localBranch <- readProcessWithIgnoreExitCode "git" ["symbolic-ref", "--short", "HEAD"] ""
  putMVar out localBranch

gitRemoteTrackingConfigKey :: String -> String
gitRemoteTrackingConfigKey localBranchName = "branch." ++ localBranchName ++ ".remote"

gitRemoteBranchConfigKey :: String -> String
gitRemoteBranchConfigKey localBranchName = "branch." ++ localBranchName ++ ".merge"

gitRemoteName :: String         -- ^ local branch name
              -> MVar String   -- ^ the output ivar
              -> IO ()
gitRemoteName localBranchName out = do
  remoteName <- readProcessWithIgnoreExitCode "git" ["config", "--get", gitRemoteTrackingConfigKey localBranchName] ""
  putMVar out remoteName

gitRemoteBranchName :: String     -- ^ remote name
                    -> MVar String     -- ^ The output ivar
                    -> IO ()
gitRemoteBranchName remoteName out = do
  remoteBranch <- readProcessWithIgnoreExitCode "git" ["config", "--get", gitRemoteBranchConfigKey remoteName] ""
  putMVar out remoteBranch


-- | Assumes that we are in a git repo
gitPorcelainStatus :: MVar String -> IO ()
gitPorcelainStatus out = do
  porcelainStatus <- readProcessWithIgnoreExitCode "git" ["status", "--porcelain"] ""
  putMVar out porcelainStatus

gitRevToPush :: String          -- ^ from revision
             -> String          -- ^ to revision
             -> MVar String      -- ^ The output ivar
             -> IO ()
gitRevToPush fromCommit toCommit out = do
  revToPush <- readProcessWithIgnoreExitCode "git" ["rev-list", "--right-only", "--count", mergeBaseDiffFromTo fromCommit toCommit] ""
  putMVar out revToPush

gitRevToPull :: String          -- ^ from revision
             -> String          -- ^ to revision
             -> MVar String      -- ^ The output ivar
             -> IO ()
gitRevToPull fromCommit toCommit out = do
  revToPull <- readProcessWithIgnoreExitCode "git" ["rev-list", "--left-only", "--count", mergeBaseDiffFromTo fromCommit toCommit] ""
  putMVar out revToPull

gitStashCount :: MVar String     -- ^ The output ivar
              -> IO ()
gitStashCount out = do
  ( _, Just hGitStashList, _, _) <- createProcess
    (proc "git" ["stash", "list"])
    { std_out = CreatePipe }
  ( _, Just hCountStr, _, _) <- createProcess
    (proc "wc" ["-l"])
    { std_in = UseHandle hGitStashList, std_out = CreatePipe }
  count <- hGetLine hCountStr
  putMVar out count

mergeBaseDiffFromTo :: String -> String -> String
mergeBaseDiffFromTo fromCommit toCommit = fromCommit ++ "..." ++ toCommit

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
          if (push < 0)
            then do
              putStr "m "
              showStrInColor Green Vivid "\8594"
              putStr " "
              (putStr . show) push
            else return ()
        )
    )

  if (pull > 0) || (push > 0)
    then putStr " "
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

outputStashCount :: String -> IO ()
outputStashCount stashCountStr = do
  let stashCount = getCount stashCountStr
  if (stashCount == 0)
    then return ()
    else do
      putStr . show $ stashCount
      showStrInColor Green Vivid "≡ "

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
