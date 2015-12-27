module GitHUD (
    githud
    ) where

import System.Process (readProcessWithExitCode, proc, StdStream(CreatePipe, UseHandle), createProcess, CreateProcess(..))
import GHC.IO.Handle (hGetLine)
import System.Exit (ExitCode(ExitSuccess))
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar)
import Control.Monad (when)

import GitHUD.Process (readProcessWithIgnoreExitCode)
import GitHUD.Terminal.Types (ColorIntensity(..), Color(..))
import GitHUD.Parse.Status
import GitHUD.Parse.Branch
import GitHUD.Parse.Count
import GitHUD.Terminal.Base (showStrInColor)

githud :: IO ()
githud = do
  isGit <- checkInGitDirectory
  when isGit $ do
    -- TODO - gbataille : build a datastructure
    -- Preparing MVars
    mvLocalBranch <- newEmptyMVar
    mvGitStatus <- newEmptyMVar
    mvRemoteName <- newEmptyMVar
    mvRemoteBranchName <- newEmptyMVar
    mvCommitsToPull <- newEmptyMVar
    mvCommitsToPush <- newEmptyMVar
    mvRemoteCommitsToPull <- newEmptyMVar
    mvRemoteCommitsToPush <- newEmptyMVar
    mvStashCount <- newEmptyMVar

    --
    -- Running git commands with the concurrent API
    forkIO $ gitLocalBranchName mvLocalBranch
    forkIO $ gitPorcelainStatus mvGitStatus
    forkIO $ gitStashCount mvStashCount

    -- Retrieving the values of the git commands
    repoState <- gitParseStatus <$> takeMVar mvGitStatus
    localBranchName <- removeEndingNewline <$> (takeMVar mvLocalBranch)

    forkIO $ gitRemoteName localBranchName mvRemoteName
    remoteName <- removeEndingNewline <$> (takeMVar mvRemoteName)

    outputGitRepoIndicator

    if (remoteName == "")
      then outputLocalBranchName localBranchName
      else do
        forkIO $ gitRemoteBranchName localBranchName mvRemoteBranchName
        remoteBranch <- removeEndingNewline <$> (takeMVar mvRemoteBranchName)

        let fullRemoteBranchName = buildFullyQualifiedRemoteBranchName remoteName remoteBranch

        forkIO $ gitRevToPush "origin/master" fullRemoteBranchName mvRemoteCommitsToPush
        forkIO $ gitRevToPull "origin/master" fullRemoteBranchName mvRemoteCommitsToPull
        forkIO $ gitRevToPush fullRemoteBranchName "HEAD" mvCommitsToPush
        forkIO $ gitRevToPull fullRemoteBranchName "HEAD" mvCommitsToPull

        rCommitsToMergeStr <- takeMVar mvRemoteCommitsToPush
        let rCommitsToMerge = getCount rCommitsToMergeStr
        rCommitsToRMaserStr <- takeMVar mvRemoteCommitsToPull
        let rCommitsToRMaser = getCount rCommitsToRMaserStr

        commitsToPushStr <- takeMVar mvCommitsToPush
        let commitsToPush = getCount commitsToPushStr
        commitsToPullStr <- takeMVar mvCommitsToPull
        let commitsToPull = getCount commitsToPullStr

        outputRCommits rCommitsToMerge rCommitsToRMaser
        outputLocalBranchName localBranchName
        outputCommitsToPullPush commitsToPull commitsToPush

    outputRepoState repoState

    stashCountStr <- takeMVar mvStashCount
    outputStashCount stashCountStr

    -- Necessary to properly terminate the output
    putStrLn ""

checkInGitDirectory :: IO Bool
checkInGitDirectory = do
  (exCode, _, _) <- readProcessWithExitCode "git" ["rev-parse", "--git-dir"] ""
  return (exCode == ExitSuccess)

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
              -> MVar String   -- ^ the output mvar
              -> IO ()
gitRemoteName localBranchName out = do
  remoteName <- readProcessWithIgnoreExitCode "git" ["config", "--get", gitRemoteTrackingConfigKey localBranchName] ""
  putMVar out remoteName

gitRemoteBranchName :: String     -- ^ remote name
                    -> MVar String     -- ^ The output mvar
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
             -> MVar String      -- ^ The output mvar
             -> IO ()
gitRevToPush fromCommit toCommit out = do
  revToPush <- readProcessWithIgnoreExitCode "git" ["rev-list", "--right-only", "--count", mergeBaseDiffFromTo fromCommit toCommit] ""
  putMVar out revToPush

gitRevToPull :: String          -- ^ from revision
             -> String          -- ^ to revision
             -> MVar String      -- ^ The output mvar
             -> IO ()
gitRevToPull fromCommit toCommit out = do
  revToPull <- readProcessWithIgnoreExitCode "git" ["rev-list", "--left-only", "--count", mergeBaseDiffFromTo fromCommit toCommit] ""
  putMVar out revToPull

gitStashCount :: MVar String     -- ^ The output mvar
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
          when (push < 0) $ do
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

outputStashCount :: String
                 -> IO ()
outputStashCount stashCountStr = do
  let stashCount = getCount stashCountStr
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
