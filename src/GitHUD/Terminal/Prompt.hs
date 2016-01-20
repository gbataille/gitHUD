module GitHUD.Terminal.Prompt (
  buildPromptWithConfig
  , resetPromptAtBeginning
  , addGitRepoIndicator
  , addNoTrackedUpstreamIndicator
  , addMergeBranchCommits
  , addLocalBranchName
  , addLocalCommits
  , addRepoState
  , addStashes
  , buildPrompt
  ) where

import Control.Applicative ((<$>))
import Control.Monad (when)
import Control.Monad.Writer (runWriterT, tell)

import GitHUD.Config.Types
import GitHUD.Git.Types
import GitHUD.Terminal.Base
import GitHUD.Terminal.Types
import GitHUD.Types

-- | From the state of the terminal (shell type + git info), builds a prompt to
-- | display by accumulating data in a Writer and returning it
buildPromptWithConfig :: TerminalState
buildPromptWithConfig = do
  (_, prompt) <- runWriterT buildPrompt
  return prompt

buildPrompt :: ShellOutput
buildPrompt = do
  config <- askConfig
  resetPromptAtBeginning
  when (confShowPartRepoIndicator config) $ addGitRepoIndicator
  when (confShowPartMergeBranchCommitsDiff config) $ addNoTrackedUpstreamIndicator
  when (confShowPartMergeBranchCommitsDiff config) $ addMergeBranchCommits
  when (confShowPartLocalBranch config) $ addLocalBranchName
  when (confShowPartCommitsToOrigin config) $ addLocalCommits
  when (confShowPartLocalChangesState config) $ addRepoState
  when (confShowPartStashes config) $ addStashes
  return ()

resetPromptAtBeginning :: ShellOutput
resetPromptAtBeginning =
  (endColorMarker <$> askShell) >>= tell

addGitRepoIndicator :: ShellOutput
addGitRepoIndicator = do
  config <- askConfig
  tell $ confRepoIndicator config
  tell " "

addNoTrackedUpstreamIndicator :: ShellOutput
addNoTrackedUpstreamIndicator = do
  repoState <- askRepoState
  config <- askConfig
  when (gitRemoteTrackingBranch repoState == "") $ do
    tellStringInColor
      (confNoTrackedUpstreamStringColor config)
      (confNoTrackedUpstreamStringIntensity config)
      (confNoTrackedUpstreamString config)
    tell " "
    tellStringInColor
      (confNoTrackedUpstreamIndicatorColor config)
      (confNoTrackedUpstreamIndicatorIntensity config)
      (confNoTrackedUpstreamIndicator config)
    tell " "
  return ()

addMergeBranchCommits :: ShellOutput
addMergeBranchCommits = do
  repoState <- askRepoState
  config <- askConfig
  let push = gitMergeBranchCommitsToPush repoState
  let pull = gitMergeBranchCommitsToPull repoState
  if (push > 0) && (pull > 0)
    then do
      tell (confMergeBranchCommitsIndicator config)
      tell " "
      tell . show $ pull
      tellStringInColor Green Vivid (confMergeBranchCommitsBothPullPush config)
      tell . show $ push
    else (
      if (pull > 0)
        then do
          tell (confMergeBranchCommitsIndicator config)
          tell " "
          tellStringInColor Green Vivid (confMergeBranchCommitsOnlyPull config)
          tell " "
          tell . show $ pull
        else (
          when (push > 0) $ do
            tell (confMergeBranchCommitsIndicator config)
            tell " "
            tellStringInColor Green Vivid (confMergeBranchCommitsOnlyPush config)
            tell " "
            tell . show $ push
        )
    )
  addSpaceIfAnyBiggerThanZero [pull, push]
  return ()

addLocalBranchName :: ShellOutput
addLocalBranchName = do
  repoState <- askRepoState
  config <- askConfig
  let localBranchName = gitLocalBranch repoState
  tell (confLocalBranchNamePrefix config)

  if (localBranchName /= "")
    then do
      tellStringInColor (confLocalBranchColor config) (confLocalBranchIntensity config) $
        localBranchName
    else do
      tellStringInColor (confLocalDetachedColor config) (confLocalDetachedIntensity config) $
        (confLocalDetachedPrefix config) ++ (gitCommitShortSHA repoState)

  tell (confLocalBranchNameSuffix config)
  tell " "
  return ()

addLocalCommits :: ShellOutput
addLocalCommits = do
  repoState <- askRepoState
  config <- askConfig
  let push = gitCommitsToPush repoState
  let pull = gitCommitsToPull repoState
  if (pull > 0) && (push > 0)
    then do
      tell . show $ pull
      tellStringInColor
        (confLocalCommitsPushPullInfixColor config)
        (confLocalCommitsPushPullInfixIntensity config)
        (confLocalCommitsPushPullInfix config)
      tell . show $ push
      tell " "
    else
      if (pull > 0)
        then do
          tell . show $ pull
          tellStringInColor
            (confLocalCommitsPullSuffixColor config)
            (confLocalCommitsPullSuffixIntensity config)
            (confLocalCommitsPullSuffix config)
          tell " "
        else
          when (push > 0) $ do
            tell . show $ push
            tellStringInColor
              (confLocalCommitsPushSuffixColor config)
              (confLocalCommitsPushSuffixIntensity config)
              (confLocalCommitsPushSuffix config)
            tell " "

  return ()

addRepoState :: ShellOutput
addRepoState = do
  repoState <- askRepoState
  config <- askConfig
  let repoChanges = gitLocalRepoChanges repoState

  let inda = indexAdd repoChanges
  let indd = indexDel repoChanges
  let indm = indexMod repoChanges
  let mv = renamed repoChanges
  addStateElem inda
    (confChangeIndexAddSuffixColor config)
    (confChangeIndexAddSuffixIntensity config)
    (confChangeIndexAddSuffix config)
  addStateElem indd
    (confChangeIndexDelSuffixColor config)
    (confChangeIndexDelSuffixIntensity config)
    (confChangeIndexDelSuffix config)
  addStateElem indm
    (confChangeIndexModSuffixColor config)
    (confChangeIndexModSuffixIntensity config)
    (confChangeIndexModSuffix config)
  addStateElem mv
    (confChangeRenamedSuffixColor config)
    (confChangeRenamedSuffixIntensity config)
    (confChangeRenamedSuffix config)
  addSpaceIfAnyBiggerThanZero [inda, indd, indm, mv]

  let ld = localDel repoChanges
  let lm = localMod repoChanges
  addStateElem ld
    (confChangeLocalDelSuffixColor config)
    (confChangeLocalDelSuffixIntensity config)
    (confChangeLocalDelSuffix config)
  addStateElem lm
    (confChangeLocalModSuffixColor config)
    (confChangeLocalModSuffixIntensity config)
    (confChangeLocalModSuffix config)
  addSpaceIfAnyBiggerThanZero [ld, lm]

  let la = localAdd repoChanges
  addStateElem la
    (confChangeLocalAddSuffixColor config)
    (confChangeLocalAddSuffixIntensity config)
    (confChangeLocalAddSuffix config)
  addSpaceIfAnyBiggerThanZero [la]

  let co = conflict repoChanges
  addStateElem co
    (confChangeConflictedSuffixColor config)
    (confChangeConflictedSuffixIntensity config)
    (confChangeConflictedSuffix config)
  addSpaceIfAnyBiggerThanZero [co]
  return ()

addSpaceIfAnyBiggerThanZero :: [Int] -> ShellOutput
addSpaceIfAnyBiggerThanZero list =
  when (any (>0) list) $ tell " "

addStateElem :: Int
             -> Color
             -> ColorIntensity
             -> String
             -> ShellOutput
addStateElem stateElem color intensity letter =
  when (stateElem > 0) $ addNumStateElem stateElem color intensity letter

addNumStateElem :: Int
                -> Color
                -> ColorIntensity
                -> String
                -> ShellOutput
addNumStateElem num color intensity letter = do
  tell . show $ num
  tellStringInColor color intensity letter
  return ()

addStashes :: ShellOutput
addStashes = do
  repoState <- askRepoState
  config <- askConfig
  let stashCount = gitStashCount repoState
  when (stashCount /= 0) $ do
    tell . show $ stashCount
    tellStringInColor
      (confStashSuffixColor config)
      (confStashSuffixIntensity config)
      (confStashSuffix config)
    tell " "

