module GitHUD.Terminal.Prompt (
  buildPromptWithConfig
  ) where

import Control.Monad (when)
import Control.Monad.Writer (runWriterT, tell)

import GitHUD.Git.Types
import GitHUD.Terminal.Base
import GitHUD.Terminal.Types

-- | From the state of the terminal (shell type + git info), builds a prompt to
-- | display by accumulating data in a Writer and returning it
buildPromptWithConfig :: TerminalState
buildPromptWithConfig = do
  (_, prompt) <- runWriterT buildPrompt
  return prompt

buildPrompt :: ShellOutput
buildPrompt = do
  addGitRepoIndicator
  addUpstreamIndicator
  addRemoteCommits
  addLocalBranchName
  addLocalCommits
  addRepoState
  addStashes
  return ()

addGitRepoIndicator :: ShellOutput
addGitRepoIndicator = tell "\57504 "

addUpstreamIndicator :: ShellOutput
addUpstreamIndicator = do
  repoState <- getRepoState
  when (gitRemoteTrackingBranch repoState == "") $ do
    tell "upstream "
    tellStringInColor Red Vivid "\9889"
    tell " "
  return ()

addRemoteCommits :: ShellOutput
addRemoteCommits = do
  repoState <- getRepoState
  let push = gitRemoteCommitsToPush repoState
  let pull = gitRemoteCommitsToPull repoState
  if (push > 0) && (pull > 0)
    then do
      tell "\120366 "
      tell . show $ pull
      tellStringInColor Green Vivid "\8644"
      tell . show $ push
      tell " "
    else (
      if (pull > 0)
        then do
          tell "\120366 "
          tellStringInColor Green Vivid "\8594 "
          tell . show $ pull
          tell " "
        else (
          when (push > 0) $ do
            tell "\120366 "
            tellStringInColor Green Vivid "\8592 "
            tell . show $ push
        )
    )
  return ()

addLocalBranchName :: ShellOutput
addLocalBranchName = do
  repoState <- getRepoState
  let localBranchName = gitLocalBranch repoState
  tell "["

  if (localBranchName /= "")
    then do
      tell localBranchName
    else do
      tellStringInColor Yellow Vivid $ "detached@" ++ (gitCommitShortSHA repoState)

  tell "] "
  return ()

addLocalCommits :: ShellOutput
addLocalCommits = do
  repoState <- getRepoState
  let push = gitCommitsToPush repoState
  let pull = gitCommitsToPull repoState
  if (pull > 0) && (push > 0)
    then do
      tell . show $ pull
      tellStringInColor Green Vivid "\8645"
      tell . show $ push
      tell " "
    else
      if (pull > 0)
        then do
          tell . show $ pull
          tellStringInColor Red Vivid "\8595 "
          tell " "
        else
          when (push > 0) $ do
            tell . show $ push
            tellStringInColor Green Vivid "\8593"
            tell " "

  return ()

addRepoState :: ShellOutput
addRepoState = do
  repoState <- getRepoState
  let repoChanges = gitLocalRepoChanges repoState

  let inda = indexAdd repoChanges
  let indd = indexDel repoChanges
  let indm = indexMod repoChanges
  let mv = renamed repoChanges
  addStateElem inda Green Vivid "A"
  addStateElem indd Green Vivid "D"
  addStateElem indm Green Vivid "M"
  addStateElem mv Green Vivid "R"
  addSpaceIfAnyBiggerThanZero [inda, indd, indm, mv]

  let ld = localDel repoChanges
  let lm = localMod repoChanges
  addStateElem ld Red Vivid "D"
  addStateElem lm Red Vivid "M"
  addSpaceIfAnyBiggerThanZero [ld, lm]

  let la = localAdd repoChanges
  addStateElem la White Vivid "A"
  addSpaceIfAnyBiggerThanZero [la]

  let co = conflict repoChanges
  addStateElem co Green Vivid "C"
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
  repoState <- getRepoState
  let stashCount = gitStashCount repoState
  when (stashCount /= 0) $ do
    tell . show $ stashCount
    tellStringInColor Green Vivid "≡ "

