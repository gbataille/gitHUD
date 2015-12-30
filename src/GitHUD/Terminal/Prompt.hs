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
        else
          when (push > 0) $ do
            tell . show $ push
            tellStringInColor Green Vivid "\8593"

  return ()

--   outputCommitsToPullPush (gitCommitsToPull repoState) (gitCommitsToPush repoState)
--   outputRepoState (gitLocalRepoChanges repoState)
--   outputStashCount (gitStashCount repoState)
--
-- outputcommitsToPush :: Int
--                     -> ShellOutput
-- outputcommitsToPush commitCount = do
--   when (commitCount > 0) $ do
--     liftIO . putStr . show $ commitCount
--     showStrInColor Green Vivid "\8593"
--
-- outputcommitsToPull :: Int
--                     -> ShellOutput
-- outputcommitsToPull commitCount = do
--   when (commitCount > 0) $ do
--     liftIO . putStr . show $ commitCount
--     showStrInColor Red Vivid "\8595"
--
-- outputStashCount :: Int
--                  -> ShellOutput
-- outputStashCount stashCount = do
--   when (stashCount /= 0) $ do
--     liftIO . putStr . show $ stashCount
--     showStrInColor Green Vivid "≡ "
--
-- outputRepoState :: GitLocalRepoChanges
--                 -> ShellOutput
-- outputRepoState repoState = do
--   showElem indexAdd repoState Green Vivid "A"
--   showElem indexDel repoState Green Vivid "D"
--   showElem indexMod repoState Green Vivid "M"
--   showElem renamed  repoState Green Vivid "R"
--   when ((indexAdd repoState > 0) || (indexDel repoState > 0) || (indexMod repoState > 0) || (renamed repoState > 0)) . liftIO . putStr $ " "
--
--   showElem localDel repoState Red Vivid "D"
--   showElem localMod repoState Red Vivid "M"
--   when ((localDel repoState > 0) || (localMod repoState > 0)) . liftIO . putStr $ " "
--
--   showElem localAdd repoState White Vivid "A"
--   when (localAdd repoState > 0) . liftIO . putStr $ " "
--
--   showElem conflict repoState Green Vivid "C"
--   when (conflict repoState > 0) . liftIO . putStr $ " "
--
-- showElem :: (GitLocalRepoChanges -> Int)
--          -> GitLocalRepoChanges
--          -> Color
--          -> ColorIntensity
--          -> String
--          -> ShellOutput
-- showElem elemFunc repoState color intensity letter = do
--   let num = elemFunc repoState
--   when (num > 0) $ showNumState num color intensity letter
--
-- showNumState :: Int
--          -> Color
--          -> ColorIntensity
--          -> String
--          -> ShellOutput
-- showNumState num color intensity letter = do
--     liftIO . putStr . show $ num
--     showStrInColor color intensity letter
