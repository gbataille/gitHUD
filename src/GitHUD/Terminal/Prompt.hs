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

--   outputLocalBranchName (gitLocalBranch repoState) (gitCommitShortSHA repoState)
--   outputCommitsToPullPush (gitCommitsToPull repoState) (gitCommitsToPush repoState)
--   outputRepoState (gitLocalRepoChanges repoState)
--   outputStashCount (gitStashCount repoState)
--
-- outputLocalBranchName :: String       -- ^ the local branch name
--                       -> String         -- ^ the HEAD commit short sha
--                       -> ShellOutput
-- outputLocalBranchName localBranchName commitSHA = do
--   if (localBranchName /= "")
--     then liftIO $ do
--       putStr "["
--       mapM_ putStr (lines localBranchName)
--       putStr "]"
--       putStr " "
--     else do
--       liftIO . putStr $ "["
--       showStrInColor Yellow Vivid "detached@"
--       showStrInColor Yellow Vivid commitSHA
--       liftIO . putStr $ "]"
--       liftIO . putStr $ " "
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
-- outputCommitsToPullPush :: Int          -- ^ commits to pull
--                         -> Int          -- ^ commits to push
--                         -> ShellOutput
-- outputCommitsToPullPush pull push = do
--   if (pull > 0) && (push > 0)
--     then do
--       liftIO . putStr . show $ pull
--       showStrInColor Green Vivid "\8645"
--       liftIO . putStr . show $ push
--     else
--       if (pull > 0)
--         then outputcommitsToPull pull
--         else
--           when (push > 0) $ outputcommitsToPush push
--
--   when ((pull > 0) || (push > 0)) . liftIO . putStr $ " "
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
