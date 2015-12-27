module GitHUD.Git.Common (
  gitRemoteTrackingConfigKey
  , gitRemoteBranchConfigKey
  , mergeBaseDiffFromTo
  ) where


gitRemoteTrackingConfigKey :: String -> String
gitRemoteTrackingConfigKey localBranchName = "branch." ++ localBranchName ++ ".remote"

gitRemoteBranchConfigKey :: String -> String
gitRemoteBranchConfigKey localBranchName = "branch." ++ localBranchName ++ ".merge"

mergeBaseDiffFromTo :: String -> String -> String
mergeBaseDiffFromTo fromCommit toCommit = fromCommit ++ "..." ++ toCommit

