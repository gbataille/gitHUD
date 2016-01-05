module GitHUD.Config.Types (
  Config(..)
  , defaultConfig
  ) where

import GitHUD.Terminal.Types

data Config = Config {
  confRepoIndicator :: String

  , confNoUpstreamString :: String
  , confNoUpstreamStringColor :: Color
  , confNoUpstreamStringIntensity :: ColorIntensity
  , confNoUpstreamIndicator :: String
  , confNoUpstreamIndicatorColor :: Color
  , confNoUpstreamIndicatorIntensity :: ColorIntensity

  , confRemoteCommitsIndicator :: String
  , confRemoteCommitsOnlyPush :: String
  , confRemoteCommitsOnlyPull :: String
  , confRemoteCommitsBothPullPush :: String

  , confLocalBranchNamePrefix :: String
  , confLocalBranchNameSuffix :: String
  , confLocalDetachedPrefix :: String
  , confLocalBranchColor :: Color
  , confLocalBranchIntensity :: ColorIntensity
  , confLocalDetachedColor :: Color
  , confLocalDetachedIntensity :: ColorIntensity

  , confLocalCommitsPushSuffix :: String
  , confLocalCommitsPushSuffixColor :: Color
  , confLocalCommitsPushSuffixIntensity :: ColorIntensity
  , confLocalCommitsPullSuffix :: String
  , confLocalCommitsPullSuffixColor :: Color
  , confLocalCommitsPullSuffixIntensity :: ColorIntensity
  , confLocalCommitsPushPullInfix :: String
  , confLocalCommitsPushPullInfixColor :: Color
  , confLocalCommitsPushPullInfixIntensity :: ColorIntensity

  , confChangeIndexAddSuffix :: String
  , confChangeIndexAddSuffixColor :: Color
  , confChangeIndexAddSuffixIntensity :: ColorIntensity
  , confChangeIndexModSuffix :: String
  , confChangeIndexModSuffixColor :: Color
  , confChangeIndexModSuffixIntensity :: ColorIntensity
  , confChangeIndexDelSuffix :: String
  , confChangeIndexDelSuffixColor :: Color
  , confChangeIndexDelSuffixIntensity :: ColorIntensity
  , confChangeLocalAddSuffix :: String
  , confChangeLocalAddSuffixColor :: Color
  , confChangeLocalAddSuffixIntensity :: ColorIntensity
  , confChangeLocalModSuffix :: String
  , confChangeLocalModSuffixColor :: Color
  , confChangeLocalModSuffixIntensity :: ColorIntensity
  , confChangeLocalDelSuffix :: String
  , confChangeLocalDelSuffixColor :: Color
  , confChangeLocalDelSuffixIntensity :: ColorIntensity
  , confChangeRenamedSuffix :: String
  , confChangeRenamedSuffixColor :: Color
  , confChangeRenamedSuffixIntensity :: ColorIntensity
  , confChangeConflictedSuffix :: String
  , confChangeConflictedSuffixColor :: Color
  , confChangeConflictedSuffixIntensity :: ColorIntensity

  , confStashSuffix :: String
  , confStashSuffixColor :: Color
  , confStashSuffixIntensity :: ColorIntensity
} deriving (Eq, Show)

defaultConfig :: Config
defaultConfig = Config {
  confRepoIndicator = "\57504"

  , confNoUpstreamString = "upstream"
  , confNoUpstreamStringColor = Red
  , confNoUpstreamStringIntensity = Vivid
  , confNoUpstreamIndicator = "\9889"
  , confNoUpstreamIndicatorColor = Red
  , confNoUpstreamIndicatorIntensity = Vivid

  , confRemoteCommitsIndicator = "\120366"
  , confRemoteCommitsOnlyPush = "\8592"
  , confRemoteCommitsOnlyPull = "\8594"
  , confRemoteCommitsBothPullPush = "\8644"

  , confLocalBranchNamePrefix = "["
  , confLocalBranchNameSuffix = "]"
  , confLocalDetachedPrefix = "detached@"
  , confLocalBranchColor = Blue
  , confLocalBranchIntensity = Vivid
  , confLocalDetachedColor = Yellow
  , confLocalDetachedIntensity = Vivid

  , confLocalCommitsPushSuffix = "\8593"
  , confLocalCommitsPushSuffixColor = Green
  , confLocalCommitsPushSuffixIntensity = Vivid
  , confLocalCommitsPullSuffix = "\8595"
  , confLocalCommitsPullSuffixColor = Red
  , confLocalCommitsPullSuffixIntensity = Vivid
  , confLocalCommitsPushPullInfix = "\8645"
  , confLocalCommitsPushPullInfixColor = Green
  , confLocalCommitsPushPullInfixIntensity = Vivid

  , confChangeIndexAddSuffix = "A"
  , confChangeIndexAddSuffixColor = Green
  , confChangeIndexAddSuffixIntensity = Vivid
  , confChangeIndexModSuffix = "M"
  , confChangeIndexModSuffixColor = Green
  , confChangeIndexModSuffixIntensity = Vivid
  , confChangeIndexDelSuffix = "D"
  , confChangeIndexDelSuffixColor = Green
  , confChangeIndexDelSuffixIntensity = Vivid
  , confChangeLocalAddSuffix = "A"
  , confChangeLocalAddSuffixColor = White
  , confChangeLocalAddSuffixIntensity = Vivid
  , confChangeLocalModSuffix = "M"
  , confChangeLocalModSuffixColor = Red
  , confChangeLocalModSuffixIntensity = Vivid
  , confChangeLocalDelSuffix = "D"
  , confChangeLocalDelSuffixColor = Red
  , confChangeLocalDelSuffixIntensity = Vivid
  , confChangeRenamedSuffix = "R"
  , confChangeRenamedSuffixColor = Green
  , confChangeRenamedSuffixIntensity = Vivid
  , confChangeConflictedSuffix = "C"
  , confChangeConflictedSuffixColor = Green
  , confChangeConflictedSuffixIntensity = Vivid

  , confStashSuffix = "≡"
  , confStashSuffixColor = Green
  , confStashSuffixIntensity = Vivid
}
