module GitHUD.Config.Types (
  Config(..)
  , defaultConfig
  ) where

import GitHUD.Terminal.Types

data Config = Config {
  confRepoIndicator :: String

  , confNoUpstreamString :: String
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
} deriving (Eq, Show)

defaultConfig :: Config
defaultConfig = Config {
  confRepoIndicator = "\57504"

  , confNoUpstreamString = "upstream"
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
}
