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
}
