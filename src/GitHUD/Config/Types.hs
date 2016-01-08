module GitHUD.Config.Types (
  Config(..)
  , defaultConfig
  ) where

import GitHUD.Terminal.Types

data Config = Config {
    confShowPartRepoIndicator :: Bool
  , confShowPartMergeBranchCommitsDiff :: Bool
  , confShowPartLocalBranch :: Bool
  , confShowPartCommitsToOrigin :: Bool
  , confShowPartLocalChangesState :: Bool
  , confShowPartStashes :: Bool

  , confRepoIndicator :: String

  , confNoTrackedUpstreamString :: String
  , confNoTrackedUpstreamStringColor :: Color
  , confNoTrackedUpstreamStringIntensity :: ColorIntensity
  , confNoTrackedUpstreamIndicator :: String
  , confNoTrackedUpstreamIndicatorColor :: Color
  , confNoTrackedUpstreamIndicatorIntensity :: ColorIntensity

  , confMergeBranchCommitsIndicator :: String
  , confMergeBranchCommitsOnlyPush :: String
  , confMergeBranchCommitsOnlyPull :: String
  , confMergeBranchCommitsBothPullPush :: String

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
    confShowPartRepoIndicator = True
  , confShowPartMergeBranchCommitsDiff = True
  , confShowPartLocalBranch = True
  , confShowPartCommitsToOrigin = True
  , confShowPartLocalChangesState = True
  , confShowPartStashes = True

  , confRepoIndicator = "ᚴ"

  , confNoTrackedUpstreamString = "upstream"
  , confNoTrackedUpstreamStringColor = Red
  , confNoTrackedUpstreamStringIntensity = Vivid
  , confNoTrackedUpstreamIndicator = "\9889"
  , confNoTrackedUpstreamIndicatorColor = Red
  , confNoTrackedUpstreamIndicatorIntensity = Vivid

  , confMergeBranchCommitsIndicator = "\120366"
  , confMergeBranchCommitsOnlyPush = "\8592"
  , confMergeBranchCommitsOnlyPull = "\8594"
  , confMergeBranchCommitsBothPullPush = "\8644"

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
  , confLocalCommitsPushPullInfix = "⥯"
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
