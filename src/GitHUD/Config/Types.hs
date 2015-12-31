module GitHUD.Config.Types (
  Config(..)
  , defaultConfig
  ) where

data Config = Config {
  gitRepoIndicator :: String
} deriving (Eq, Show)

defaultConfig :: Config
defaultConfig = Config {
  gitRepoIndicator = "\57504"
}
