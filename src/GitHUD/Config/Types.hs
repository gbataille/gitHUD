module GitHUD.Config.Types (
  Config(..)
  , defaultConfig
  ) where

data Config = Config {
  confRepoIndicator :: String
} deriving (Eq, Show)

defaultConfig :: Config
defaultConfig = Config {
  confRepoIndicator = "\57504"
}
