module GitHUD.Config.Parse (
  parseConfigFile
  , commentParser
  , itemParser
  , fallThroughItemParser
  , configItemsFolder
  , ConfigItem(..)
  , colorConfigToColor
  , intensityConfigToIntensity
  ) where

import Control.Monad (void, when)
import Text.Parsec (parse)
import Text.Parsec.Char (anyChar, char, newline, letter, string)
import Text.Parsec.Combinator (choice, eof, manyTill)
import Text.Parsec.Prim (many, try, unexpected, (<|>), (<?>))
import Text.Parsec.String (parseFromFile, Parser)

import GitHUD.Config.Types
import GitHUD.Terminal.Types

data ConfigItem = Item String String
                | Comment
                | ErrorLine deriving (Eq, Show)

parseConfigFile :: FilePath -> IO Config
parseConfigFile filePath = do
  eitherParsed <- parseFromFile configFileParser filePath
  return $ either
    (const defaultConfig)
    id
    eitherParsed

configFileParser :: Parser Config
configFileParser = do
  items <- many configItemParser
  return $ foldl configItemsFolder defaultConfig items

configItemParser :: Parser ConfigItem
configItemParser = choice [
  commentParser
  , itemParser
  , fallThroughItemParser
  ] <?> "config file line"

endItem :: Parser ()
endItem = choice [
  void newline
  , eof
  ] <?> "end of item"

commentParser :: Parser ConfigItem
commentParser = try $ do
  char '#'
  manyTill anyChar (try endItem)
  return Comment

itemParser :: Parser ConfigItem
itemParser = try $ do
  key <- manyTill validKeyChar (char '=')
  when (key == "") $ unexpected "A key in the config file should not be empty"
  value <- manyTill anyChar (try endItem)
  return $ Item key value

validKeyChar :: Parser Char
validKeyChar = letter <|> (char '_')

-- | Must not be able to process an empty string
-- This is mandated by the use of 'many' in configFileParser
-- Therefore the definition `manyTill anyChar eof` is invalid, thus using newline
fallThroughItemParser :: Parser ConfigItem
fallThroughItemParser = do
  manyTill anyChar (try newline)
  return ErrorLine

configItemsFolder :: Config -> ConfigItem -> Config
configItemsFolder conf (Item "git_repo_indicator" repoIndicator) = conf { confRepoIndicator = repoIndicator }

configItemsFolder conf (Item "no_upstream_text" noUpstreamText) =
  conf { confNoUpstreamString = noUpstreamText }
configItemsFolder conf (Item "no_upstream_indicator" noUpstreamIndicator) =
  conf { confNoUpstreamIndicator = noUpstreamIndicator }
configItemsFolder conf (Item "no_upstream_indicator_color" noUpstreamIndColor) =
  conf { confNoUpstreamIndicatorColor = colorConfigToColor noUpstreamIndColor }
configItemsFolder conf (Item "no_upstream_indicator_intensity" noUpstreamIndIntensity) =
  conf { confNoUpstreamIndicatorIntensity = intensityConfigToIntensity noUpstreamIndIntensity }

configItemsFolder conf (Item "remote_commits_indicator" value) =
  conf { confRemoteCommitsIndicator = value }
configItemsFolder conf (Item "remote_commits_pull_prefix" value) =
  conf { confRemoteCommitsOnlyPull = value }
configItemsFolder conf (Item "remote_commits_push_prefix" value) =
  conf { confRemoteCommitsOnlyPush = value }
configItemsFolder conf (Item "remote_commits_push_pull_infix" value) =
  conf { confRemoteCommitsBothPullPush = value }

configItemsFolder conf (Item "local_branch_prefix" value) =
  conf { confLocalBranchNamePrefix = value }
configItemsFolder conf (Item "local_branch_suffix" value) =
  conf { confLocalBranchNameSuffix = value }
configItemsFolder conf (Item "local_branch_color" value) =
  conf { confLocalBranchColor = colorConfigToColor value }
configItemsFolder conf (Item "local_branch_intensity" value) =
  conf { confLocalBranchIntensity = intensityConfigToIntensity value }
configItemsFolder conf (Item "local_detached_prefix" value) =
  conf { confLocalDetachedPrefix = value }
configItemsFolder conf (Item "local_detached_color" value) =
  conf { confLocalDetachedColor = colorConfigToColor value }
configItemsFolder conf (Item "local_detached_intensity" value) =
  conf { confLocalDetachedIntensity = intensityConfigToIntensity value }

configItemsFolder conf _ = conf

colorConfigToColor :: String -> Color
colorConfigToColor str =
  either
    (const Blue)
    id
    (parse colorParser "" str)

colorParser :: Parser Color
colorParser = choice [
    string "Black"   >> return Black
  , string "Red"     >> return Red
  , string "Green"   >> return Green
  , string "Yellow"  >> return Yellow
  , string "Blue"    >> return Blue
  , string "Magenta" >> return Magenta
  , string "Cyan"    >> return Cyan
  , string "White"   >> return White
  ] <?> "color"

intensityConfigToIntensity :: String -> ColorIntensity
intensityConfigToIntensity str =
  either
    (const Vivid)
    id
    (parse intensityParser "" str)

intensityParser :: Parser ColorIntensity
intensityParser = choice [
    string "Dull" >> return Dull
  , string "Vivid" >> return Vivid
  ] <?> "intensity"
