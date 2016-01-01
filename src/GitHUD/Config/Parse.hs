module GitHUD.Config.Parse (
  parseConfigFile
  , commentParser
  , itemParser
  , fallThroughItemParser
  , ConfigItem(..)
  ) where

import Control.Monad (void, when)
import Text.Parsec.Char (anyChar, char, newline, letter)
import Text.Parsec.Combinator (choice, eof, manyTill)
import Text.Parsec.Prim (many, try, unexpected, (<|>), (<?>))
import Text.Parsec.String (parseFromFile, Parser)

import GitHUD.Config.Types

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
  conf { confNoUpstreamIndicatorColor = read noUpstreamIndColor }
configItemsFolder conf (Item "no_upstream_indicator_intensity" noUpstreamIndIntensity) =
  conf { confNoUpstreamIndicatorIntensity = read noUpstreamIndIntensity }

configItemsFolder conf _ = conf

