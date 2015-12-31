module GitHUD.Config.Parse (
  parseConfigFile
  ) where

import Control.Monad (when)
import Text.Parsec.Char (anyChar, char, newline, letter)
import Text.Parsec.Combinator (choice, eof, manyTill)
import Text.Parsec.Prim (try, unexpected, (<|>), (<?>))
import Text.Parsec.String (parseFromFile, Parser)

import GitHUD.Config.Types

data ConfigItem = Item String String
                | Comment
                | ErrorLine

parseConfigFile :: FilePath -> IO Config
parseConfigFile filePath = do
  eitherParsed <- parseFromFile configFileParser filePath
  return $ either
    (const defaultConfig)
    id
    eitherParsed

configFileParser :: Parser Config
configFileParser = do
  items <- manyTill configItemParser eof
  return $ foldl configItemsFolder defaultConfig items

configItemParser :: Parser ConfigItem
configItemParser = choice [
  commentParser
  , itemParser
  , fallThroughItemParser
  ] <?> "config file line"

commentParser :: Parser ConfigItem
commentParser = try $ do
  char '#'
  manyTill anyChar (try newline)
  return Comment

itemParser :: Parser ConfigItem
itemParser = try $ do
  key <- manyTill validKeyChar (char '=')
  when (key == "") $ unexpected "A key in the config file should not be empty"
  value <- manyTill anyChar (try newline)
  return $ Item key value

validKeyChar :: Parser Char
validKeyChar = letter <|> (char '_')

fallThroughItemParser :: Parser ConfigItem
fallThroughItemParser = do
  manyTill anyChar (try newline)
  return ErrorLine

configItemsFolder :: Config -> ConfigItem -> Config
configItemsFolder conf (Item "git_repo_indicator" repoIndicator) = conf { gitRepoIndicator = repoIndicator }
configItemsFolder conf _ = conf

