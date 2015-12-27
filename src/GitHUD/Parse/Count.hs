module GitHUD.Parse.Count (
  getCount
  ) where

import Text.Parsec (parse)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (anyChar)
import Text.Parsec.Prim (many)

getCount :: String -> Int
getCount numberString =
  either
  (\_ -> 0)
  (id)
  (parse countParser "" numberString)

countParser :: Parser Int
countParser = do
  read <$> many anyChar

