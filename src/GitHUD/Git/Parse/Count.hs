module GitHUD.Git.Parse.Count (
  getCount
  ) where

import Text.Parsec (parse)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (anyChar)
import Text.Parsec.Prim (many)

getCount :: String -> Int
getCount numberString =
  either
    (const 0)
    id
    (parse countParser "" numberString)

countParser :: Parser Int
countParser = do
  number <- many anyChar
  if null number
    then return 0
    else return (read number)

