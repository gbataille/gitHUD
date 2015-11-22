module GitHUD (
    githud
    ) where

import System.Process (readProcess)
import Text.Parsec (runParser, Parsec)
import Text.Parsec.Char (anyChar, newline, noneOf)
import Text.Parsec.Prim (many)
import Text.Parsec.Combinator (manyTill)
import Text.Parsec.Error (ParseError)

type GitParser = Parsec String () String

githud :: IO ()
githud = do
  -- TODO: gbataille - Check that we are in a git repo
  out <- gitPorcelainStatus
  let parsed = runParser porcelainStatusParser () "" out
  outputParsed parsed

-- | Assumes that we are in a git repo
gitPorcelainStatus :: IO String
gitPorcelainStatus = readProcess "git" ["status", "--porcelain"] ""

porcelainStatusParser :: GitParser
porcelainStatusParser = concat <$> many gitLines

gitLines :: GitParser
gitLines = do
    line <- many $ noneOf "\n"
    newline
    return line

outputParsed :: Either ParseError String -> IO ()
outputParsed (Left error) = print error
outputParsed (Right str) = print str
