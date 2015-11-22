module GitHUD (
    githud
    ) where

import System.Process (readProcess)
import Text.Parsec (runParser, Parsec)
import Text.Parsec.Char (anyChar, char, newline, noneOf, space, oneOf)
import Text.Parsec.Prim (many, (<?>), try)
import Text.Parsec.Combinator (manyTill, choice)
import Text.Parsec.Error (ParseError)

type GitHUDParser = Parsec String ()

data GitFileState = LocalMod
                  | LocalAdd
                  | LocalDel
                  | IndexMod
                  | IndexAdd
                  | IndexDel
                  | Untracked
                  | Conflict
                  deriving (Show)

data GitRepoState = GitRepoState { localMod :: Int
                                 , localAdd :: Int
                                 , localDel :: Int
                                 , indexMod :: Int
                                 , indexAdd :: Int
                                 , indexDel :: Int
                                 , untracked :: Int
                                 , conflict :: Int
                                 } deriving (Show)

zeroRepoState = GitRepoState { localMod = 0
                             , localAdd = 0
                             , localDel = 0
                             , indexMod = 0
                             , indexAdd = 0
                             , indexDel = 0
                             , untracked = 0
                             , conflict = 0
                             }

githud :: IO ()
githud = do
  -- TODO: gbataille - Check that we are in a git repo
  out <- gitPorcelainStatus
  let parsed = runParser porcelainStatusParser () "" out
  outputParsed parsed

-- | Assumes that we are in a git repo
gitPorcelainStatus :: IO String
gitPorcelainStatus = readProcess "git" ["status", "--porcelain"] ""

porcelainStatusParser :: GitHUDParser String
porcelainStatusParser = gitRepoStateToString . gitLinesToRepoState . many $ gitLines

gitRepoStateToString :: GitHUDParser GitRepoState -> GitHUDParser String
gitRepoStateToString repoStateP = do
    repoState <- repoStateP
    return $ show repoState

gitLinesToRepoState :: GitHUDParser [GitFileState] -> GitHUDParser GitRepoState
gitLinesToRepoState linesP = do
    lines <- linesP
    return $ foldl linesStateFolder zeroRepoState lines

linesStateFolder :: GitRepoState -> GitFileState -> GitRepoState
linesStateFolder repoS (LocalMod) = repoS { localMod = (localMod repoS) + 1 }
linesStateFolder repoS (LocalAdd) = repoS { localAdd = (localAdd repoS) + 1 }
linesStateFolder repoS (LocalDel) = repoS { localDel = (localDel repoS) + 1 }
linesStateFolder repoS (IndexMod) = repoS { indexMod = (indexMod repoS) + 1 }
linesStateFolder repoS (IndexAdd) = repoS { indexAdd = (indexAdd repoS) + 1 }
linesStateFolder repoS (IndexDel) = repoS { indexDel = (indexDel repoS) + 1 }
linesStateFolder repoS (Untracked) = repoS { untracked = (untracked repoS) + 1 }
linesStateFolder repoS (Conflict) = repoS { conflict = (conflict repoS) + 1 }

gitLines :: GitHUDParser GitFileState
gitLines = do
    state <- fileState
    newline
    return state

fileState :: GitHUDParser GitFileState
fileState = do
    state <- choice [
        conflictState
        , localModState
        , localAddState
        , localDelState
        , indexModState
        , indexAddState
        , indexDelState
        , untrackedFile
        ] <?> "file state"
    many $ noneOf "\n"
    return state

conflictState :: GitHUDParser GitFileState
conflictState = try $ do
    oneOf "DAU"
    oneOf "DAU"
    return Conflict

localModState :: GitHUDParser GitFileState
localModState = try $ do
    space
    char 'M'
    return LocalMod

localAddState :: GitHUDParser GitFileState
localAddState = try $ do
    space
    char 'A'
    return LocalAdd

localDelState :: GitHUDParser GitFileState
localDelState = try $ do
    space
    char 'D'
    return LocalDel

indexModState :: GitHUDParser GitFileState
indexModState = try $ do
    char 'M'
    space
    return IndexMod

indexAddState :: GitHUDParser GitFileState
indexAddState = try $ do
    char 'A'
    space
    return IndexAdd

indexDelState :: GitHUDParser GitFileState
indexDelState = try $ do
    char 'D'
    space
    return IndexDel

untrackedFile :: GitHUDParser GitFileState
untrackedFile = try $ do
    char '?'
    char '?'
    return Untracked

outputParsed :: Either ParseError String -> IO ()
outputParsed (Left error) = print error
outputParsed (Right str) = print str
