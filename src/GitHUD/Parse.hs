module GitHUD.Parse (
  gitParse
  , GitRepoState()
  , localAdd, localDel, localMod
  ) where

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

gitParse :: String -> GitRepoState
gitParse out = either (\_ -> zeroRepoState) (id) (runParser porcelainStatusParser () "" out)

porcelainStatusParser :: GitHUDParser GitRepoState
porcelainStatusParser = gitLinesToRepoState . many $ gitLines

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

-- | Parser of 2 characters exactly that returns a specific State
twoCharParser :: [Char]           -- ^ List of allowed first Char to be matched
              -> [Char]           -- ^ List of allowed second Char to be matched
              -> GitFileState   -- ^ the GitFileState to return as output
              -> GitHUDParser GitFileState
twoCharParser first second state = try $ do
  oneOf first
  oneOf second
  return state

conflictState :: GitHUDParser GitFileState
conflictState = twoCharParser "DAU" "DAU" Conflict

localModState :: GitHUDParser GitFileState
localModState = twoCharParser " " "M" LocalMod

localAddState :: GitHUDParser GitFileState
localAddState = twoCharParser " " "A" LocalAdd

localDelState :: GitHUDParser GitFileState
localDelState = twoCharParser " " "D" LocalDel

indexModState :: GitHUDParser GitFileState
indexModState = twoCharParser "M" " " IndexMod

indexAddState :: GitHUDParser GitFileState
indexAddState = twoCharParser "A" " " IndexAdd

indexDelState :: GitHUDParser GitFileState
indexDelState = twoCharParser "D" " " IndexDel

untrackedFile :: GitHUDParser GitFileState
untrackedFile = twoCharParser "?" "?" Untracked

