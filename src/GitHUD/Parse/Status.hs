module GitHUD.Parse.Status (
  gitParseStatus
  , zeroRepoState
  , GitRepoState(..)
  ) where

import Text.Parsec (parse)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (newline, noneOf, oneOf)
import Text.Parsec.Prim (many, (<?>), try)
import Text.Parsec.Combinator (choice)

data GitFileState = LocalMod
                  | LocalAdd
                  | LocalDel
                  | IndexMod
                  | IndexAdd
                  | IndexDel
                  | Conflict
                  deriving (Show)

data GitRepoState = GitRepoState { localMod :: Int
                                 , localAdd :: Int
                                 , localDel :: Int
                                 , indexMod :: Int
                                 , indexAdd :: Int
                                 , indexDel :: Int
                                 , conflict :: Int
                                 } deriving (Show, Eq)
zeroRepoState :: GitRepoState
zeroRepoState = GitRepoState { localMod = 0
                             , localAdd = 0
                             , localDel = 0
                             , indexMod = 0
                             , indexAdd = 0
                             , indexDel = 0
                             , conflict = 0
                             }

-- | In case of error, return zeroRepoState, i.e. no changes
gitParseStatus :: String -> GitRepoState
gitParseStatus out = (either
               (\_ -> zeroRepoState)
               (id)
               (parse porcelainStatusParser "" out)
               )

porcelainStatusParser :: Parser GitRepoState
porcelainStatusParser = gitLinesToRepoState . many $ gitLines

gitLinesToRepoState :: Parser [GitFileState] -> Parser GitRepoState
gitLinesToRepoState gitFileStateP = do
    gitFileState <- gitFileStateP
    return $ foldl linesStateFolder zeroRepoState gitFileState

linesStateFolder :: GitRepoState -> GitFileState -> GitRepoState
linesStateFolder repoS (LocalMod) = repoS { localMod = (localMod repoS) + 1 }
linesStateFolder repoS (LocalAdd) = repoS { localAdd = (localAdd repoS) + 1 }
linesStateFolder repoS (LocalDel) = repoS { localDel = (localDel repoS) + 1 }
linesStateFolder repoS (IndexMod) = repoS { indexMod = (indexMod repoS) + 1 }
linesStateFolder repoS (IndexAdd) = repoS { indexAdd = (indexAdd repoS) + 1 }
linesStateFolder repoS (IndexDel) = repoS { indexDel = (indexDel repoS) + 1 }
linesStateFolder repoS (Conflict) = repoS { conflict = (conflict repoS) + 1 }

gitLines :: Parser GitFileState
gitLines = do
    state <- fileState
    newline
    return state

fileState :: Parser GitFileState
fileState = do
    state <- choice [
        conflictState
        , localModState
        , localAddState
        , localDelState
        , indexModState
        , indexAddState
        , indexDelState
        ] <?> "file state"
    many $ noneOf "\n"
    return state

-- | Parser of 2 characters exactly that returns a specific State
twoCharParser :: [Char]           -- ^ List of allowed first Char to be matched
              -> [Char]           -- ^ List of allowed second Char to be matched
              -> GitFileState   -- ^ the GitFileState to return as output
              -> Parser GitFileState
twoCharParser first second state = try $ do
  oneOf first
  oneOf second
  return state

conflictState :: Parser GitFileState
conflictState = twoCharParser "DAU" "DAU" Conflict

localModState :: Parser GitFileState
localModState = twoCharParser " " "M" LocalMod

localAddState :: Parser GitFileState
localAddState = twoCharParser "?" "?" LocalAdd

localDelState :: Parser GitFileState
localDelState = twoCharParser " " "D" LocalDel

indexModState :: Parser GitFileState
indexModState = twoCharParser "M" " " IndexMod

indexAddState :: Parser GitFileState
indexAddState = twoCharParser "A" " " IndexAdd

indexDelState :: Parser GitFileState
indexDelState = twoCharParser "D" " " IndexDel
