module GitHUD.Git.Parse.Status (
  gitParseStatus
  ) where

import Text.Parsec (parse)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (newline, noneOf, oneOf)
import Text.Parsec.Prim (many, (<?>), try)
import Text.Parsec.Combinator (choice)

import GitHUD.Git.Types (zeroLocalRepoChanges, GitLocalRepoChanges(..))

data GitFileState = LocalMod
                  | LocalAdd
                  | LocalDel
                  | IndexMod
                  | IndexAdd
                  | IndexDel
                  | Conflict
                  deriving (Show)

-- | In case of error, return zeroRepoState, i.e. no changes
gitParseStatus :: String -> GitLocalRepoChanges
gitParseStatus out =
  either
   (const zeroLocalRepoChanges)
   id
   (parse porcelainStatusParser "" out)

porcelainStatusParser :: Parser GitLocalRepoChanges
porcelainStatusParser = gitLinesToRepoState . many $ gitLines

gitLinesToRepoState :: Parser [GitFileState] -> Parser GitLocalRepoChanges
gitLinesToRepoState gitFileStateP = do
    gitFileState <- gitFileStateP
    return $ foldl linesStateFolder zeroLocalRepoChanges gitFileState

linesStateFolder :: GitLocalRepoChanges -> GitFileState -> GitLocalRepoChanges
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
localModState = twoCharParser "DAM " "M" LocalMod

localAddState :: Parser GitFileState
localAddState = twoCharParser "?" "?" LocalAdd

localDelState :: Parser GitFileState
localDelState = twoCharParser "DAM " "D" LocalDel

indexModState :: Parser GitFileState
indexModState = twoCharParser "M" "DAM " IndexMod

indexAddState :: Parser GitFileState
indexAddState = twoCharParser "A" "DAM " IndexAdd

indexDelState :: Parser GitFileState
indexDelState = twoCharParser "D" "DAM " IndexDel
