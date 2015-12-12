module GitHUD.Git.Parse.Status (
  gitParseStatus
  ) where

import Text.Parsec (parse)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (anyChar, newline, noneOf, oneOf)
import Text.Parsec.Prim (many, (<?>), try)
import Text.Parsec.Combinator (choice)

import GitHUD.Git.Types

data GitFileState = LocalMod
                  | LocalAdd
                  | LocalDel
                  | IndexMod
                  | IndexAdd
                  | IndexDel
                  | Conflict
                  | Skip            -- ^ Used to skip an output. Necessary because we are parsing twice the output, ignoring certain lines on each pass
                  deriving (Show)

-- | In case of error, return zeroRepoState, i.e. no changes
gitParseStatus :: String -> GitLocalRepoChanges
gitParseStatus out =
  mergeGitLocalRepoChanges local index
  where local = (parseLocal out)
        index = (parseIndex out)

parseLocal :: String -> GitLocalRepoChanges
parseLocal str =
  either
    (const zeroLocalRepoChanges)
    id
    (parse localPorcelainStatusParser "" str)

parseIndex :: String -> GitLocalRepoChanges
parseIndex str =
  either
    (const zeroLocalRepoChanges)
    id
    (parse indexPorcelainStatusParser "" str)

localPorcelainStatusParser :: Parser GitLocalRepoChanges
localPorcelainStatusParser = gitLinesToLocalRepoState . many $ gitLocalLines

indexPorcelainStatusParser :: Parser GitLocalRepoChanges
indexPorcelainStatusParser = gitLinesToIndexRepoState . many $ gitIndexLines

gitLinesToLocalRepoState :: Parser [GitFileState] -> Parser GitLocalRepoChanges
gitLinesToLocalRepoState gitFileStateP = do
    gitFileState <- gitFileStateP
    return $ foldl linesStateFolder zeroLocalRepoChanges gitFileState

gitLinesToIndexRepoState :: Parser [GitFileState] -> Parser GitLocalRepoChanges
gitLinesToIndexRepoState gitFileStateP = do
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
linesStateFolder repoS (Skip) = repoS

gitLocalLines :: Parser GitFileState
gitLocalLines = do
    state <- localFileState
    newline
    return state

gitIndexLines :: Parser GitFileState
gitIndexLines = do
    state <- indexFileState
    newline
    return state

indexFileState :: Parser GitFileState
indexFileState = do
    state <- choice [
        indexModState
        , indexAddState
        , indexDelState
        -- Fallthrough to skip the lines indicating local modifications
        , skipLine
        ] <?> "local file state"
    many $ noneOf "\n"
    return state

localFileState :: Parser GitFileState
localFileState = do
    state <- choice [
        conflictState
        , localModState
        , localAddState
        , localDelState
        -- Fallthrough to skip the lines indicating index modifications
        , skipLine
        ] <?> "local file state"
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

skipLine :: Parser GitFileState
skipLine = anyChar >> return Skip

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
