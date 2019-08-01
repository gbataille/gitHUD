module GitHUD.Config.Parse (
  parseConfigFile
  , commentParser
  , itemParser
  , fallThroughItemParser
  , configItemsFolder
  , ConfigItem(..)
  , colorConfigToColor
  , intensityConfigToIntensity
  , stringConfigToStringList
  , redirectionParser
  , strConfigToRedirection
  , boolConfigToBool
  , intConfigToInt
  ) where

import Control.Monad (void, when)
import System.Posix.Daemon (Redirection(ToFile, DevNull))
import Text.Parsec (parse)
import Text.Parsec.Char (anyChar, char, digit, newline, noneOf, letter, spaces, string)
import Text.Parsec.Combinator (choice, eof, many1, manyTill, optional, sepBy)
import Text.Parsec.Prim (many, try, unexpected, (<|>), (<?>))
import Text.Parsec.String (parseFromFile, Parser)

import GitHUD.Config.Types
import GitHUD.Terminal.Types

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
configItemsFolder conf (Item "show_part_repo_indicator" value) =
  conf { confShowPartRepoIndicator = boolConfigToIntensity value }
configItemsFolder conf (Item "show_part_merge_branch_commits_diff" value) =
  conf { confShowPartMergeBranchCommitsDiff = boolConfigToIntensity value }
configItemsFolder conf (Item "show_part_local_branch" value) =
  conf { confShowPartLocalBranch = boolConfigToIntensity value }
configItemsFolder conf (Item "show_part_commits_to_origin" value) =
  conf { confShowPartCommitsToOrigin = boolConfigToIntensity value }
configItemsFolder conf (Item "show_part_local_changes_state" value) =
  conf { confShowPartLocalChangesState = boolConfigToIntensity value }
configItemsFolder conf (Item "show_part_stashes" value) =
  conf { confShowPartStashes = boolConfigToIntensity value }

configItemsFolder conf (Item "git_repo_indicator" repoIndicator) = conf { confRepoIndicator = repoIndicator }

configItemsFolder conf (Item "no_tracked_upstream_text" value) =
  conf { confNoTrackedUpstreamString = value }
configItemsFolder conf (Item "no_tracked_upstream_text_color" value) =
  conf { confNoTrackedUpstreamStringColor = colorConfigToColor value }
configItemsFolder conf (Item "no_tracked_upstream_text_intensity" value) =
  conf { confNoTrackedUpstreamStringIntensity = intensityConfigToIntensity value }
configItemsFolder conf (Item "no_tracked_upstream_indicator" value) =
  conf { confNoTrackedUpstreamIndicator = value }
configItemsFolder conf (Item "no_tracked_upstream_indicator_color" value) =
  conf { confNoTrackedUpstreamIndicatorColor = colorConfigToColor value }
configItemsFolder conf (Item "no_tracked_upstream_indicator_intensity" value) =
  conf { confNoTrackedUpstreamIndicatorIntensity = intensityConfigToIntensity value }

configItemsFolder conf (Item "merge_branch_commits_indicator" value) =
  conf { confMergeBranchCommitsIndicator = value }
configItemsFolder conf (Item "merge_branch_commits_pull_prefix" value) =
  conf { confMergeBranchCommitsOnlyPull = value }
configItemsFolder conf (Item "merge_branch_commits_push_prefix" value) =
  conf { confMergeBranchCommitsOnlyPush = value }
configItemsFolder conf (Item "merge_branch_commits_push_pull_infix" value) =
  conf { confMergeBranchCommitsBothPullPush = value }
configItemsFolder conf (Item "merge_branch_ignore_branches" value) =
  conf { confMergeBranchIgnoreBranches = stringConfigToStringList value }

configItemsFolder conf (Item "local_branch_prefix" value) =
  conf { confLocalBranchNamePrefix = value }
configItemsFolder conf (Item "local_branch_suffix" value) =
  conf { confLocalBranchNameSuffix = value }
configItemsFolder conf (Item "local_branch_color" value) =
  conf { confLocalBranchColor = colorConfigToColor value }
configItemsFolder conf (Item "local_branch_intensity" value) =
  conf { confLocalBranchIntensity = intensityConfigToIntensity value }
configItemsFolder conf (Item "local_detached_prefix" value) =
  conf { confLocalDetachedPrefix = value }
configItemsFolder conf (Item "local_detached_color" value) =
  conf { confLocalDetachedColor = colorConfigToColor value }
configItemsFolder conf (Item "local_detached_intensity" value) =
  conf { confLocalDetachedIntensity = intensityConfigToIntensity value }

configItemsFolder conf (Item "local_commits_push_suffix" value) =
  conf { confLocalCommitsPushSuffix = value }
configItemsFolder conf (Item "local_commits_push_suffix_color" value) =
  conf { confLocalCommitsPushSuffixColor = colorConfigToColor value }
configItemsFolder conf (Item "local_commits_push_suffix_intensity" value) =
  conf { confLocalCommitsPushSuffixIntensity = intensityConfigToIntensity value }
configItemsFolder conf (Item "local_commits_pull_suffix" value) =
  conf { confLocalCommitsPullSuffix = value }
configItemsFolder conf (Item "local_commits_pull_suffix_color" value) =
  conf { confLocalCommitsPullSuffixColor = colorConfigToColor value }
configItemsFolder conf (Item "local_commits_pull_suffix_intensity" value) =
  conf { confLocalCommitsPullSuffixIntensity = intensityConfigToIntensity value }
configItemsFolder conf (Item "local_commits_push_pull_infix" value) =
  conf { confLocalCommitsPushPullInfix = value }
configItemsFolder conf (Item "local_commits_push_pull_infix_color" value) =
  conf { confLocalCommitsPushPullInfixColor = colorConfigToColor value }
configItemsFolder conf (Item "local_commits_push_pull_infix_intensity" value) =
  conf { confLocalCommitsPushPullInfixIntensity = intensityConfigToIntensity value }

configItemsFolder conf (Item "change_index_add_suffix" value) =
  conf { confChangeIndexAddSuffix = value }
configItemsFolder conf (Item "change_index_add_suffix_color" value) =
  conf { confChangeIndexAddSuffixColor = colorConfigToColor value }
configItemsFolder conf (Item "change_index_add_suffix_intensity" value) =
  conf { confChangeIndexAddSuffixIntensity = intensityConfigToIntensity value }
configItemsFolder conf (Item "change_index_mod_suffix" value) =
  conf { confChangeIndexModSuffix = value }
configItemsFolder conf (Item "change_index_mod_suffix_color" value) =
  conf { confChangeIndexModSuffixColor = colorConfigToColor value }
configItemsFolder conf (Item "change_index_mod_suffix_intensity" value) =
  conf { confChangeIndexModSuffixIntensity = intensityConfigToIntensity value }
configItemsFolder conf (Item "change_index_del_suffix" value) =
  conf { confChangeIndexDelSuffix = value }
configItemsFolder conf (Item "change_index_del_suffix_color" value) =
  conf { confChangeIndexDelSuffixColor = colorConfigToColor value }
configItemsFolder conf (Item "change_index_del_suffix_intensity" value) =
  conf { confChangeIndexDelSuffixIntensity = intensityConfigToIntensity value }
configItemsFolder conf (Item "change_local_add_suffix" value) =
  conf { confChangeLocalAddSuffix = value }
configItemsFolder conf (Item "change_local_add_suffix_color" value) =
  conf { confChangeLocalAddSuffixColor = colorConfigToColor value }
configItemsFolder conf (Item "change_local_add_suffix_intensity" value) =
  conf { confChangeLocalAddSuffixIntensity = intensityConfigToIntensity value }
configItemsFolder conf (Item "change_local_mod_suffix" value) =
  conf { confChangeLocalModSuffix = value }
configItemsFolder conf (Item "change_local_mod_suffix_color" value) =
  conf { confChangeLocalModSuffixColor = colorConfigToColor value }
configItemsFolder conf (Item "change_local_mod_suffix_intensity" value) =
  conf { confChangeLocalModSuffixIntensity = intensityConfigToIntensity value }
configItemsFolder conf (Item "change_local_del_suffix" value) =
  conf { confChangeLocalDelSuffix = value }
configItemsFolder conf (Item "change_local_del_suffix_color" value) =
  conf { confChangeLocalDelSuffixColor = colorConfigToColor value }
configItemsFolder conf (Item "change_local_del_suffix_intensity" value) =
  conf { confChangeLocalDelSuffixIntensity = intensityConfigToIntensity value }
configItemsFolder conf (Item "change_renamed_suffix" value) =
  conf { confChangeRenamedSuffix = value }
configItemsFolder conf (Item "change_renamed_suffix_color" value) =
  conf { confChangeRenamedSuffixColor = colorConfigToColor value }
configItemsFolder conf (Item "change_renamed_suffix_intensity" value) =
  conf { confChangeRenamedSuffixIntensity = intensityConfigToIntensity value }
configItemsFolder conf (Item "change_conflicted_suffix" value) =
  conf { confChangeConflictedSuffix = value }
configItemsFolder conf (Item "change_conflicted_suffix_color" value) =
  conf { confChangeConflictedSuffixColor = colorConfigToColor value }
configItemsFolder conf (Item "change_conflicted_suffix_intensity" value) =
  conf { confChangeConflictedSuffixIntensity = intensityConfigToIntensity value }

configItemsFolder conf (Item "stash_suffix" value) =
  conf { confStashSuffix = value }
configItemsFolder conf (Item "stash_suffix_color" value) =
  conf { confStashSuffixColor = colorConfigToColor value }
configItemsFolder conf (Item "stash_suffix_intensity" value) =
  conf { confStashSuffixIntensity = intensityConfigToIntensity value }

configItemsFolder conf (Item "run_fetcher_daemon" value) =
  conf { confRunFetcherDaemon = boolConfigToBool value }
configItemsFolder conf (Item "githudd_sleep_seconds" value) =
  conf { confGithuddSleepSeconds = intConfigToInt value }
configItemsFolder conf (Item "githudd_pid_file_path" value) =
  conf { confGithuddPidFilePath = value }
configItemsFolder conf (Item "githudd_socket_file_path" value) =
  conf { confGithuddSocketFilePath = value }
configItemsFolder conf (Item "githudd_log_file_path" value) =
  conf { confGithuddLogFilePath = strConfigToRedirection value }

configItemsFolder conf _ = conf

colorConfigToColor :: String -> Color
colorConfigToColor str =
  either
    (const NoColor)
    id
    (parse colorParser "" str)

colorParser :: Parser Color
colorParser = choice [
    string "Black"   >> return Black
  , string "Red"     >> return Red
  , string "Green"   >> return Green
  , string "Yellow"  >> return Yellow
  , string "Blue"    >> return Blue
  , string "Magenta" >> return Magenta
  , string "Cyan"    >> return Cyan
  , string "White"   >> return White
  , string "NoColor" >> return NoColor
  ] <?> "color"

intensityConfigToIntensity :: String -> ColorIntensity
intensityConfigToIntensity str =
  either
    (const Vivid)
    id
    (parse intensityParser "" str)

intensityParser :: Parser ColorIntensity
intensityParser = choice [
    string "Dull" >> return Dull
  , string "Vivid" >> return Vivid
  ] <?> "intensity"

boolConfigToIntensity :: String -> Bool
boolConfigToIntensity str =
  either
    (const True)
    id
    (parse boolParser "" str)

stringConfigToStringList :: String -> [String]
stringConfigToStringList str =
  either
    (const [])
    id
    (parse stringListParser "" str)

stringListParser :: Parser [String]
stringListParser = do
  branchNameList <- sepBy stripedBranchName (char ',')
  return $ filter noEmptyStringFilter branchNameList

noEmptyStringFilter :: String -> Bool
noEmptyStringFilter = (/=) ""

stripedBranchName :: Parser String
stripedBranchName = do
  spaces
  branchName <- many (noneOf [',', ' '])
  spaces
  return branchName

intParser :: Parser Int
intParser = read <$> many1 digit

intConfigToInt :: String -> Int
intConfigToInt str =
  either
    (const 5)
    id
    (parse intParser "" str)

boolParser :: Parser Bool
boolParser = choice [
    string "False" >> return False
  , string "F" >> return False
  , string "false" >> return False
  , string "f" >> return False
  , string "No" >> return False
  , string "N" >> return False
  , string "no" >> return False
  , string "n" >> return False
  , string "True" >> return True
  , string "T" >> return True
  , string "true" >> return True
  , string "t" >> return True
  , string "Yes" >> return True
  , string "Y" >> return True
  , string "yes" >> return True
  , string "y" >> return True
  ] <?> "bool"

boolConfigToBool :: String -> Bool
boolConfigToBool str =
  either
    (const False)
    id
    (parse boolParser "" str)

strConfigToRedirection :: String -> Redirection
strConfigToRedirection str =
  either
    (const DevNull)
    id
    (parse redirectionParser "" str)

redirectionParser :: Parser Redirection
redirectionParser =
  choice [
    try (string "/dev/null") >> return DevNull
  , ToFile <$> many1 anyChar
  ] <?> "Redirection"
