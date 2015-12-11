module GitHUD.Git.Parse.Branch (
  buildFullyQualifiedRemoteBranchName
  ) where

import Text.Parsec (parse)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (anyChar, string)
import Text.Parsec.Prim (many)

buildFullyQualifiedRemoteBranchName :: String       -- ^ remote
                                    -> String       -- ^ remote Branch Name
                                    -> String
buildFullyQualifiedRemoteBranchName remote branch =
  remote ++ "/" ++ (simpleRemoteBranchName branch)

simpleRemoteBranchName :: String
                       -> String
simpleRemoteBranchName branch =
  either
    (const "")
    id
    (parse remoteBranchParser "" branch)

remoteBranchParser :: Parser String
remoteBranchParser = do
  string "refs/heads/"
  many anyChar


