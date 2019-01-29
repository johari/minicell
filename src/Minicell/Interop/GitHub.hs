{-# LANGUAGE OverloadedStrings #-}

module Minicell.Interop.GitHub where

import Prelude.Compat

import Data.Foldable
import Data.Text         (Text, pack, unpack)
import Data.Text.IO as T (putStrLn)
import Data.Monoid       ((<>))

import qualified GitHub.Endpoints.Users.Followers as GitHub
import qualified GitHub.Endpoints.Issues as Issues
import GitHub.Data.Id
import GitHub.Data.Options
import GitHub.Auth

import Data.Graph.Inductive.NodeMap
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree (Gr)

import Data.Maybe
import Data.String

import Data.Binary
import System.Directory


auth = (BasicAuth "johari" "PASS")

octoGraph repo = do
    -- possibleIssues <- Issues.issue' (Just (BasicAuth "johari" "PASS")) "johari" "minicell" (Id 20)

    -- cacheKey  = ["interop", "github:issues", "johari", (fromString repo)]

    let cacheFile = "/tmp/issues.txt"
    fileExist <- doesFileExist cacheFile

    possibleIssues <-
            case fileExist of
                True -> do
                    ret <- decodeFile cacheFile
                    return ret
                False -> do
                    res <- Issues.issuesForRepo' (Just auth) "johari" (fromString repo) mempty
                    print "hi"
                    print res
                    let Right ret = res
                    encodeFile cacheFile ret
                    return ret

    return $ octoGraphFromIssues (toList possibleIssues)
    -- print [(Issues.issueTitle x, Issues.issueState x, Issues.issueNumber x) | x <- toList possibleIssues]


octoGraphFromIssues :: [Issues.Issue] -> Gr String Int
octoGraphFromIssues issues = (mkGraph vertices edges)
  where
    (vertices, nm) = mkNodes new ([ unpack $ Issues.issueTitle x | x <- issues ] ++ [ show $ Issues.issueNumber x | x <- issues ])
    edges = fromMaybe [] $ mkEdges nm [ (unpack $ Issues.issueTitle x, show $ Issues.issueNumber x, 0) | x <- issues]

main2 = do
    possibleUsers <- GitHub.usersFollowing "agobal"
    T.putStrLn $ either (("Error: " <>) . pack . show)
                        (foldMap ((<> "\n") . formatUser))
                        possibleUsers


formatUser :: GitHub.SimpleUser -> Text
formatUser = GitHub.untagName . GitHub.simpleUserLogin