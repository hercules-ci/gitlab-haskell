{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Common where

import Control.Exception
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.TreeDiff.Class
import Data.TreeDiff.Pretty
import GitLab
import GitLab.SystemHooks.GitLabSystemHooks
import GitLab.SystemHooks.Types
import Test.Tasty
import Test.Tasty.HUnit
import qualified Text.PrettyPrint.ANSI.Leijen as WL

gitlabParseTestOne :: (ToExpr a, FromJSON a, Eq a, Show a) => a -> String -> Assertion
gitlabParseTestOne expectedHaskellValue filename = do
  raw <- BSL.readFile filename
  result <- parseOne raw
  (expectedHaskellValue == result)
    @? showWL (ansiWlEditExprCompact (ediff expectedHaskellValue result))

showWL :: WL.Doc -> String
showWL doc = WL.displayS (WL.renderSmart 0.4 80 doc) ""

gitlabParseTestMany :: (ToExpr a, FromJSON a, Eq a, Show a) => [a] -> String -> Assertion
gitlabParseTestMany expectedHaskellValue filename = do
  raw <- BSL.readFile filename
  result <- parseMany raw
  (expectedHaskellValue == result)
    @? showWL (ansiWlEditExprCompact (ediff expectedHaskellValue result))

parseOne :: FromJSON a => BSL.ByteString -> IO a
parseOne bs =
  case eitherDecode bs of
    Left err -> assertFailure err
    Right xs -> return xs

parseMany :: FromJSON a => BSL.ByteString -> IO [a]
parseMany bs =
  case eitherDecode bs of
    Left err -> assertFailure err
    Right xs -> return xs

-------------
-- ToExpr instances

instance ToExpr ArchiveFormat

instance ToExpr Member

instance ToExpr Namespace

instance ToExpr Links

instance ToExpr Owner

instance ToExpr Permissions

instance ToExpr Project

instance ToExpr ProjectStats

instance ToExpr User

instance ToExpr Milestone

instance ToExpr MilestoneState

instance ToExpr TimeStats

instance ToExpr Issue

instance ToExpr Pipeline

instance ToExpr Commit

instance ToExpr CommitTodo

instance ToExpr CommitStats

instance ToExpr Tag

instance ToExpr Release

instance ToExpr Diff

instance ToExpr Repository

instance ToExpr Job

instance ToExpr Artifact

instance ToExpr Group

instance ToExpr GroupShare

instance ToExpr Branch

instance ToExpr RepositoryFile

instance ToExpr MergeRequest

instance ToExpr Todo

instance ToExpr TodoProject

instance ToExpr TodoAction

instance ToExpr TodoTarget

instance ToExpr TodoState

instance ToExpr Version

instance ToExpr EditIssueReq

instance ToExpr Discussion

instance ToExpr Note

instance ToExpr IssueStatistics

instance ToExpr IssueStats

instance ToExpr IssueCounts

instance ToExpr IssueBoard

instance ToExpr BoardIssue

instance ToExpr BoardIssueLabel

instance ToExpr ProjectBoard

instance ToExpr Visibility

instance ToExpr TestReport

instance ToExpr TestSuite

instance ToExpr TestCase

instance ToExpr TimeEstimate
