{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Helpers.Fixtures
  ( generateRandomName
  , withProject
  , withUser
  , withGroup
  , withIssue
  ) where

import qualified Data.Text as T
import qualified GitLab
import System.Random (randomRIO)
import Control.Exception (bracket)
import Control.Monad (replicateM)
import Test.Helpers.Assertions (expectRight, expectJust)

generateRandomName :: String -> IO T.Text
generateRandomName prefix = do
  suffix <- T.pack <$> replicateM 5 randomAlphaNum
  return $ T.pack prefix <> "-" <> suffix
  where
    chars = T.pack (['a'..'z'] ++ ['0'..'9'])
    randomAlphaNum = do
      idx <- randomRIO (0, T.length chars - 1)
      return (T.index chars idx)

-- | Helper to create a project, run an action with it, and clean up.
-- Uses bracket to ensure cleanup happens even if the action fails.
withProject :: GitLab.GitLabServerConfig -> (GitLab.Project -> IO a) -> IO a
withProject cfg action = do
  projectName <- generateRandomName "test-project"
  projectPath <- generateRandomName "test-project"

  bracket
    (createProject projectName projectPath)
    deleteProject
    action
  where
    createProject name projectPath = do
      createResult <- GitLab.runGitLab cfg $ GitLab.createProject name projectPath
      parsedOrHttpError <- expectRight ("Failed to create project '" ++ T.unpack name ++ "'") createResult
      projectOrNotFound <- expectRight ("Failed to create project '" ++ T.unpack name ++ "' (HTTP error)") parsedOrHttpError
      expectJust ("Failed to create project '" ++ T.unpack name ++ "' (not found)") projectOrNotFound

    deleteProject project = do
      result <- GitLab.runGitLab cfg $ GitLab.deleteProject project
      parsedOrHttpError <- expectRight ("Failed to delete project " ++ show (GitLab.project_id project)) result
      (_ :: Maybe ()) <- expectRight ("Failed to delete project " ++ show (GitLab.project_id project) ++ " (HTTP error)") parsedOrHttpError
      -- Note: Both Just () and Nothing indicate success (2xx status code)
      -- Just () means the response body parsed as (), Nothing means parse failed (e.g., empty body)
      return ()

-- | Helper to create a user, run an action with it, and clean up.
-- Uses bracket to ensure cleanup happens even if the action fails.
withUser :: GitLab.GitLabServerConfig -> (GitLab.User -> IO a) -> IO a
withUser cfg action = do
  username <- generateRandomName "testuser"
  email <- (\n -> n <> "@example.com") <$> generateRandomName "test"

  bracket
    (createUser username email)
    deleteUser
    action
  where
    createUser username email = do
      let attrs = GitLab.defaultUserFilters
            { GitLab.userFilter_password = Just "xK9#mP2$vL8@qW5!"
            , GitLab.userFilter_skip_confirmation = Just True
            }
      createResult <- GitLab.runGitLab cfg $ GitLab.createUser email "Test User" username attrs
      parsedOrHttpError <- expectRight ("Failed to create user '" ++ T.unpack username ++ "'") createResult
      userOrNotFound <- expectRight ("Failed to create user '" ++ T.unpack username ++ "' (HTTP error)") parsedOrHttpError
      expectJust ("Failed to create user '" ++ T.unpack username ++ "' (not found)") userOrNotFound

    deleteUser user = do
      result <- GitLab.runGitLab cfg $ GitLab.deleteUser user
      parsedOrHttpError <- expectRight ("Failed to delete user " ++ show (GitLab.user_id user)) result
      (_ :: Maybe ()) <- expectRight ("Failed to delete user " ++ show (GitLab.user_id user) ++ " (HTTP error)") parsedOrHttpError
      -- Note: Both Just () and Nothing indicate success (2xx status code)
      -- Just () means the response body parsed as (), Nothing means parse failed (e.g., empty body)
      return ()

-- | Helper to create a group, run an action with it, and clean up.
-- Uses bracket to ensure cleanup happens even if the action fails.
withGroup :: GitLab.GitLabServerConfig -> (GitLab.Group -> IO a) -> IO a
withGroup cfg action = do
  groupName <- generateRandomName "test-group"
  groupPath <- generateRandomName "test-group"

  bracket
    (createGroup groupName groupPath)
    deleteGroup
    action
  where
    createGroup name path = do
      let attrs = GitLab.defaultGroupFilters
      createResult <- GitLab.runGitLab cfg $ GitLab.newGroup name path attrs
      parsedOrHttpError <- expectRight ("Failed to create group '" ++ T.unpack name ++ "'") createResult
      groupOrNotFound <- expectRight ("Failed to create group '" ++ T.unpack name ++ "' (HTTP error)") parsedOrHttpError
      expectJust ("Failed to create group '" ++ T.unpack name ++ "' (not found)") groupOrNotFound

    deleteGroup group = do
      result <- GitLab.runGitLab cfg $ GitLab.removeGroup (GitLab.group_id group)
      parsedOrHttpError <- expectRight ("Failed to delete group " ++ show (GitLab.group_id group)) result
      (_ :: Maybe ()) <- expectRight ("Failed to delete group " ++ show (GitLab.group_id group) ++ " (HTTP error)") parsedOrHttpError
      -- Note: Both Just () and Nothing indicate success (2xx status code)
      -- Just () means the response body parsed as (), Nothing means parse failed (e.g., empty body)
      return ()

-- | Helper to create an issue within a project, run an action with it, and clean up.
-- Uses bracket to ensure cleanup happens even if the action fails.
withIssue :: GitLab.GitLabServerConfig -> GitLab.Project -> (GitLab.Issue -> IO a) -> IO a
withIssue cfg project action = do
  issueTitle <- generateRandomName "test-issue"

  bracket
    (createIssue issueTitle)
    deleteIssue
    action
  where
    createIssue title = do
      let attrs = GitLab.defaultIssueAttrs (GitLab.project_id project)
      createResult <- GitLab.runGitLab cfg $
        GitLab.newIssue project title "Test issue description" attrs
      parsedOrHttpError <- expectRight ("Failed to create issue '" ++ T.unpack title ++ "'") createResult
      issueOrNotFound <- expectRight ("Failed to create issue '" ++ T.unpack title ++ "' (HTTP error)") parsedOrHttpError
      expectJust ("Failed to create issue '" ++ T.unpack title ++ "' (not found)") issueOrNotFound

    deleteIssue issue = do
      -- Use iid (project-internal ID) for project-scoped operations
      result <- GitLab.runGitLab cfg $ GitLab.deleteIssue project (GitLab.issue_iid issue)
      parsedOrHttpError <- expectRight ("Failed to delete issue " ++ show (GitLab.issue_iid issue)) result
      (_ :: Maybe ()) <- expectRight ("Failed to delete issue " ++ show (GitLab.issue_iid issue) ++ " (HTTP error)") parsedOrHttpError
      -- Note: Both Just () and Nothing indicate success (2xx status code)
      -- Just () means the response body parsed as (), Nothing means parse failed (e.g., empty body)
      return ()
