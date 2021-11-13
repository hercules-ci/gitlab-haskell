-- |
-- Module      : GitLab.SystemHooks.Rules
-- Description : Common GitLab system hook rules
-- Copyright   : (c) Rob Stewart, Heriot-Watt University, 2020
-- License     : BSD3
-- Maintainer  : robstewart57@gmail.com
-- Stability   : stable
module GitLab.SystemHooks.Rules (ruleAddMembers, ruleAddNewUserToGroups) where

import Control.Monad
import Data.Text
import GitLab.API.Members
import GitLab.API.Projects
import GitLab.API.Users
import GitLab.SystemHooks.Types
import GitLab.Types

-- | When a new user creates an account on a GitLab server they are
-- added to all groups in the specified group list
ruleAddNewUserToGroups ::
  -- | rule label
  String ->
  -- | list of (non registered) usernames
  [Text] ->
  -- | list of groups to add new user to
  [Text] ->
  Rule
ruleAddNewUserToGroups lbl nonRegisteredUsernames groupNames =
  matchIf
    lbl
    ( \event@UserCreate {} -> do
        return (userCreate_username event `elem` nonRegisteredUsernames)
    )
    ( \event@UserCreate {} -> do
        mapM_
          ( \groupName ->
              -- will return value of type `Left Status` if user already
              -- member of the group, `void` silently ignores any outcome.
              addUserToGroup' groupName Reporter (userCreate_user_id event)
          )
          groupNames
    )

-- | Add specified users with a 'Reporter' role to all specified
-- projects with a given project name
ruleAddMembers ::
  -- | rule label
  String ->
  -- | project names to match on
  [Text] ->
  -- | user names to add as member of matched project
  [Text] ->
  Rule
ruleAddMembers label projectNames userNames =
  matchIf
    label
    ( \event@ProjectCreate {} -> do
        request <- searchProjectId (projectCreate_project_id event)
        case request of
          Left _ -> return False
          Right Nothing -> return False
          Right (Just prj) ->
            return
              ( project_path prj
                  `elem` projectNames
              )
    )
    ( \event@ProjectCreate {} -> do
        mapM_
          ( \userName -> do
              request <- searchUser userName
              case request of
                Nothing -> return ()
                Just foundUser ->
                  void $
                    addMemberToProject'
                      (projectCreate_project_id event)
                      Reporter
                      (user_id foundUser)
          )
          userNames
    )
