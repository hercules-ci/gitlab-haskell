{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Members
-- Description : Queries about and updates to members of projects
-- Copyright   : (c) Rob Stewart, Heriot-Watt University, 2021
-- License     : BSD3
-- Maintainer  : robstewart57@gmail.com
-- Stability   : stable
module GitLab.API.Members
  ( AccessLevel (..),

    -- * Projects

    -- * Project membership
    membersOfProject,
    membersOfProject',

    -- ** Adding project members
    addMemberToProject,
    addMemberToProject',
    addMembersToProject,
    addMembersToProject',

    -- ** Removing project members
    removeUserFromProject,
    removeUserFromProject',

    -- * Groups

    -- * Group membership
    membersOfGroup,
    membersOfGroup',

    -- ** Adding group members
    addAllUsersToGroup,
    addUserToGroup,
    addUserToGroup',
    addUsersToGroup,
    addUsersToGroup',

    -- ** Removing group members
    removeUserFromGroup,
    removeUserFromGroup',
  )
where

import qualified Data.ByteString.Lazy as BSL
import Data.Either
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GitLab.API.Groups
import GitLab.API.Users
import GitLab.Types
import GitLab.WebRequests.GitLabWebCalls
import Network.HTTP.Client
import Network.HTTP.Types.URI

-- | the members of a project.
membersOfProject :: Project -> GitLab [Member]
membersOfProject p = do
  result <- membersOfProject' (project_id p)
  return (fromRight (error "membersOfProject error") result)

-- | the members of a project given its ID.
membersOfProject' :: Int -> GitLab (Either (Response BSL.ByteString) [Member])
membersOfProject' projectId =
  membersOfEntity' projectId "projects"

-- | adds a user to a project with the given access level. Returns
-- 'Right Member' for each successful action, otherwise it returns
-- 'Left Status'.
addMemberToProject ::
  -- | the project
  Project ->
  -- | level of access
  AccessLevel ->
  -- | the user
  User ->
  GitLab (Either (Response BSL.ByteString) (Maybe Member))
addMemberToProject project access usr =
  addMemberToProject' (project_id project) access (user_id usr)

-- | adds a user to a project with the given access level, given the
-- project's ID and the user's ID. Returns @Right Member@ for each
-- successful action, otherwise it returns @Left Status@.
addMemberToProject' ::
  -- | project ID
  Int ->
  -- | level of access
  AccessLevel ->
  -- | user ID
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe Member))
addMemberToProject' projectId access usrId =
  gitlabPost addr params
  where
    params :: [GitLabParam]
    params =
      [ ("user_id", Just (T.encodeUtf8 (T.pack (show usrId)))),
        ("access_level", Just (T.encodeUtf8 (T.pack (show access))))
      ]
    addr =
      "/projects/" <> T.pack (show projectId) <> "/members"

-- | adds a list of users to a project with the given access
-- level. Returns 'Right Member' for each successful action, otherwise
-- it returns 'Left Status'.
addMembersToProject ::
  -- | the project
  Project ->
  -- | level of access
  AccessLevel ->
  -- | users to add to the project
  [User] ->
  GitLab [Either (Response BSL.ByteString) (Maybe Member)]
addMembersToProject project access =
  mapM (addMemberToProject project access)

-- | adds a list of users to a project with the given access level,
-- given the project's ID and the user IDs. Returns @Right Member@ for
-- each successful action, otherwise it returns @Left Status@.
addMembersToProject' ::
  -- | project ID
  Int ->
  -- | level of acces
  AccessLevel ->
  -- | IDs of users to add to the project
  [Int] ->
  GitLab [Either (Response BSL.ByteString) (Maybe Member)]
addMembersToProject' projectId access =
  mapM (addMemberToProject' projectId access)

-- | the members of a group.
membersOfGroup :: Group -> GitLab [Member]
membersOfGroup p = do
  result <- membersOfGroup' (group_id p)
  return (fromRight (error "membersOfGroup error") result)

-- | the members of a group given its ID.
membersOfGroup' :: Int -> GitLab (Either (Response BSL.ByteString) [Member])
membersOfGroup' projectId =
  membersOfEntity' projectId "groups"

-- | adds all registered users to a group.
addAllUsersToGroup ::
  -- | group name
  Text ->
  -- | level of access granted
  AccessLevel ->
  GitLab [Either (Response BSL.ByteString) (Maybe Member)]
addAllUsersToGroup groupName access = do
  allRegisteredUsers <- allUsers
  let allUserIds = map user_username allRegisteredUsers
  addUsersToGroup' groupName access allUserIds

-- | adds a user to a group.
addUserToGroup ::
  -- | group name
  Text ->
  -- | level of access granted
  AccessLevel ->
  -- | the user
  User ->
  GitLab (Either (Response BSL.ByteString) (Maybe Member))
addUserToGroup groupName access usr =
  addUserToGroup' groupName access (user_id usr)

-- | adds a user with a given user ID to a group.
addUserToGroup' ::
  -- | group name
  Text ->
  -- | level of access granted
  AccessLevel ->
  -- | user ID
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe Member))
addUserToGroup' groupName access usrId = do
  attempt <- groupsWithNameOrPath groupName
  case attempt of
    Left resp -> return (Left resp)
    Right [] ->
      return (Right Nothing)
    Right [grp] ->
      gitlabPost addr params
      where
        params :: [GitLabParam]
        params =
          [ ("user_id", Just (T.encodeUtf8 (T.pack (show usrId)))),
            ("access_level", Just (T.encodeUtf8 (T.pack (show access))))
          ]
        addr =
          "/groups/"
            <> T.decodeUtf8 (urlEncode False (T.encodeUtf8 (T.pack (show (group_id grp)))))
            <> "/members"
    Right (_ : _) ->
      return (Right Nothing)

-- | adds a list of users to a group.
addUsersToGroup ::
  -- | group name
  Text ->
  -- | level of access granted
  AccessLevel ->
  -- | list of usernames to be added to the group
  [User] ->
  GitLab [Either (Response BSL.ByteString) (Maybe Member)]
addUsersToGroup groupName access =
  mapM (addUserToGroup groupName access)

-- | adds a list of users to a group.
addUsersToGroup' ::
  -- | group name
  Text ->
  -- | level of access granted
  AccessLevel ->
  -- | list of usernames to be added to the group
  [Text] ->
  GitLab [Either (Response BSL.ByteString) (Maybe Member)]
addUsersToGroup' groupName access usernames = do
  users <- catMaybes <$> mapM searchUser usernames
  mapM (addUserToGroup' groupName access . user_id) users

-- | Removes a user from a project where the user has been explicitly assigned a role
removeUserFromProject ::
  -- | project name
  Text ->
  -- | user
  User ->
  GitLab (Either (Response BSL.ByteString) (Maybe ()))
removeUserFromProject grpName usr =
  removeUserFromEntity grpName "projects" usr

-- | Removes a user from a project where the user has been explicitly assigned a role
removeUserFromProject' ::
  -- | project name
  Text ->
  -- | user ID
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe ()))
removeUserFromProject' grpName usrId =
  removeUserFromEntity' grpName "projects" usrId

-- | Removes a user from a group where the user has been explicitly assigned a role
removeUserFromGroup ::
  -- | group name
  Text ->
  -- | user
  User ->
  GitLab (Either (Response BSL.ByteString) (Maybe ()))
removeUserFromGroup grpName usr =
  removeUserFromEntity grpName "groups" usr

-- | Removes a user from a group where the user has been explicitly assigned a role
removeUserFromGroup' ::
  -- | group name
  Text ->
  -- | user ID
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe ()))
removeUserFromGroup' grpName usrId =
  removeUserFromEntity' grpName "groups" usrId

-----------------------
-- Internal functions.

-- | removes a user from a group or project.
removeUserFromEntity ::
  -- | group name
  Text ->
  -- | entity ("groups" or "projects)
  Text ->
  -- | user
  User ->
  GitLab (Either (Response BSL.ByteString) (Maybe ()))
removeUserFromEntity groupName entity usr =
  removeUserFromEntity' groupName entity (user_id usr)

-- | removes a user with a given user ID from a group or project.
removeUserFromEntity' ::
  -- | group name
  Text ->
  -- | entity ("groups" or "projects")
  Text ->
  -- | user ID
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe ()))
removeUserFromEntity' groupName entity usrId = do
  attempt <- groupsWithNameOrPath groupName
  case attempt of
    Left resp -> return (Left resp)
    Right [] ->
      return (Right Nothing)
    Right [grp] -> do
      result <- gitlabDelete addr
      case result of
        Left err -> return (Left err)
        -- GitLab version 14.2.3 returns Version JSON info when a
        -- member is removed from a group/project. I'm not sure if
        -- this is new behaviour, anyway we catch it here.
        Right (Just (Version {})) -> return (Right (Just ()))
        Right Nothing -> return (Right (Just ()))
      where
        addr =
          "/"
            <> entity
            <> "/"
            <> T.decodeUtf8 (urlEncode False (T.encodeUtf8 (T.pack (show (group_id grp)))))
            <> "/members/"
            <> T.decodeUtf8 (urlEncode False (T.encodeUtf8 (T.pack (show usrId))))
    Right (_ : _) ->
      return (Right Nothing)

-- | the members of a project given its ID.
membersOfEntity' ::
  -- | group or project ID
  Int ->
  -- | entity ("groups" or "projects")
  Text ->
  GitLab (Either (Response BSL.ByteString) [Member])
membersOfEntity' projectId entity =
  gitlabGetMany addr []
  where
    addr =
      "/"
        <> entity
        <> "/"
        <> T.pack (show projectId)
        <> "/members"
