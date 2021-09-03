{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Groups
-- Description : Queries about and updates to groups
-- Copyright   : (c) Rob Stewart, Heriot-Watt University, 2019
-- License     : BSD3
-- Maintainer  : robstewart57@gmail.com
-- Stability   : stable
module GitLab.API.Groups where

import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GitLab.Types
import GitLab.WebRequests.GitLabWebCalls
import Network.HTTP.Client

-- | gets groups with the given group name, path or full path.
--
-- > projectsWithNameOrPath "group1"
groupsWithNameOrPath ::
  -- | group name being searched for.
  Text ->
  GitLab (Either (Response BSL.ByteString) [Group])
groupsWithNameOrPath groupName = do
  result <- gitlabGetMany "/groups" [("search", Just (T.encodeUtf8 groupName))]
  case result of
    Left {} -> return result
    Right groups ->
      return
        ( Right
            $filter
            ( \group ->
                groupName == group_name group
                  || groupName == group_path group
                  || groupName == group_full_path group
            )
            groups
        )

groupProjects ::
  -- | group
  Group ->
  GitLab (Either (Response BSL.ByteString) [Project])
groupProjects group = do
  groupProjects' (group_id group)

groupProjects' ::
  -- | group ID
  Int ->
  GitLab (Either (Response BSL.ByteString) [Project])
groupProjects' groupID = do
  let urlPath =
        T.pack $
          "/groups/"
            <> show groupID
            <> "/projects"
  gitlabGetMany urlPath []
