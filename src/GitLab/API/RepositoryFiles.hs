{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : RepositoryFiles
-- Description : Queries about project repository files
-- Copyright   : (c) Rob Stewart, Heriot-Watt University, 2019
-- License     : BSD3
-- Maintainer  : robstewart57@gmail.com
-- Stability   : stable
module GitLab.API.RepositoryFiles where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GitLab.Types
import GitLab.WebRequests.GitLabWebCalls
import Network.HTTP.Client
import Network.HTTP.Types.Status
import Network.HTTP.Types.URI

-- | Get a list of repository files and directories in a project.
repositoryFiles ::
  -- | the project
  Project ->
  -- | the file path
  Text ->
  -- | name of the branch, tag or commit
  Text ->
  GitLab (Either (Response BSL.ByteString) (Maybe RepositoryFile))
repositoryFiles project = repositoryFiles' (project_id project)

-- | Get a list of repository files and directories in a project given
-- the project's ID.
repositoryFiles' ::
  -- | project ID
  Int ->
  -- | the file path
  Text ->
  -- | name of the branch, tag or commit
  Text ->
  GitLab (Either (Response BSL.ByteString) (Maybe RepositoryFile))
repositoryFiles' projectId filePath reference =
  gitlabGetOne addr [("ref", Just (T.encodeUtf8 reference))]
  where
    addr =
      "/projects/"
        <> T.pack (show projectId)
        <> "/repository"
        <> "/files"
        <> "/"
        <> T.decodeUtf8 (urlEncode False (T.encodeUtf8 filePath))

-- | Get raw data for a given file blob hash.
repositoryFileBlob ::
  -- | project ID
  Int ->
  -- | blob SHA
  Text ->
  GitLab (Either (Response BSL.ByteString) String)
repositoryFileBlob projectId blobSha = do
  resp <- gitlabGetByteStringResponse addr []
  if successStatus (responseStatus resp)
    then return (Right (BSL8.unpack (responseBody resp)))
    else return (Left resp)
  where
    addr =
      "/projects/"
        <> T.pack (show projectId)
        <> "/repository"
        <> "/blobs/"
        <> blobSha
        <> "/raw"
    successStatus :: Status -> Bool
    successStatus (Status n _msg) =
      n >= 200 && n <= 226
