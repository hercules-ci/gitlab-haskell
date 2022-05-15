{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Discussions
-- Description : Queries about discussions, which are a set of related notes on snippets, issues, epics, merge requests and commits.
-- Copyright   : (c) Rob Stewart, Heriot-Watt University, 2021
-- License     : BSD3
-- Maintainer  : robstewart57@gmail.com
-- Stability   : stable
module GitLab.API.Discussions where

import qualified Data.ByteString.Lazy as BSL
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GitLab.Types
import GitLab.WebRequests.GitLabWebCalls
import Network.HTTP.Client

-- | Gets a list of all discussion items for a single issue.
projectIssueDiscussions' ::
  -- | the project ID
  Int ->
  -- | The IID of an issue
  Int ->
  GitLab (Either (Response BSL.ByteString) [Discussion])
projectIssueDiscussions' projId issueIid = do
  let urlPath =
        T.pack $
          "/projects/"
            <> show projId
            <> "/issues/"
            <> show issueIid
            <> "/discussions"
  gitlabGetMany urlPath []

-- | Returns a single discussion item for a specific project issue.
issueDiscussion' ::
  -- | the project ID
  Int ->
  -- | The IID of an issue
  Int ->
  -- | The ID of a discussion item
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe Discussion))
issueDiscussion' projId issueIid discussionId = do
  let urlPath =
        T.pack $
          "/projects/"
            <> show projId
            <> "/issues/"
            <> show issueIid
            <> "/discussions/"
            <> show discussionId
  gitlabGetOne urlPath []

-- | Creates a new thread to a single project issue. This is similar
-- to creating a note but other comments (replies) can be added to it
-- later.
createIssueThread' ::
  -- | the project ID
  Int ->
  -- | The IID of an issue
  Int ->
  -- | The content of the thread
  Text ->
  GitLab (Either (Response BSL.ByteString) (Maybe Discussion))
createIssueThread' projectId issueIid threadContent = do
  gitlabPost discussionAddr [("body", Just (T.encodeUtf8 threadContent))]
  where
    discussionAddr :: Text
    discussionAddr =
      "/projects/"
        <> T.pack (show projectId)
        <> "/issues/"
        <> T.pack (show issueIid)
        <> "/discussions"

-- | Adds a new note to the thread. This can also create a thread from
-- a single comment. Notes can be added to other items than comments,
-- such as system notes, making them threads.
addNoteToIssueThread' ::
  -- | the project ID
  Int ->
  -- | The IID of an issue
  Int ->
  -- | The ID of a thread
  Int ->
  -- -- | The ID of a thread note
  -- Int ->

  -- | The content of the note/reply
  Text ->
  GitLab (Either (Response BSL.ByteString) (Maybe Discussion))
addNoteToIssueThread' projectId issueIid discussionId content = do
  gitlabPost discussionAddr [("body", Just (T.encodeUtf8 content))]
  where
    discussionAddr :: Text
    discussionAddr =
      "/projects/"
        <> T.pack (show projectId)
        <> "/issues/"
        <> T.pack (show issueIid)
        <> "/discussions/"
        <> T.pack (show discussionId)
        <> "/notes"

-- <> "/notes/"
-- <> T.pack (show noteId)

-- | Modify existing thread note of an issue.
modifyThreadNoteIssue' ::
  -- | the project ID
  Int ->
  -- | The IID of an issue
  Int ->
  -- | The ID of a thread
  Int ->
  -- | The ID of a thread note
  Int ->
  -- | The content of the note/reply
  Text ->
  GitLab (Either (Response BSL.ByteString) (Maybe Discussion))
modifyThreadNoteIssue' projectId issueIid discussionId noteId content = do
  gitlabPut noteAddr [("body", Just (T.encodeUtf8 content))]
  where
    noteAddr :: Text
    noteAddr =
      "/projects/"
        <> T.pack (show projectId)
        <> "/issues/"
        <> T.pack (show issueIid)
        <> "/discussions/"
        <> T.pack (show discussionId)
        <> "/notes/"
        <> T.pack (show noteId)

-- | Deletes an existing thread note of an issue.
deleteIssueThreadNote' ::
  -- | the project ID
  Int ->
  -- | The IID of an issue
  Int ->
  -- | The ID of a discussion
  Int ->
  -- | The ID of a discussion note
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe ()))
deleteIssueThreadNote' projectId issueIid discussionId noteId = do
  gitlabDelete noteAddr []
  where
    noteAddr :: Text
    noteAddr =
      "/projects/"
        <> T.pack (show projectId)
        <> "/issues/"
        <> T.pack (show issueIid)
        <> "/discussions/"
        <> T.pack (show discussionId)
        <> "/notes/"
        <> T.pack (show noteId)

-- | Gets a list of all discussion items for a single snippet.
snippetDiscussionItems' ::
  -- | project ID
  Int ->
  -- | snippet ID
  Int ->
  GitLab (Either (Response BSL.ByteString) [Discussion])
snippetDiscussionItems' projectId snippetId =
  gitlabGetMany urlPath []
  where
    urlPath =
      T.pack $
        "/projects/"
          <> show projectId
          <> "/snippets/"
          <> show snippetId
          <> "/discussions"

-- | Returns a single discussion item for a specific project snippet.
snippetDiscussionItem' ::
  -- | project ID
  Int ->
  -- | snippet ID
  Int ->
  -- | discussion ID
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe Discussion))
snippetDiscussionItem' projectId snippetId discussionId =
  gitlabGetOne urlPath []
  where
    urlPath =
      T.pack $
        "/projects/"
          <> show projectId
          <> "/snippets/"
          <> show snippetId
          <> "/discussions/"
          <> show discussionId

-- | Creates a new thread to a single project snippet. This is similar
-- to creating a note but other comments (replies) can be added to it
-- later.
createSnippetThread' ::
  -- | the project ID
  Int ->
  -- | snippet ID
  Int ->
  -- | The content of a discussion
  Text ->
  GitLab (Either (Response BSL.ByteString) (Maybe Discussion))
createSnippetThread' projectId snippetId content = do
  gitlabPost discussionAddr [("body", Just (T.encodeUtf8 content))]
  where
    discussionAddr :: Text
    discussionAddr =
      "/projects/"
        <> T.pack (show projectId)
        <> "/snippets/"
        <> T.pack (show snippetId)
        <> "/discussions"

-- | Adds a new note to the thread.
addNoteToSnippetThread' ::
  -- | project ID
  Int ->
  -- | snippet ID
  Int ->
  -- | discussion ID
  Int ->
  -- -- | note ID
  -- Int ->

  -- | The content of the note/reply
  Text ->
  GitLab (Either (Response BSL.ByteString) (Maybe Discussion))
addNoteToSnippetThread' projectId snippetId discussionId content =
  gitlabPost discussionAddr [("body", Just (T.encodeUtf8 content))]
  where
    discussionAddr :: Text
    discussionAddr =
      "/projects/"
        <> T.pack (show projectId)
        <> "/snippets/"
        <> T.pack (show snippetId)
        <> "/discussions/"
        <> T.pack (show discussionId)
        <> "/notes"

-- <> "/notes/"
-- <> T.pack (show noteId)

-- | Modify existing thread note of a snippet.
modifySnippetThreadNote' ::
  -- | project ID
  Int ->
  -- | snippet ID
  Int ->
  -- | discussion ID
  Int ->
  -- | note ID
  Int ->
  -- | The content of the note/reply
  Text ->
  GitLab (Either (Response BSL.ByteString) (Maybe Discussion))
modifySnippetThreadNote' projectId snippetId discussionId noteId content =
  gitlabPut noteAddr [("body", Just (T.encodeUtf8 content))]
  where
    noteAddr :: Text
    noteAddr =
      "/projects/"
        <> T.pack (show projectId)
        <> "/snippets/"
        <> T.pack (show snippetId)
        <> "/discussions/"
        <> T.pack (show discussionId)
        <> "/notes/"
        <> T.pack (show noteId)

-- | Deletes an existing thread note of an issue.
deleteSnippetThreadNote' ::
  -- | the project ID
  Int ->
  -- | snippet ID
  Int ->
  -- | discussion ID
  Int ->
  -- | note ID
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe ()))
deleteSnippetThreadNote' projectId snippetId discussionId noteId = do
  gitlabDelete noteAddr []
  where
    noteAddr :: Text
    noteAddr =
      "/projects/"
        <> T.pack (show projectId)
        <> "/snippets/"
        <> T.pack (show snippetId)
        <> "/discussions/"
        <> T.pack (show discussionId)
        <> "/notes/"
        <> T.pack (show noteId)

-- | Gets a list of all discussion items for a single merge request.
projectMergeRequestDiscussionItems' ::
  -- | the project ID
  Int ->
  -- | Merge request IID
  Int ->
  GitLab (Either (Response BSL.ByteString) [Discussion])
projectMergeRequestDiscussionItems' projId mergeRequestIid = do
  gitlabGetMany urlPath []
  where
    urlPath =
      T.pack $
        "/projects/"
          <> show projId
          <> "/merge_requests/"
          <> show mergeRequestIid
          <> "/discussions"

-- | Gets a list of all discussion items for a single merge request.
mergeRequestDiscussionItems' ::
  -- | the project ID
  Int ->
  -- | Merge request IID
  Int ->
  -- | discussion ID
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe Discussion))
mergeRequestDiscussionItems' projId mergeRequestIid discussionId = do
  gitlabGetOne urlPath []
  where
    urlPath =
      T.pack $
        "/projects/"
          <> show projId
          <> "/merge_requests/"
          <> show mergeRequestIid
          <> "/discussions/"
          <> show discussionId

data PositionReference = TextPos | ImagePos

instance Show PositionReference where
  show TextPos = "text"
  show ImagePos = "image"

-- | Creates a new thread to a single project merge request. This is
-- similar to creating a note but other comments (replies) can be
-- added to it later.  See the GitLab document:
-- https://docs.gitlab.com/ee/api/discussions.html#create-new-merge-request-thread
createMergeRequestThread' ::
  -- | project ID
  Int ->
  -- | merge request ID
  Int ->
  -- | The content of the thread
  Text ->
  -- | Base commit SHA in the source branch
  Text ->
  -- | SHA referencing commit in target branch
  Text ->
  -- | SHA referencing HEAD of this merge request
  Text ->
  -- | Type of the position reference
  PositionReference ->
  -- | File path after change
  Text ->
  -- | File path before change
  Text ->
  GitLab (Either (Response BSL.ByteString) (Maybe Discussion))
createMergeRequestThread' projectId mergeRequestIid content baseCommitShaSource shaCommitTarget shaHeadMR typePosRef filePathAfter filePathBefore =
  gitlabPost
    discussionAddr
    [ ("body", Just (T.encodeUtf8 content)),
      ("position[base_sha]", Just (T.encodeUtf8 baseCommitShaSource)),
      ("position[start_sha]", Just (T.encodeUtf8 shaCommitTarget)),
      ("position[head_sha]", Just (T.encodeUtf8 shaHeadMR)),
      ("position[position_type]", Just (T.encodeUtf8 (T.pack (show typePosRef)))),
      ("position[new_path]", Just (T.encodeUtf8 filePathAfter)),
      ("position[old_path]", Just (T.encodeUtf8 filePathBefore))
    ]
  where
    discussionAddr :: Text
    discussionAddr =
      "/projects/"
        <> T.pack (show projectId)
        <> "/merge_requests/"
        <> T.pack (show mergeRequestIid)
        <> "/discussions"

-- | Resolve/unresolve whole thread of a merge request.
resolveMergeRequestThread' ::
  -- | project ID
  Int ->
  -- | merge request IID
  Int ->
  -- | discussion ID
  Int ->
  -- | Resolve/unresolve the discussion
  Bool ->
  GitLab (Either (Response BSL.ByteString) (Maybe Discussion))
resolveMergeRequestThread' projectId mergeRequestIid discussionId resolved =
  gitlabPut noteAddr [("resolved", Just (T.encodeUtf8 (resolvedStr resolved)))]
  where
    noteAddr :: Text
    noteAddr =
      "/projects/"
        <> T.pack (show projectId)
        <> "/merge_requests/"
        <> T.pack (show mergeRequestIid)
        <> "/discussions/"
        <> T.pack (show discussionId)
    resolvedStr True = "true"
    resolvedStr False = "false"

-- | Adds a new note to the thread. This can also create a thread from a single comment.
addNoteToMergeRequestThread' ::
  -- | project ID
  Int ->
  -- | merge request ID
  Int ->
  -- | discussion ID
  Int ->
  -- -- | note ID
  -- Int ->

  -- | The content of the note/reply
  Text ->
  GitLab (Either (Response BSL.ByteString) (Maybe Discussion))
addNoteToMergeRequestThread' projectId mergeRequestIid discussionId content =
  gitlabPost
    discussionAddr
    [ ("body", Just (T.encodeUtf8 content))
    ]
  where
    discussionAddr :: Text
    discussionAddr =
      "/projects/"
        <> T.pack (show projectId)
        <> "/merge_requests/"
        <> T.pack (show mergeRequestIid)
        <> "/discussions/"
        <> T.pack (show discussionId)
        <> "/notes"

-- | exactly one of body or resolved must be a 'Just' value
modifyMergeRequestThreadNote' ::
  -- | project ID
  Int ->
  -- | merge request IID
  Int ->
  -- | discussion ID
  Int ->
  -- | note ID
  Int ->
  -- | The content of the note/reply
  Maybe Text ->
  -- | Resolve/unresolve the note
  Maybe Bool ->
  GitLab (Either (Response BSL.ByteString) (Maybe Discussion))
modifyMergeRequestThreadNote' projectId mergeRequestIid discussionId noteId content resolved =
  gitlabPut
    noteAddr
    (catMaybes [contentAttr, resolveAttr])
  where
    contentAttr =
      case content of
        Nothing -> Nothing
        Just x -> Just ("body", Just (T.encodeUtf8 x))
    resolveAttr =
      case resolved of
        Nothing -> Nothing
        Just x -> case x of
          True -> Just ("resolved", Just (T.encodeUtf8 "true"))
          False -> Just ("resolved", Just (T.encodeUtf8 "false"))
    noteAddr :: Text
    noteAddr =
      "/projects/"
        <> T.pack (show projectId)
        <> "/merge_requests/"
        <> T.pack (show mergeRequestIid)
        <> "/discussions/"
        <> T.pack (show discussionId)
        <> "/notes/"
        <> T.pack (show noteId)

-- | Deletes an existing thread note of a merge request.
deleteMergeRequestThreadNote' ::
  -- | the project ID
  Int ->
  -- | merge request IID
  Int ->
  -- | discussion ID
  Int ->
  -- | note ID
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe ()))
deleteMergeRequestThreadNote' projectId mergeRequestIid discussionId noteId = do
  gitlabDelete noteAddr []
  where
    noteAddr :: Text
    noteAddr =
      "/projects/"
        <> T.pack (show projectId)
        <> "/merge_requests/"
        <> T.pack (show mergeRequestIid)
        <> "/discussions/"
        <> T.pack (show discussionId)
        <> "/notes/"
        <> T.pack (show noteId)

-- | Gets a list of all discussion items for a single commit.
projectCommitDiscussionItems' ::
  -- | project ID
  Int ->
  -- | commit ID
  Int ->
  GitLab (Either (Response BSL.ByteString) [Discussion])
projectCommitDiscussionItems' projectId commitId =
  gitlabGetMany urlPath []
  where
    urlPath =
      T.pack $
        "/projects/"
          <> show projectId
          <> "/commits/"
          <> show commitId
          <> "/discussions"

-- | Returns a single discussion item for a specific project commit.
projectCommitDiscussionItem' ::
  -- | project ID
  Int ->
  -- | commit ID
  Int ->
  -- | discussion ID
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe Discussion))
projectCommitDiscussionItem' projectId commitId discussionId =
  gitlabGetOne urlPath []
  where
    urlPath =
      T.pack $
        "/projects/"
          <> show projectId
          <> "/commits/"
          <> show commitId
          <> "/discussions/"
          <> show discussionId

-- | Creates a new thread to a single project commit. This is similar
-- to creating a note but other comments (replies) can be added to it
-- later.
createCommitThread' ::
  -- | project ID
  Int ->
  -- | commit ID
  Int ->
  -- | The content of the thread
  Text ->
  -- | SHA of the parent commit
  Text ->
  -- | SHA of the parent commit (bug in GitLab document?)
  Text ->
  -- | The SHA of this commit
  Text ->
  -- | Type of the position reference
  PositionReference ->
  GitLab (Either (Response BSL.ByteString) (Maybe Discussion))
createCommitThread' projectId commitId content shaParent shaStart shaThisCommit typePosRef =
  gitlabPost
    discussionAddr
    [ ("body", Just (T.encodeUtf8 content)),
      ("position[base_sha]", Just (T.encodeUtf8 shaParent)),
      ("position[start_sha]", Just (T.encodeUtf8 shaStart)),
      ("position[head_sha]", Just (T.encodeUtf8 shaThisCommit)),
      ("position[position_type]", Just (T.encodeUtf8 (T.pack (show typePosRef))))
    ]
  where
    discussionAddr :: Text
    discussionAddr =
      "/projects/"
        <> T.pack (show projectId)
        <> "/commits/"
        <> T.pack (show commitId)
        <> "/discussions"

-- | Adds a new note to the thread.
addNoteToCommitThread' ::
  -- | project ID
  Int ->
  -- | commit ID
  Int ->
  -- | discussion ID
  Int ->
  -- -- | note ID
  -- Int ->

  -- | The content of the note/reply
  Text ->
  GitLab (Either (Response BSL.ByteString) (Maybe Discussion))
addNoteToCommitThread' projectId commitId discussionId content =
  gitlabPost
    discussionAddr
    [ ("body", Just (T.encodeUtf8 content))
    ]
  where
    discussionAddr :: Text
    discussionAddr =
      "/projects/"
        <> T.pack (show projectId)
        <> "/commits/"
        <> T.pack (show commitId)
        <> "/discussions/"
        <> T.pack (show discussionId)
        <> "/notes"

-- | Adds a new note to the thread.
modifyCommityThreadNote' ::
  -- | project ID
  Int ->
  -- | commit ID
  Int ->
  -- | discussion ID
  Int ->
  -- | note ID
  Int ->
  -- | The content of the note/reply
  Maybe Text ->
  -- | Resolve/unresolve the note
  Maybe Bool ->
  GitLab (Either (Response BSL.ByteString) (Maybe Discussion))
modifyCommityThreadNote' projectId commitId discussionId noteId content resolved =
  gitlabPut
    noteAddr
    (catMaybes [contentAttr, resolveAttr])
  where
    contentAttr =
      case content of
        Nothing -> Nothing
        Just x -> Just ("body", Just (T.encodeUtf8 x))
    resolveAttr =
      case resolved of
        Nothing -> Nothing
        Just x -> case x of
          True -> Just ("resolved", Just (T.encodeUtf8 "true"))
          False -> Just ("resolved", Just (T.encodeUtf8 "false"))
    noteAddr :: Text
    noteAddr =
      "/projects/"
        <> T.pack (show projectId)
        <> "/commits/"
        <> T.pack (show commitId)
        <> "/discussions/"
        <> T.pack (show discussionId)
        <> "/notes/"
        <> T.pack (show noteId)

-- | Deletes an existing thread note of a commit.
deleteCommitThreadNote' ::
  -- | the project ID
  Int ->
  -- | commit ID
  Int ->
  -- | discussion ID
  Int ->
  -- | The ID of a discussion note
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe ()))
deleteCommitThreadNote' projectId commitId discussionId noteId = do
  gitlabDelete noteAddr []
  where
    noteAddr :: Text
    noteAddr =
      "/projects/"
        <> T.pack (show projectId)
        <> "/commits/"
        <> T.pack (show commitId)
        <> "/discussions/"
        <> T.pack (show discussionId)
        <> "/notes/"
        <> T.pack (show noteId)
