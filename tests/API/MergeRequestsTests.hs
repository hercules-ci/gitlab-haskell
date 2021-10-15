{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module API.MergeRequestsTests (mergeRequestsTests) where

import API.Common
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import GitLab
import GitLab.SystemHooks.GitLabSystemHooks
import GitLab.SystemHooks.Types
import Test.Tasty
import Test.Tasty.HUnit

-- | https://docs.gitlab.com/ee/api/merge_requests.html
mergeRequestsTests :: [TestTree]
mergeRequestsTests =
  [ testCase
      "merge-request-list-project-merge-requests"
      ( gitlabParseTestMany
          listProjectMergeRequestsHaskell
          "data/api/merge-requests/list-project-merge-requests.json"
      )
  ]

listProjectMergeRequestsHaskell :: [MergeRequest]
listProjectMergeRequestsHaskell =
  [MergeRequest {merge_request_id = 1, merge_request_iid = 1, merge_request_project_id = 3, merge_request_title = "test1", merge_request_description = "fixed login page css paddings", merge_request_state = "merged", merge_request_merged_by = Just (User {user_id = 87854, user_username = "DouweM", user_name = "Douwe Maan", user_state = "active", user_avatar_uri = Just "https://gitlab.example.com/uploads/-/system/user/avatar/87854/avatar.png", user_web_url = Just "https://gitlab.com/DouweM"}), merge_request_merged_at = Just (read "2018-09-07 11:16:17.520 UTC"), merge_request_closed_by = Nothing, merge_request_closed_at = Nothing, merge_request_created_at = (read "2017-04-29 08:46:00 UTC"), merge_request_updated_at = (read "2017-04-29 08:46:00 UTC"), merge_request_target_branch = "master", merge_request_source_branch = "test1", merge_request_upvotes = 0, merge_request_downvotes = 0, merge_request_author = User {user_id = 1, user_username = "admin", user_name = "Administrator", user_state = "active", user_avatar_uri = Nothing, user_web_url = Just "https://gitlab.example.com/admin"}, merge_request_assignee = Just (User {user_id = 1, user_username = "admin", user_name = "Administrator", user_state = "active", user_avatar_uri = Nothing, user_web_url = Just "https://gitlab.example.com/admin"}), merge_request_assignees = Just [User {user_id = 12, user_username = "axel.block", user_name = "Miss Monserrate Beier", user_state = "active", user_avatar_uri = Just "http://www.gravatar.com/avatar/46f6f7dc858ada7be1853f7fb96e81da?s=80&d=identicon", user_web_url = Just "https://gitlab.example.com/axel.block"}], merge_request_reviewers = Just [User {user_id = 2, user_username = "kenyatta_oconnell", user_name = "Sam Bauch", user_state = "active", user_avatar_uri = Just "https://www.gravatar.com/avatar/956c92487c6f6f7616b536927e22c9a0?s=80&d=identicon", user_web_url = Just "http://gitlab.example.com//kenyatta_oconnell"}], merge_request_source_project_id = 2, merge_request_target_project_id = 3, merge_request_labels = ["Community contribution", "Manage"], merge_request_work_in_progress = False, merge_request_milestone = Just (Milestone {milestone_project_id = Just 3, milestone_group_id = Nothing, milestone_description = Just "Assumenda aut placeat expedita exercitationem labore sunt enim earum.", milestone_state = Just MSClosed, milestone_due_date = Just "2018-09-22", milestone_iid = Just 1, milestone_created_at = Just (read "2015-02-02 19:49:26.013 UTC"), milestone_title = "v2.0", milestone_id = 5, milestone_updated_at = Just (read "2015-02-02 19:49:26.013 UTC"), milestone_web_url = Just "https://gitlab.example.com/my-group/my-project/milestones/1"}), merge_request_merge_when_pipeline_succeeds = True, merge_request_merge_status = "can_be_merged", merge_request_sha = "8888888888888888888888888888888888888888", merge_request_merge_commit_sha = Nothing, merge_request_user_notes_count = 1, merge_request_discussion_locked = Nothing, merge_request_should_remove_source_branch = Just True, merge_request_force_remove_source_branch = Just False, merge_request_allow_collaboration = Just False, merge_request_allow_maintainer_to_push = Just False, merge_request_web_url = "http://gitlab.example.com/my-group/my-project/merge_requests/1", merge_request_time_stats = TimeStats {time_estimate = 0, total_time_spent = 0, human_time_estimate = Nothing, human_total_time_spent = Nothing}, merge_request_squash = False, merge_request_changes_count = Nothing, merge_request_pipeline = Nothing, merge_request_diverged_commits_count = Nothing, merge_request_rebase_in_progress = Nothing, merge_request_has_conflicts = False, merge_request_blocking_discussions_resolved = Just True, merge_request_approvals_before_merge = Nothing}]
