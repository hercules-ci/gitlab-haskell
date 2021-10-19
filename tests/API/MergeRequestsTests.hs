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
import GitLab.Types (TimeEstimate)
import Test.Tasty
import Test.Tasty.HUnit

-- | https://docs.gitlab.com/ee/api/merge_requests.html
mergeRequestsTests :: [TestTree]
mergeRequestsTests =
  [ testCase
      "merge-request-accept-merge-request"
      ( gitlabParseTestOne
          acceptMergeRequestT
          "data/api/merge-requests/accept-merge-request.json"
      ),
    testCase
      "merge-request-add-spent-time-merge-request"
      ( gitlabParseTestOne
          addTimeSpentMergeRequest
          "data/api/merge-requests/add-spent-time-merge-request.json"
      ),
    testCase
      "merge-request-cancel-merge-request-when-pipeline-succeeds"
      ( gitlabParseTestOne
          cancelMergeRequest
          "data/api/merge-requests/cancel-merge-request-when-pipeline-succeeds.json"
      ),
    -- testCase
    --   "merge-request-comments-on-merge-requests"
    --   ( gitlabParseTestMany
    --       undefined
    --       "data/api/merge-requests/comments-on-merge-requests.json"
    --   ),
    testCase
      "merge-request-create-merge-request"
      ( gitlabParseTestOne
          createMergeRequestT
          "data/api/merge-requests/create-merge-request.json"
      ),
    testCase
      "merge-request-create-merge-request-pipeline"
      ( gitlabParseTestOne
          createMergeRequestPipeline
          "data/api/merge-requests/create-merge-request-pipeline.json"
      ),
    testCase
      "merge-request-create-todo-item"
      ( gitlabParseTestOne
          createTodoItem
          "data/api/merge-requests/create-todo-item.json"
      ),
    -- testCase
    --   "merge-request-get-merge-request-diff-versions"
    --   ( gitlabParseTestMany
    --       undefined
    --       "data/api/merge-requests/get-merge-request-diff-versions.json"
    --   ),
    testCase
      "merge-request-list-group-merge-requests"
      ( gitlabParseTestMany
          listGroupMergeRequestsHaskell
          "data/api/merge-requests/list-group-merge-requests.json"
      ),
    testCase
      "merge-request-list-merge-request-pipelines"
      ( gitlabParseTestMany
          listMergeRequestPipelines
          "data/api/merge-requests/list-merge-request-pipelines.json"
      ),
    testCase
      "merge-request-list-merge-requests"
      ( gitlabParseTestMany
          listMergeRequestsHaskell
          "data/api/merge-requests/list-merge-requests.json"
      ),
    testCase
      "merge-request-list-project-merge-requests"
      ( gitlabParseTestMany
          listProjectMergeRequestsHaskell
          "data/api/merge-requests/list-project-merge-requests.json"
      ),
    -- testCase
    --   "merge-request-merge-to-default-merge-path"
    --   ( gitlabParseTestMany
    --       undefined
    --       "data/api/merge-requests/merge-to-default-merge-path.json"
    --   ),
    -- testCase
    --   "merge-request-rebase-merge-request"
    --   ( gitlabParseTestMany
    --       undefined
    --       "data/api/merge-requests/rebase-merge-request.json"
    --   ),
    testCase
      "merge-request-reset-time-estimate-merge-request"
      ( gitlabParseTestOne
          resetTimeEstimateMergeRequest
          "data/api/merge-requests/reset-time-estimate-merge-request.json"
      ),
    testCase
      "merge-request-reset-time-merge-request"
      ( gitlabParseTestOne
          resetTimeMergeRequest
          "data/api/merge-requests/reset-time-merge-request.json"
      ),
    testCase
      "merge-request-single-merge-request-changes"
      ( gitlabParseTestOne
          singleMergeRequestChanges
          "data/api/merge-requests/single-merge-request-changes.json"
      ),
    -- testCase
    --   "merge-request-single-merge-request-commits"
    --   ( gitlabParseTestMany
    --       undefined
    --       "data/api/merge-requests/single-merge-request-commits.json"
    --   ),
    -- testCase
    --   "merge-request-single-merge-request-diff-version"
    --   ( gitlabParseTestMany
    --       undefined
    --       "data/api/merge-requests/single-merge-request-diff-version.json"
    --   ),
    testCase
      "merge-request-single-merge-request"
      ( gitlabParseTestOne
          singleMergeRequest
          "data/api/merge-requests/single-merge-request.json"
      ),
    testCase
      "merge-request-single-merge-request-participants"
      ( gitlabParseTestMany
          singleMergeRequestParticipants
          "data/api/merge-requests/single-merge-request-participants.json"
      ),
    testCase
      "merge-request-subscribe-merge-request"
      ( gitlabParseTestOne
          subscribeMergeRequest
          "data/api/merge-requests/subscribe-merge-request.json"
      ),
    testCase
      "merge-request-time-estimate-merge-request"
      ( gitlabParseTestOne
          timeEstimateMergeRequest
          "data/api/merge-requests/time-estimate-merge-request.json"
      ),
    testCase
      "merge-request-time-tracking-stats"
      ( gitlabParseTestOne
          timeTrackingStats
          "data/api/merge-requests/time-tracking-stats.json"
      ),
    testCase
      "merge-request-unsubscribe-merge-request"
      ( gitlabParseTestOne
          unsubscribeMergeRequest
          "data/api/merge-requests/unsubscribe-merge-request.json"
      ),
    testCase
      "merge-request-update-merge-request"
      ( gitlabParseTestOne
          updateMergeRequest
          "data/api/merge-requests/update-merge-request.json"
      )
  ]

cancelMergeRequest :: MergeRequest
cancelMergeRequest =
  MergeRequest
    { merge_request_id = 1,
      merge_request_iid = 1,
      merge_request_project_id = 3,
      merge_request_title = "test1",
      merge_request_description = "fixed login page css paddings",
      merge_request_state = "merged",
      merge_request_merged_by = Just (User {user_id = 87854, user_username = "DouweM", user_name = "Douwe Maan", user_state = "active", user_avatar_uri = Just "https://gitlab.example.com/uploads/-/system/user/avatar/87854/avatar.png", user_web_url = Just "https://gitlab.com/DouweM"}),
      merge_request_merged_at = Just (read "2018-09-07 11:16:17.52 UTC"),
      merge_request_closed_by = Nothing,
      merge_request_closed_at = Nothing,
      merge_request_created_at = read "2017-04-29 08:46:00 UTC",
      merge_request_updated_at = read "2017-04-29 08:46:00 UTC",
      merge_request_target_branch = "master",
      merge_request_source_branch = "test1",
      merge_request_upvotes = 0,
      merge_request_downvotes = 0,
      merge_request_author = User {user_id = 1, user_username = "admin", user_name = "Administrator", user_state = "active", user_avatar_uri = Nothing, user_web_url = Just "https://gitlab.example.com/admin"},
      merge_request_assignee = Just (User {user_id = 1, user_username = "admin", user_name = "Administrator", user_state = "active", user_avatar_uri = Nothing, user_web_url = Just "https://gitlab.example.com/admin"}),
      merge_request_assignees = Just [User {user_id = 12, user_username = "axel.block", user_name = "Miss Monserrate Beier", user_state = "active", user_avatar_uri = Just "http://www.gravatar.com/avatar/46f6f7dc858ada7be1853f7fb96e81da?s=80&d=identicon", user_web_url = Just "https://gitlab.example.com/axel.block"}],
      merge_request_reviewers = Just [User {user_id = 12, user_username = "axel.block", user_name = "Miss Monserrate Beier", user_state = "active", user_avatar_uri = Just "http://www.gravatar.com/avatar/46f6f7dc858ada7be1853f7fb96e81da?s=80&d=identicon", user_web_url = Just "https://gitlab.example.com/axel.block"}],
      merge_request_source_project_id = 2,
      merge_request_target_project_id = 3,
      merge_request_labels = ["Community contribution", "Manage"],
      merge_request_work_in_progress = False,
      merge_request_milestone = Just (Milestone {milestone_project_id = Just 3, milestone_group_id = Nothing, milestone_description = Just "Assumenda aut placeat expedita exercitationem labore sunt enim earum.", milestone_state = Just MSClosed, milestone_due_date = Just "2018-09-22", milestone_iid = Just 1, milestone_created_at = Just (read "2015-02-02 19:49:26.013 UTC"), milestone_title = "v2.0", milestone_id = 5, milestone_updated_at = Just (read "2015-02-02 19:49:26.013 UTC"), milestone_web_url = Just "https://gitlab.example.com/my-group/my-project/milestones/1"}),
      merge_request_merge_when_pipeline_succeeds = False,
      merge_request_merge_status = "can_be_merged",
      merge_request_sha = "8888888888888888888888888888888888888888",
      merge_request_merge_commit_sha = Nothing,
      merge_request_squash_commit_sha = Nothing,
      merge_request_user_notes_count = 1,
      merge_request_discussion_locked = Nothing,
      merge_request_should_remove_source_branch = Just True,
      merge_request_force_remove_source_branch = Just False,
      merge_request_allow_collaboration = Just False,
      merge_request_allow_maintainer_to_push = Just False,
      merge_request_web_url = "http://gitlab.example.com/my-group/my-project/merge_requests/1",
      merge_request_time_stats = Just (TimeStats {time_estimate = 0, total_time_spent = 0, human_time_estimate = Nothing, human_total_time_spent = Nothing}),
      merge_request_squash = False,
      merge_request_changes_count = Just "1",
      merge_request_pipeline = Just (Pipeline {pipeline_id = 29626725, sha = "2be7ddb704c7b6b83732fdd5b9f09d5a397b5f8f", pipeline_ref = "patch-28", pipeline_status = "success", pipeline_web_url = Just "https://gitlab.example.com/my-group/my-project/pipelines/29626725"}),
      merge_request_diverged_commits_count = Just 2,
      merge_request_rebase_in_progress = Nothing,
      merge_request_has_conflicts = Nothing,
      merge_request_blocking_discussions_resolved = Nothing,
      merge_request_approvals_before_merge = Nothing,
      merge_request_draft =
        Just False,
      merge_request_subscribed = Just False
    }

acceptMergeRequestT :: MergeRequest
acceptMergeRequestT =
  MergeRequest
    { merge_request_id = 1,
      merge_request_iid = 1,
      merge_request_project_id = 3,
      merge_request_title = "test1",
      merge_request_description = "fixed login page css paddings",
      merge_request_state = "merged",
      merge_request_merged_by = Just (User {user_id = 87854, user_username = "DouweM", user_name = "Douwe Maan", user_state = "active", user_avatar_uri = Just "https://gitlab.example.com/uploads/-/system/user/avatar/87854/avatar.png", user_web_url = Just "https://gitlab.com/DouweM"}),
      merge_request_merged_at = Just (read "2018-09-07 11:16:17.52 UTC"),
      merge_request_closed_by = Nothing,
      merge_request_closed_at = Nothing,
      merge_request_created_at = read "2017-04-29 08:46:00 UTC",
      merge_request_updated_at = read "2017-04-29 08:46:00 UTC",
      merge_request_target_branch = "master",
      merge_request_source_branch = "test1",
      merge_request_upvotes = 0,
      merge_request_downvotes = 0,
      merge_request_author = User {user_id = 1, user_username = "admin", user_name = "Administrator", user_state = "active", user_avatar_uri = Nothing, user_web_url = Just "https://gitlab.example.com/admin"},
      merge_request_assignee = Just (User {user_id = 1, user_username = "admin", user_name = "Administrator", user_state = "active", user_avatar_uri = Nothing, user_web_url = Just "https://gitlab.example.com/admin"}),
      merge_request_assignees = Just [User {user_id = 12, user_username = "axel.block", user_name = "Miss Monserrate Beier", user_state = "active", user_avatar_uri = Just "http://www.gravatar.com/avatar/46f6f7dc858ada7be1853f7fb96e81da?s=80&d=identicon", user_web_url = Just "https://gitlab.example.com/axel.block"}],
      merge_request_reviewers = Just [User {user_id = 12, user_username = "axel.block", user_name = "Miss Monserrate Beier", user_state = "active", user_avatar_uri = Just "http://www.gravatar.com/avatar/46f6f7dc858ada7be1853f7fb96e81da?s=80&d=identicon", user_web_url = Just "https://gitlab.example.com/axel.block"}],
      merge_request_source_project_id = 2,
      merge_request_target_project_id = 3,
      merge_request_labels = ["Community contribution", "Manage"],
      merge_request_work_in_progress = False,
      merge_request_milestone = Just (Milestone {milestone_project_id = Just 3, milestone_group_id = Nothing, milestone_description = Just "Assumenda aut placeat expedita exercitationem labore sunt enim earum.", milestone_state = Just MSClosed, milestone_due_date = Just "2018-09-22", milestone_iid = Just 1, milestone_created_at = Just (read "2015-02-02 19:49:26.013 UTC"), milestone_title = "v2.0", milestone_id = 5, milestone_updated_at = Just (read "2015-02-02 19:49:26.013 UTC"), milestone_web_url = Just "https://gitlab.example.com/my-group/my-project/milestones/1"}),
      merge_request_merge_when_pipeline_succeeds = True,
      merge_request_merge_status = "can_be_merged",
      merge_request_sha = "8888888888888888888888888888888888888888",
      merge_request_merge_commit_sha = Nothing,
      merge_request_squash_commit_sha = Nothing,
      merge_request_user_notes_count = 1,
      merge_request_discussion_locked = Nothing,
      merge_request_should_remove_source_branch = Just True,
      merge_request_force_remove_source_branch = Just False,
      merge_request_allow_collaboration = Just False,
      merge_request_allow_maintainer_to_push = Just False,
      merge_request_web_url = "http://gitlab.example.com/my-group/my-project/merge_requests/1",
      merge_request_time_stats = Just (TimeStats {time_estimate = 0, total_time_spent = 0, human_time_estimate = Nothing, human_total_time_spent = Nothing}),
      merge_request_squash = False,
      merge_request_changes_count = Just "1",
      merge_request_pipeline = Just (Pipeline {pipeline_id = 29626725, sha = "2be7ddb704c7b6b83732fdd5b9f09d5a397b5f8f", pipeline_ref = "patch-28", pipeline_status = "success", pipeline_web_url = Just "https://gitlab.example.com/my-group/my-project/pipelines/29626725"}),
      merge_request_diverged_commits_count = Just 2,
      merge_request_rebase_in_progress = Nothing,
      merge_request_has_conflicts = Nothing,
      merge_request_blocking_discussions_resolved = Nothing,
      merge_request_approvals_before_merge = Nothing,
      merge_request_draft =
        Just False,
      merge_request_subscribed = Just False
    }

createMergeRequestT :: MergeRequest
createMergeRequestT =
  MergeRequest
    { merge_request_id = 1,
      merge_request_iid = 1,
      merge_request_project_id = 3,
      merge_request_title = "test1",
      merge_request_description = "fixed login page css paddings",
      merge_request_state = "merged",
      merge_request_merged_by = Just (User {user_id = 87854, user_username = "DouweM", user_name = "Douwe Maan", user_state = "active", user_avatar_uri = Just "https://gitlab.example.com/uploads/-/system/user/avatar/87854/avatar.png", user_web_url = Just "https://gitlab.com/DouweM"}),
      merge_request_merged_at = Just (read "2018-09-07 11:16:17.52 UTC"),
      merge_request_closed_by = Nothing,
      merge_request_closed_at = Nothing,
      merge_request_created_at = read "2017-04-29 08:46:00 UTC",
      merge_request_updated_at = read "2017-04-29 08:46:00 UTC",
      merge_request_target_branch = "master",
      merge_request_source_branch = "test1",
      merge_request_upvotes = 0,
      merge_request_downvotes = 0,
      merge_request_author = User {user_id = 1, user_username = "admin", user_name = "Administrator", user_state = "active", user_avatar_uri = Nothing, user_web_url = Just "https://gitlab.example.com/admin"},
      merge_request_assignee = Just (User {user_id = 1, user_username = "admin", user_name = "Administrator", user_state = "active", user_avatar_uri = Nothing, user_web_url = Just "https://gitlab.example.com/admin"}),
      merge_request_assignees = Nothing,
      merge_request_reviewers = Nothing,
      merge_request_source_project_id = 2,
      merge_request_target_project_id = 3,
      merge_request_labels = ["Community contribution", "Manage"],
      merge_request_work_in_progress = False,
      merge_request_milestone = Just (Milestone {milestone_project_id = Just 3, milestone_group_id = Nothing, milestone_description = Just "Assumenda aut placeat expedita exercitationem labore sunt enim earum.", milestone_state = Just MSClosed, milestone_due_date = Just "2018-09-22", milestone_iid = Just 1, milestone_created_at = Just (read "2015-02-02 19:49:26.013 UTC"), milestone_title = "v2.0", milestone_id = 5, milestone_updated_at = Just (read "2015-02-02 19:49:26.013 UTC"), milestone_web_url = Just "https://gitlab.example.com/my-group/my-project/milestones/1"}),
      merge_request_merge_when_pipeline_succeeds = True,
      merge_request_merge_status = "can_be_merged",
      merge_request_sha = "8888888888888888888888888888888888888888",
      merge_request_merge_commit_sha = Nothing,
      merge_request_squash_commit_sha = Nothing,
      merge_request_user_notes_count = 1,
      merge_request_discussion_locked = Nothing,
      merge_request_should_remove_source_branch = Just True,
      merge_request_force_remove_source_branch = Just False,
      merge_request_allow_collaboration = Just False,
      merge_request_allow_maintainer_to_push = Just False,
      merge_request_web_url = "http://gitlab.example.com/my-group/my-project/merge_requests/1",
      merge_request_time_stats = Just (TimeStats {time_estimate = 0, total_time_spent = 0, human_time_estimate = Nothing, human_total_time_spent = Nothing}),
      merge_request_squash = False,
      merge_request_changes_count = Just "1",
      merge_request_pipeline = Just (Pipeline {pipeline_id = 29626725, sha = "2be7ddb704c7b6b83732fdd5b9f09d5a397b5f8f", pipeline_ref = "patch-28", pipeline_status = "success", pipeline_web_url = Just "https://gitlab.example.com/my-group/my-project/pipelines/29626725"}),
      merge_request_diverged_commits_count = Just 2,
      merge_request_rebase_in_progress = Nothing,
      merge_request_has_conflicts = Nothing,
      merge_request_blocking_discussions_resolved = Nothing,
      merge_request_approvals_before_merge = Nothing,
      merge_request_draft =
        Just False,
      merge_request_subscribed = Just False
    }

listMergeRequestsHaskell :: [MergeRequest]
listMergeRequestsHaskell =
  [ MergeRequest
      { merge_request_id = 1,
        merge_request_iid = 1,
        merge_request_project_id = 3,
        merge_request_title = "test1",
        merge_request_description = "fixed login page css paddings",
        merge_request_state = "merged",
        merge_request_merged_by = Just (User {user_id = 87854, user_username = "DouweM", user_name = "Douwe Maan", user_state = "active", user_avatar_uri = Just "https://gitlab.example.com/uploads/-/system/user/avatar/87854/avatar.png", user_web_url = Just "https://gitlab.com/DouweM"}),
        merge_request_merged_at = Just (read "2018-09-07 11:16:17.52 UTC"),
        merge_request_closed_by = Nothing,
        merge_request_closed_at = Nothing,
        merge_request_created_at = read "2017-04-29 08:46:00 UTC",
        merge_request_updated_at = read "2017-04-29 08:46:00 UTC",
        merge_request_target_branch = "master",
        merge_request_source_branch = "test1",
        merge_request_upvotes = 0,
        merge_request_downvotes = 0,
        merge_request_author = User {user_id = 1, user_username = "admin", user_name = "Administrator", user_state = "active", user_avatar_uri = Nothing, user_web_url = Just "https://gitlab.example.com/admin"},
        merge_request_assignee = Just (User {user_id = 1, user_username = "admin", user_name = "Administrator", user_state = "active", user_avatar_uri = Nothing, user_web_url = Just "https://gitlab.example.com/admin"}),
        merge_request_assignees = Just [User {user_id = 12, user_username = "axel.block", user_name = "Miss Monserrate Beier", user_state = "active", user_avatar_uri = Just "http://www.gravatar.com/avatar/46f6f7dc858ada7be1853f7fb96e81da?s=80&d=identicon", user_web_url = Just "https://gitlab.example.com/axel.block"}],
        merge_request_reviewers = Just [User {user_id = 2, user_username = "kenyatta_oconnell", user_name = "Sam Bauch", user_state = "active", user_avatar_uri = Just "https://www.gravatar.com/avatar/956c92487c6f6f7616b536927e22c9a0?s=80&d=identicon", user_web_url = Just "http://gitlab.example.com//kenyatta_oconnell"}],
        merge_request_source_project_id = 2,
        merge_request_target_project_id = 3,
        merge_request_labels = ["Community contribution", "Manage"],
        merge_request_work_in_progress = False,
        merge_request_milestone = Just (Milestone {milestone_project_id = Just 3, milestone_group_id = Nothing, milestone_description = Just "Assumenda aut placeat expedita exercitationem labore sunt enim earum.", milestone_state = Just MSClosed, milestone_due_date = Just "2018-09-22", milestone_iid = Just 1, milestone_created_at = Just (read "2015-02-02 19:49:26.013 UTC"), milestone_title = "v2.0", milestone_id = 5, milestone_updated_at = Just (read "2015-02-02 19:49:26.013 UTC"), milestone_web_url = Just "https://gitlab.example.com/my-group/my-project/milestones/1"}),
        merge_request_merge_when_pipeline_succeeds = True,
        merge_request_merge_status = "can_be_merged",
        merge_request_sha = "8888888888888888888888888888888888888888",
        merge_request_merge_commit_sha = Nothing,
        merge_request_squash_commit_sha = Nothing,
        merge_request_user_notes_count = 1,
        merge_request_discussion_locked = Nothing,
        merge_request_should_remove_source_branch = Just True,
        merge_request_force_remove_source_branch = Just False,
        merge_request_allow_collaboration = Just False,
        merge_request_allow_maintainer_to_push = Just False,
        merge_request_web_url = "http://gitlab.example.com/my-group/my-project/merge_requests/1",
        merge_request_time_stats = Just (TimeStats {time_estimate = 0, total_time_spent = 0, human_time_estimate = Nothing, human_total_time_spent = Nothing}),
        merge_request_squash = False,
        merge_request_changes_count = Nothing,
        merge_request_pipeline = Nothing,
        merge_request_diverged_commits_count = Nothing,
        merge_request_rebase_in_progress = Nothing,
        merge_request_has_conflicts = Nothing,
        merge_request_blocking_discussions_resolved = Nothing,
        merge_request_approvals_before_merge = Nothing,
        merge_request_draft =
          Just False,
        merge_request_subscribed = Nothing
      }
  ]

listProjectMergeRequestsHaskell :: [MergeRequest]
listProjectMergeRequestsHaskell =
  [ MergeRequest
      { merge_request_id = 1,
        merge_request_iid = 1,
        merge_request_project_id = 3,
        merge_request_title = "test1",
        merge_request_description = "fixed login page css paddings",
        merge_request_state = "merged",
        merge_request_merged_by = Just (User {user_id = 87854, user_username = "DouweM", user_name = "Douwe Maan", user_state = "active", user_avatar_uri = Just "https://gitlab.example.com/uploads/-/system/user/avatar/87854/avatar.png", user_web_url = Just "https://gitlab.com/DouweM"}),
        merge_request_merged_at = Just (read "2018-09-07 11:16:17.52 UTC"),
        merge_request_closed_by = Nothing,
        merge_request_closed_at = Nothing,
        merge_request_created_at = read "2017-04-29 08:46:00 UTC",
        merge_request_updated_at = read "2017-04-29 08:46:00 UTC",
        merge_request_target_branch = "master",
        merge_request_source_branch = "test1",
        merge_request_upvotes = 0,
        merge_request_downvotes = 0,
        merge_request_author = User {user_id = 1, user_username = "admin", user_name = "Administrator", user_state = "active", user_avatar_uri = Nothing, user_web_url = Just "https://gitlab.example.com/admin"},
        merge_request_assignee = Just (User {user_id = 1, user_username = "admin", user_name = "Administrator", user_state = "active", user_avatar_uri = Nothing, user_web_url = Just "https://gitlab.example.com/admin"}),
        merge_request_assignees = Just [User {user_id = 12, user_username = "axel.block", user_name = "Miss Monserrate Beier", user_state = "active", user_avatar_uri = Just "http://www.gravatar.com/avatar/46f6f7dc858ada7be1853f7fb96e81da?s=80&d=identicon", user_web_url = Just "https://gitlab.example.com/axel.block"}],
        merge_request_reviewers = Just [User {user_id = 2, user_username = "kenyatta_oconnell", user_name = "Sam Bauch", user_state = "active", user_avatar_uri = Just "https://www.gravatar.com/avatar/956c92487c6f6f7616b536927e22c9a0?s=80&d=identicon", user_web_url = Just "http://gitlab.example.com//kenyatta_oconnell"}],
        merge_request_source_project_id = 2,
        merge_request_target_project_id = 3,
        merge_request_labels = ["Community contribution", "Manage"],
        merge_request_work_in_progress = False,
        merge_request_milestone = Just (Milestone {milestone_project_id = Just 3, milestone_group_id = Nothing, milestone_description = Just "Assumenda aut placeat expedita exercitationem labore sunt enim earum.", milestone_state = Just MSClosed, milestone_due_date = Just "2018-09-22", milestone_iid = Just 1, milestone_created_at = Just (read "2015-02-02 19:49:26.013 UTC"), milestone_title = "v2.0", milestone_id = 5, milestone_updated_at = Just (read "2015-02-02 19:49:26.013 UTC"), milestone_web_url = Just "https://gitlab.example.com/my-group/my-project/milestones/1"}),
        merge_request_merge_when_pipeline_succeeds = True,
        merge_request_merge_status = "can_be_merged",
        merge_request_sha = "8888888888888888888888888888888888888888",
        merge_request_merge_commit_sha = Nothing,
        merge_request_squash_commit_sha = Nothing,
        merge_request_user_notes_count = 1,
        merge_request_discussion_locked = Nothing,
        merge_request_should_remove_source_branch = Just True,
        merge_request_force_remove_source_branch = Just False,
        merge_request_allow_collaboration = Just False,
        merge_request_allow_maintainer_to_push = Just False,
        merge_request_web_url = "http://gitlab.example.com/my-group/my-project/merge_requests/1",
        merge_request_time_stats = Just (TimeStats {time_estimate = 0, total_time_spent = 0, human_time_estimate = Nothing, human_total_time_spent = Nothing}),
        merge_request_squash = False,
        merge_request_changes_count = Nothing,
        merge_request_pipeline = Nothing,
        merge_request_diverged_commits_count = Nothing,
        merge_request_rebase_in_progress = Nothing,
        merge_request_has_conflicts = Just False,
        merge_request_blocking_discussions_resolved = Just True,
        merge_request_approvals_before_merge = Nothing,
        merge_request_draft =
          Just False,
        merge_request_subscribed = Nothing
      }
  ]

listGroupMergeRequestsHaskell :: [MergeRequest]
listGroupMergeRequestsHaskell =
  [ MergeRequest
      { merge_request_id = 1,
        merge_request_iid = 1,
        merge_request_project_id = 3,
        merge_request_title = "test1",
        merge_request_description = "fixed login page css paddings",
        merge_request_state = "merged",
        merge_request_merged_by = Just (User {user_id = 87854, user_username = "DouweM", user_name = "Douwe Maan", user_state = "active", user_avatar_uri = Just "https://gitlab.example.com/uploads/-/system/user/avatar/87854/avatar.png", user_web_url = Just "https://gitlab.com/DouweM"}),
        merge_request_merged_at = Just (read "2018-09-07 11:16:17.52 UTC"),
        merge_request_closed_by = Nothing,
        merge_request_closed_at = Nothing,
        merge_request_created_at = read "2017-04-29 08:46:00 UTC",
        merge_request_updated_at = read "2017-04-29 08:46:00 UTC",
        merge_request_target_branch = "master",
        merge_request_source_branch = "test1",
        merge_request_upvotes = 0,
        merge_request_downvotes = 0,
        merge_request_author = User {user_id = 1, user_username = "admin", user_name = "Administrator", user_state = "active", user_avatar_uri = Nothing, user_web_url = Just "https://gitlab.example.com/admin"},
        merge_request_assignee = Just (User {user_id = 1, user_username = "admin", user_name = "Administrator", user_state = "active", user_avatar_uri = Nothing, user_web_url = Just "https://gitlab.example.com/admin"}),
        merge_request_assignees = Just [User {user_id = 12, user_username = "axel.block", user_name = "Miss Monserrate Beier", user_state = "active", user_avatar_uri = Just "http://www.gravatar.com/avatar/46f6f7dc858ada7be1853f7fb96e81da?s=80&d=identicon", user_web_url = Just "https://gitlab.example.com/axel.block"}],
        merge_request_reviewers = Just [User {user_id = 2, user_username = "kenyatta_oconnell", user_name = "Sam Bauch", user_state = "active", user_avatar_uri = Just "https://www.gravatar.com/avatar/956c92487c6f6f7616b536927e22c9a0?s=80&d=identicon", user_web_url = Just "http://gitlab.example.com//kenyatta_oconnell"}],
        merge_request_source_project_id = 2,
        merge_request_target_project_id = 3,
        merge_request_labels = ["Community contribution", "Manage"],
        merge_request_work_in_progress = False,
        merge_request_milestone = Just (Milestone {milestone_project_id = Just 3, milestone_group_id = Nothing, milestone_description = Just "Assumenda aut placeat expedita exercitationem labore sunt enim earum.", milestone_state = Just MSClosed, milestone_due_date = Just "2018-10-22", milestone_iid = Just 1, milestone_created_at = Just (read "2015-02-02 19:49:26.013 UTC"), milestone_title = "v2.0", milestone_id = 5, milestone_updated_at = Just (read "2015-02-02 19:49:26.013 UTC"), milestone_web_url = Just "gitlab.example.com/my-group/my-project/milestones/1"}),
        merge_request_merge_when_pipeline_succeeds = True,
        merge_request_merge_status = "can_be_merged",
        merge_request_sha = "8888888888888888888888888888888888888888",
        merge_request_merge_commit_sha = Nothing,
        merge_request_squash_commit_sha = Nothing,
        merge_request_user_notes_count = 1,
        merge_request_discussion_locked = Nothing,
        merge_request_should_remove_source_branch = Just True,
        merge_request_force_remove_source_branch = Just False,
        merge_request_allow_collaboration = Nothing,
        merge_request_allow_maintainer_to_push = Nothing,
        merge_request_web_url = "http://gitlab.example.com/my-group/my-project/merge_requests/1",
        merge_request_time_stats = Just (TimeStats {time_estimate = 0, total_time_spent = 0, human_time_estimate = Nothing, human_total_time_spent = Nothing}),
        merge_request_squash = False,
        merge_request_changes_count = Nothing,
        merge_request_pipeline = Nothing,
        merge_request_diverged_commits_count = Nothing,
        merge_request_rebase_in_progress = Nothing,
        merge_request_has_conflicts = Just False,
        merge_request_blocking_discussions_resolved = Just True,
        merge_request_approvals_before_merge = Nothing,
        merge_request_draft =
          Just False,
        merge_request_subscribed = Nothing
      }
  ]

singleMergeRequest :: MergeRequest
singleMergeRequest =
  MergeRequest
    { merge_request_id = 1,
      merge_request_iid = 1,
      merge_request_project_id = 3,
      merge_request_title = "test1",
      merge_request_description = "fixed login page css paddings",
      merge_request_state = "merged",
      merge_request_merged_by = Just (User {user_id = 87854, user_username = "DouweM", user_name = "Douwe Maan", user_state = "active", user_avatar_uri = Just "https://gitlab.example.com/uploads/-/system/user/avatar/87854/avatar.png", user_web_url = Just "https://gitlab.com/DouweM"}),
      merge_request_merged_at = Just (read "2018-09-07 11:16:17.52 UTC"),
      merge_request_closed_by = Nothing,
      merge_request_closed_at = Nothing,
      merge_request_created_at = read "2017-04-29 08:46:00 UTC",
      merge_request_updated_at = read "2017-04-29 08:46:00 UTC",
      merge_request_target_branch = "master",
      merge_request_source_branch = "test1",
      merge_request_upvotes = 0,
      merge_request_downvotes = 0,
      merge_request_author = User {user_id = 1, user_username = "admin", user_name = "Administrator", user_state = "active", user_avatar_uri = Nothing, user_web_url = Just "https://gitlab.example.com/admin"},
      merge_request_assignee = Just (User {user_id = 1, user_username = "admin", user_name = "Administrator", user_state = "active", user_avatar_uri = Nothing, user_web_url = Just "https://gitlab.example.com/admin"}),
      merge_request_assignees = Just [User {user_id = 12, user_username = "axel.block", user_name = "Miss Monserrate Beier", user_state = "active", user_avatar_uri = Just "http://www.gravatar.com/avatar/46f6f7dc858ada7be1853f7fb96e81da?s=80&d=identicon", user_web_url = Just "https://gitlab.example.com/axel.block"}],
      merge_request_reviewers = Just [User {user_id = 2, user_username = "kenyatta_oconnell", user_name = "Sam Bauch", user_state = "active", user_avatar_uri = Just "https://www.gravatar.com/avatar/956c92487c6f6f7616b536927e22c9a0?s=80&d=identicon", user_web_url = Just "http://gitlab.example.com//kenyatta_oconnell"}],
      merge_request_source_project_id = 2,
      merge_request_target_project_id = 3,
      merge_request_labels = ["Community contribution", "Manage"],
      merge_request_work_in_progress = False,
      merge_request_milestone = Just (Milestone {milestone_project_id = Just 3, milestone_group_id = Nothing, milestone_description = Just "Assumenda aut placeat expedita exercitationem labore sunt enim earum.", milestone_state = Just MSClosed, milestone_due_date = Just "2018-09-22", milestone_iid = Just 1, milestone_created_at = Just (read "2015-02-02 19:49:26.013 UTC"), milestone_title = "v2.0", milestone_id = 5, milestone_updated_at = Just (read "2015-02-02 19:49:26.013 UTC"), milestone_web_url = Just "https://gitlab.example.com/my-group/my-project/milestones/1"}),
      merge_request_merge_when_pipeline_succeeds = True,
      merge_request_merge_status = "can_be_merged",
      merge_request_sha = "8888888888888888888888888888888888888888",
      merge_request_merge_commit_sha = Nothing,
      merge_request_squash_commit_sha = Nothing,
      merge_request_user_notes_count = 1,
      merge_request_discussion_locked = Nothing,
      merge_request_should_remove_source_branch = Just True,
      merge_request_force_remove_source_branch = Just False,
      merge_request_allow_collaboration = Just False,
      merge_request_allow_maintainer_to_push = Just False,
      merge_request_web_url = "http://gitlab.example.com/my-group/my-project/merge_requests/1",
      merge_request_time_stats = Just (TimeStats {time_estimate = 0, total_time_spent = 0, human_time_estimate = Nothing, human_total_time_spent = Nothing}),
      merge_request_squash = False,
      merge_request_changes_count = Just "1",
      merge_request_pipeline = Just (Pipeline {pipeline_id = 29626725, sha = "2be7ddb704c7b6b83732fdd5b9f09d5a397b5f8f", pipeline_ref = "patch-28", pipeline_status = "success", pipeline_web_url = Just "https://gitlab.example.com/my-group/my-project/pipelines/29626725"}),
      merge_request_diverged_commits_count = Just 2,
      merge_request_rebase_in_progress = Just False,
      merge_request_has_conflicts = Just False,
      merge_request_blocking_discussions_resolved = Just True,
      merge_request_approvals_before_merge = Nothing,
      merge_request_draft =
        Just False,
      merge_request_subscribed = Just False
    }

singleMergeRequestParticipants :: [User]
singleMergeRequestParticipants =
  [User {user_id = 1, user_username = "user1", user_name = "John Doe1", user_state = "active", user_avatar_uri = Just "http://www.gravatar.com/avatar/c922747a93b40d1ea88262bf1aebee62?s=80&d=identicon", user_web_url = Just "http://localhost/user1"}, User {user_id = 2, user_username = "user2", user_name = "John Doe2", user_state = "active", user_avatar_uri = Just "http://www.gravatar.com/avatar/10fc7f102be8de7657fb4d80898bbfe3?s=80&d=identicon", user_web_url = Just "http://localhost/user2"}]

timeEstimateMergeRequest :: TimeEstimate
timeEstimateMergeRequest =
  TimeEstimate {time_estimate_human_time_estimate = Just "3h 30m", time_estimate_human_time_spent = Nothing, time_estimate_time_estimate = Just 12600, time_estimate_total_time_spent = Just 0}

timeTrackingStats :: TimeEstimate
timeTrackingStats =
  TimeEstimate {time_estimate_human_time_estimate = Just "2h", time_estimate_human_time_spent = Nothing, time_estimate_time_estimate = Just 7200, time_estimate_total_time_spent = Just 3600}

resetTimeEstimateMergeRequest :: TimeEstimate
resetTimeEstimateMergeRequest =
  TimeEstimate {time_estimate_human_time_estimate = Nothing, time_estimate_human_time_spent = Nothing, time_estimate_time_estimate = Just 0, time_estimate_total_time_spent = Just 0}

resetTimeMergeRequest :: TimeEstimate
resetTimeMergeRequest =
  TimeEstimate {time_estimate_human_time_estimate = Nothing, time_estimate_human_time_spent = Nothing, time_estimate_time_estimate = Just 0, time_estimate_total_time_spent = Just 0}

addTimeSpentMergeRequest :: TimeEstimate
addTimeSpentMergeRequest =
  TimeEstimate {time_estimate_human_time_estimate = Nothing, time_estimate_human_time_spent = Nothing, time_estimate_time_estimate = Just 0, time_estimate_total_time_spent = Just 3600}

createMergeRequestPipeline :: Pipeline
createMergeRequestPipeline =
  Pipeline {pipeline_id = 2, sha = "b83d6e391c22777fca1ed3012fce84f633d7fed0", pipeline_ref = "refs/merge-requests/1/head", pipeline_status = "pending", pipeline_web_url = Just "http://localhost/user1/project1/pipelines/2"}

createTodoItem :: Todo
createTodoItem =
  Todo
    { todo_id = 113,
      todo_project = TP {tp_id = 3, tp_description = Nothing, tp_name = "GitLab CI/CD", tp_name_with_namespace = "GitLab Org / GitLab CI/CD", tp_path = "gitlab-ci", tp_path_with_namespace = "gitlab-org/gitlab-ci", tp_created_at = Nothing},
      todo_author = User {user_id = 1, user_username = "root", user_name = "Administrator", user_state = "active", user_avatar_uri = Just "http://www.gravatar.com/avatar/e64c7d89f26bd1972efa854d13d7dd61?s=80&d=identicon", user_web_url = Just "https://gitlab.example.com/root"},
      todo_action_name = TAMarked,
      todo_target =
        TTMergeRequest
          ( MergeRequest
              { merge_request_id = 27,
                merge_request_iid = 7,
                merge_request_project_id = 3,
                merge_request_title = "Et voluptas laudantium minus nihil recusandae ut accusamus earum aut non.",
                merge_request_description = "Veniam sunt nihil modi earum cumque illum delectus. Nihil ad quis distinctio quia. Autem eligendi at quibusdam repellendus.",
                merge_request_state = "merged",
                merge_request_merged_by = Nothing,
                merge_request_merged_at = Nothing,
                merge_request_closed_by = Nothing,
                merge_request_closed_at = Nothing,
                merge_request_created_at = read "2016-06-17 07:48:04.33 UTC",
                merge_request_updated_at = read "2016-07-01 11:14:15.537 UTC",
                merge_request_target_branch = "allow_regex_for_project_skip_ref",
                merge_request_source_branch = "backup",
                merge_request_upvotes = 0,
                merge_request_downvotes = 0,
                merge_request_author = User {user_id = 14, user_username = "francisca", user_name = "Jarret O'Keefe", user_state = "active", user_avatar_uri = Just "http://www.gravatar.com/avatar/a7fa515d53450023c83d62986d0658a8?s=80&d=identicon", user_web_url = Just "https://gitlab.example.com/francisca"},
                merge_request_assignee = Just (User {user_id = 4, user_username = "barrett.krajcik", user_name = "Dr. Gabrielle Strosin", user_state = "active", user_avatar_uri = Just "http://www.gravatar.com/avatar/733005fcd7e6df12d2d8580171ccb966?s=80&d=identicon", user_web_url = Just "https://gitlab.example.com/barrett.krajcik"}),
                merge_request_assignees = Just [User {user_id = 12, user_username = "axel.block", user_name = "Miss Monserrate Beier", user_state = "active", user_avatar_uri = Just "http://www.gravatar.com/avatar/46f6f7dc858ada7be1853f7fb96e81da?s=80&d=identicon", user_web_url = Just "https://gitlab.example.com/axel.block"}],
                merge_request_reviewers = Just [User {user_id = 12, user_username = "axel.block", user_name = "Miss Monserrate Beier", user_state = "active", user_avatar_uri = Just "http://www.gravatar.com/avatar/46f6f7dc858ada7be1853f7fb96e81da?s=80&d=identicon", user_web_url = Just "https://gitlab.example.com/axel.block"}],
                merge_request_source_project_id = 3,
                merge_request_target_project_id = 3,
                merge_request_labels = [],
                merge_request_work_in_progress = False,
                merge_request_milestone = Just (Milestone {milestone_project_id = Just 3, milestone_group_id = Nothing, milestone_description = Just "Quis ea accusantium animi hic fuga assumenda.", milestone_state = Just MSActive, milestone_due_date = Nothing, milestone_iid = Just 2, milestone_created_at = Just (read "2016-06-17 07:47:33.84 UTC"), milestone_title = "v1.0", milestone_id = 27, milestone_updated_at = Just (read "2016-06-17 07:47:33.84 UTC"), milestone_web_url = Nothing}),
                merge_request_merge_when_pipeline_succeeds = False,
                merge_request_merge_status = "unchecked",
                merge_request_sha = "8888888888888888888888888888888888888888",
                merge_request_merge_commit_sha = Nothing,
                merge_request_squash_commit_sha = Nothing,
                merge_request_user_notes_count = 7,
                merge_request_discussion_locked = Nothing,
                merge_request_should_remove_source_branch = Just True,
                merge_request_force_remove_source_branch = Just False,
                merge_request_allow_collaboration = Nothing,
                merge_request_allow_maintainer_to_push = Nothing,
                merge_request_web_url = "http://example.com/my-group/my-project/merge_requests/1",
                merge_request_time_stats = Nothing,
                merge_request_squash = False,
                merge_request_changes_count = Just "1",
                merge_request_pipeline = Nothing,
                merge_request_diverged_commits_count = Nothing,
                merge_request_rebase_in_progress = Nothing,
                merge_request_has_conflicts = Nothing,
                merge_request_blocking_discussions_resolved = Nothing,
                merge_request_approvals_before_merge = Nothing,
                merge_request_draft =
                  Just False,
                merge_request_subscribed = Just True
              }
          ),
      todo_target_url = "https://gitlab.example.com/gitlab-org/gitlab-ci/merge_requests/7",
      todo_body = "Et voluptas laudantium minus nihil recusandae ut accusamus earum aut non.",
      todo_state = TSPending,
      todo_created_at = read "2016-07-01 11:14:15.53 UTC"
    }

listMergeRequestPipelines :: [Pipeline]
listMergeRequestPipelines =
  [Pipeline {pipeline_id = 77, sha = "959e04d7c7a30600c894bd3c0cd0e1ce7f42c11d", pipeline_ref = "master", pipeline_status = "success", pipeline_web_url = Nothing}]

singleMergeRequestChanges :: MergeRequest
singleMergeRequestChanges =
  MergeRequest
    { merge_request_id = 21,
      merge_request_iid = 1,
      merge_request_project_id = 4,
      merge_request_title = "Blanditiis beatae suscipit hic assumenda et molestias nisi asperiores repellat et.",
      merge_request_description = "Qui voluptatibus placeat ipsa alias quasi. Deleniti rem ut sint. Optio velit qui distinctio.",
      merge_request_state = "reopened",
      merge_request_merged_by = Nothing,
      merge_request_merged_at = Nothing,
      merge_request_closed_by = Nothing,
      merge_request_closed_at = Nothing,
      merge_request_created_at = read "2015-02-02 19:49:39.159 UTC",
      merge_request_updated_at = read "2015-02-02 20:08:49.959 UTC",
      merge_request_target_branch = "secret_token",
      merge_request_source_branch = "version-1-9",
      merge_request_upvotes = 0,
      merge_request_downvotes = 0,
      merge_request_author = User {user_id = 5, user_username = "jarrett", user_name = "Chad Hamill", user_state = "active", user_avatar_uri = Just "http://www.gravatar.com/avatar/b95567800f828948baf5f4160ebb2473?s=40&d=identicon", user_web_url = Just "https://gitlab.example.com/jarrett"},
      merge_request_assignee = Just (User {user_id = 1, user_username = "root", user_name = "Administrator", user_state = "active", user_avatar_uri = Just "http://www.gravatar.com/avatar/e64c7d89f26bd1972efa854d13d7dd61?s=40&d=identicon", user_web_url = Just "https://gitlab.example.com/root"}),
      merge_request_assignees = Just [User {user_id = 12, user_username = "axel.block", user_name = "Miss Monserrate Beier", user_state = "active", user_avatar_uri = Just "http://www.gravatar.com/avatar/46f6f7dc858ada7be1853f7fb96e81da?s=80&d=identicon", user_web_url = Just "https://gitlab.example.com/axel.block"}],
      merge_request_reviewers = Just [User {user_id = 12, user_username = "axel.block", user_name = "Miss Monserrate Beier", user_state = "active", user_avatar_uri = Just "http://www.gravatar.com/avatar/46f6f7dc858ada7be1853f7fb96e81da?s=80&d=identicon", user_web_url = Just "https://gitlab.example.com/axel.block"}],
      merge_request_source_project_id = 4,
      merge_request_target_project_id = 4,
      merge_request_labels = [],
      merge_request_work_in_progress = False,
      merge_request_milestone = Just (Milestone {milestone_project_id = Just 4, milestone_group_id = Nothing, milestone_description = Just "Assumenda aut placeat expedita exercitationem labore sunt enim earum.", milestone_state = Just MSClosed, milestone_due_date = Nothing, milestone_iid = Just 1, milestone_created_at = Just (read "2015-02-02 19:49:26.013 UTC"), milestone_title = "v2.0", milestone_id = 5, milestone_updated_at = Just (read "2015-02-02 19:49:26.013 UTC"), milestone_web_url = Nothing}),
      merge_request_merge_when_pipeline_succeeds = True,
      merge_request_merge_status = "can_be_merged",
      merge_request_sha = "8888888888888888888888888888888888888888",
      merge_request_merge_commit_sha = Nothing,
      merge_request_squash_commit_sha = Nothing,
      merge_request_user_notes_count = 1,
      merge_request_discussion_locked = Just False,
      merge_request_should_remove_source_branch = Just True,
      merge_request_force_remove_source_branch = Just False,
      merge_request_allow_collaboration = Nothing,
      merge_request_allow_maintainer_to_push = Nothing,
      merge_request_web_url = "http://gitlab.example.com/my-group/my-project/merge_requests/1",
      merge_request_time_stats = Just (TimeStats {time_estimate = 0, total_time_spent = 0, human_time_estimate = Nothing, human_total_time_spent = Nothing}),
      merge_request_squash = False,
      merge_request_changes_count = Just "1",
      merge_request_pipeline = Nothing,
      merge_request_diverged_commits_count = Nothing,
      merge_request_rebase_in_progress = Nothing,
      merge_request_has_conflicts = Nothing,
      merge_request_blocking_discussions_resolved = Nothing,
      merge_request_approvals_before_merge = Nothing,
      merge_request_draft =
        Just False,
      merge_request_subscribed = Just True
    }

subscribeMergeRequest :: MergeRequest
subscribeMergeRequest =
  MergeRequest
    { merge_request_id = 1,
      merge_request_iid = 1,
      merge_request_project_id = 3,
      merge_request_title = "test1",
      merge_request_description = "fixed login page css paddings",
      merge_request_state = "merged",
      merge_request_merged_by = Just (User {user_id = 87854, user_username = "DouweM", user_name = "Douwe Maan", user_state = "active", user_avatar_uri = Just "https://gitlab.example.com/uploads/-/system/user/avatar/87854/avatar.png", user_web_url = Just "https://gitlab.com/DouweM"}),
      merge_request_merged_at = Just (read "2018-09-07 11:16:17.52 UTC"),
      merge_request_closed_by = Nothing,
      merge_request_closed_at = Nothing,
      merge_request_created_at = read "2017-04-29 08:46:00 UTC",
      merge_request_updated_at = read "2017-04-29 08:46:00 UTC",
      merge_request_target_branch = "master",
      merge_request_source_branch = "test1",
      merge_request_upvotes = 0,
      merge_request_downvotes = 0,
      merge_request_author = User {user_id = 1, user_username = "admin", user_name = "Administrator", user_state = "active", user_avatar_uri = Nothing, user_web_url = Just "https://gitlab.example.com/admin"},
      merge_request_assignee = Just (User {user_id = 1, user_username = "admin", user_name = "Administrator", user_state = "active", user_avatar_uri = Nothing, user_web_url = Just "https://gitlab.example.com/admin"}),
      merge_request_assignees = Just [User {user_id = 12, user_username = "axel.block", user_name = "Miss Monserrate Beier", user_state = "active", user_avatar_uri = Just "http://www.gravatar.com/avatar/46f6f7dc858ada7be1853f7fb96e81da?s=80&d=identicon", user_web_url = Just "https://gitlab.example.com/axel.block"}],
      merge_request_reviewers = Just [User {user_id = 12, user_username = "axel.block", user_name = "Miss Monserrate Beier", user_state = "active", user_avatar_uri = Just "http://www.gravatar.com/avatar/46f6f7dc858ada7be1853f7fb96e81da?s=80&d=identicon", user_web_url = Just "https://gitlab.example.com/axel.block"}],
      merge_request_source_project_id = 2,
      merge_request_target_project_id = 3,
      merge_request_labels = ["Community contribution", "Manage"],
      merge_request_work_in_progress = False,
      merge_request_milestone = Just (Milestone {milestone_project_id = Just 3, milestone_group_id = Nothing, milestone_description = Just "Assumenda aut placeat expedita exercitationem labore sunt enim earum.", milestone_state = Just MSClosed, milestone_due_date = Just "2018-09-22", milestone_iid = Just 1, milestone_created_at = Just (read "2015-02-02 19:49:26.013 UTC"), milestone_title = "v2.0", milestone_id = 5, milestone_updated_at = Just (read "2015-02-02 19:49:26.013 UTC"), milestone_web_url = Just "https://gitlab.example.com/my-group/my-project/milestones/1"}),
      merge_request_merge_when_pipeline_succeeds = True,
      merge_request_merge_status = "can_be_merged",
      merge_request_sha = "8888888888888888888888888888888888888888",
      merge_request_merge_commit_sha = Nothing,
      merge_request_squash_commit_sha = Nothing,
      merge_request_user_notes_count = 1,
      merge_request_discussion_locked = Nothing,
      merge_request_should_remove_source_branch = Just True,
      merge_request_force_remove_source_branch = Just False,
      merge_request_allow_collaboration = Just False,
      merge_request_allow_maintainer_to_push = Just False,
      merge_request_web_url = "http://gitlab.example.com/my-group/my-project/merge_requests/1",
      merge_request_time_stats = Just (TimeStats {time_estimate = 0, total_time_spent = 0, human_time_estimate = Nothing, human_total_time_spent = Nothing}),
      merge_request_squash = False,
      merge_request_changes_count = Just "1",
      merge_request_pipeline = Just (Pipeline {pipeline_id = 29626725, sha = "2be7ddb704c7b6b83732fdd5b9f09d5a397b5f8f", pipeline_ref = "patch-28", pipeline_status = "success", pipeline_web_url = Just "https://gitlab.example.com/my-group/my-project/pipelines/29626725"}),
      merge_request_diverged_commits_count = Just 2,
      merge_request_rebase_in_progress = Nothing,
      merge_request_has_conflicts = Nothing,
      merge_request_blocking_discussions_resolved = Nothing,
      merge_request_approvals_before_merge = Nothing,
      merge_request_draft =
        Just False,
      merge_request_subscribed = Just False
    }

unsubscribeMergeRequest :: MergeRequest
unsubscribeMergeRequest =
  MergeRequest
    { merge_request_id = 1,
      merge_request_iid = 1,
      merge_request_project_id = 3,
      merge_request_title = "test1",
      merge_request_description = "fixed login page css paddings",
      merge_request_state = "merged",
      merge_request_merged_by = Just (User {user_id = 87854, user_username = "DouweM", user_name = "Douwe Maan", user_state = "active", user_avatar_uri = Just "https://gitlab.example.com/uploads/-/system/user/avatar/87854/avatar.png", user_web_url = Just "https://gitlab.com/DouweM"}),
      merge_request_merged_at = Just (read "2018-09-07 11:16:17.52 UTC"),
      merge_request_closed_by = Nothing,
      merge_request_closed_at = Nothing,
      merge_request_created_at = read "2017-04-29 08:46:00 UTC",
      merge_request_updated_at = read "2017-04-29 08:46:00 UTC",
      merge_request_target_branch = "master",
      merge_request_source_branch = "test1",
      merge_request_upvotes = 0,
      merge_request_downvotes = 0,
      merge_request_author = User {user_id = 1, user_username = "admin", user_name = "Administrator", user_state = "active", user_avatar_uri = Nothing, user_web_url = Just "https://gitlab.example.com/admin"},
      merge_request_assignee = Just (User {user_id = 1, user_username = "admin", user_name = "Administrator", user_state = "active", user_avatar_uri = Nothing, user_web_url = Just "https://gitlab.example.com/admin"}),
      merge_request_assignees = Just [User {user_id = 12, user_username = "axel.block", user_name = "Miss Monserrate Beier", user_state = "active", user_avatar_uri = Just "http://www.gravatar.com/avatar/46f6f7dc858ada7be1853f7fb96e81da?s=80&d=identicon", user_web_url = Just "https://gitlab.example.com/axel.block"}],
      merge_request_reviewers = Just [User {user_id = 12, user_username = "axel.block", user_name = "Miss Monserrate Beier", user_state = "active", user_avatar_uri = Just "http://www.gravatar.com/avatar/46f6f7dc858ada7be1853f7fb96e81da?s=80&d=identicon", user_web_url = Just "https://gitlab.example.com/axel.block"}],
      merge_request_source_project_id = 2,
      merge_request_target_project_id = 3,
      merge_request_labels = ["Community contribution", "Manage"],
      merge_request_work_in_progress = False,
      merge_request_milestone = Just (Milestone {milestone_project_id = Just 3, milestone_group_id = Nothing, milestone_description = Just "Assumenda aut placeat expedita exercitationem labore sunt enim earum.", milestone_state = Just MSClosed, milestone_due_date = Just "2018-09-22", milestone_iid = Just 1, milestone_created_at = Just (read "2015-02-02 19:49:26.013 UTC"), milestone_title = "v2.0", milestone_id = 5, milestone_updated_at = Just (read "2015-02-02 19:49:26.013 UTC"), milestone_web_url = Just "https://gitlab.example.com/my-group/my-project/milestones/1"}),
      merge_request_merge_when_pipeline_succeeds = True,
      merge_request_merge_status = "can_be_merged",
      merge_request_sha = "8888888888888888888888888888888888888888",
      merge_request_merge_commit_sha = Nothing,
      merge_request_squash_commit_sha = Nothing,
      merge_request_user_notes_count = 1,
      merge_request_discussion_locked = Nothing,
      merge_request_should_remove_source_branch = Just True,
      merge_request_force_remove_source_branch = Just False,
      merge_request_allow_collaboration = Just False,
      merge_request_allow_maintainer_to_push = Just False,
      merge_request_web_url = "http://gitlab.example.com/my-group/my-project/merge_requests/1",
      merge_request_time_stats = Just (TimeStats {time_estimate = 0, total_time_spent = 0, human_time_estimate = Nothing, human_total_time_spent = Nothing}),
      merge_request_squash = False,
      merge_request_changes_count = Just "1",
      merge_request_pipeline = Just (Pipeline {pipeline_id = 29626725, sha = "2be7ddb704c7b6b83732fdd5b9f09d5a397b5f8f", pipeline_ref = "patch-28", pipeline_status = "success", pipeline_web_url = Just "https://gitlab.example.com/my-group/my-project/pipelines/29626725"}),
      merge_request_diverged_commits_count = Just 2,
      merge_request_rebase_in_progress = Nothing,
      merge_request_has_conflicts = Nothing,
      merge_request_blocking_discussions_resolved = Nothing,
      merge_request_approvals_before_merge = Nothing,
      merge_request_draft =
        Just False,
      merge_request_subscribed = Just False
    }

updateMergeRequest :: MergeRequest
updateMergeRequest =
  MergeRequest {merge_request_id = 1, merge_request_iid = 1, merge_request_project_id = 3, merge_request_title = "test1", merge_request_description = "fixed login page css paddings", merge_request_state = "merged", merge_request_merged_by = Just (User {user_id = 87854, user_username = "DouweM", user_name = "Douwe Maan", user_state = "active", user_avatar_uri = Just "https://gitlab.example.com/uploads/-/system/user/avatar/87854/avatar.png", user_web_url = Just "https://gitlab.com/DouweM"}), merge_request_merged_at = Just (read "2018-09-07 11:16:17.52 UTC"), merge_request_closed_by = Nothing, merge_request_closed_at = Nothing, merge_request_created_at = read "2017-04-29 08:46:00 UTC", merge_request_updated_at = read "2017-04-29 08:46:00 UTC", merge_request_target_branch = "master", merge_request_source_branch = "test1", merge_request_upvotes = 0, merge_request_downvotes = 0, merge_request_author = User {user_id = 1, user_username = "admin", user_name = "Administrator", user_state = "active", user_avatar_uri = Nothing, user_web_url = Just "https://gitlab.example.com/admin"}, merge_request_assignee = Just (User {user_id = 1, user_username = "admin", user_name = "Administrator", user_state = "active", user_avatar_uri = Nothing, user_web_url = Just "https://gitlab.example.com/admin"}), merge_request_assignees = Just [User {user_id = 12, user_username = "axel.block", user_name = "Miss Monserrate Beier", user_state = "active", user_avatar_uri = Just "http://www.gravatar.com/avatar/46f6f7dc858ada7be1853f7fb96e81da?s=80&d=identicon", user_web_url = Just "https://gitlab.example.com/axel.block"}], merge_request_reviewers = Just [User {user_id = 12, user_username = "axel.block", user_name = "Miss Monserrate Beier", user_state = "active", user_avatar_uri = Just "http://www.gravatar.com/avatar/46f6f7dc858ada7be1853f7fb96e81da?s=80&d=identicon", user_web_url = Just "https://gitlab.example.com/axel.block"}], merge_request_source_project_id = 2, merge_request_target_project_id = 3, merge_request_labels = ["Community contribution", "Manage"], merge_request_work_in_progress = False, merge_request_milestone = Just (Milestone {milestone_project_id = Just 3, milestone_group_id = Nothing, milestone_description = Just "Assumenda aut placeat expedita exercitationem labore sunt enim earum.", milestone_state = Just MSClosed, milestone_due_date = Just "2018-09-22", milestone_iid = Just 1, milestone_created_at = Just (read "2015-02-02 19:49:26.013 UTC"), milestone_title = "v2.0", milestone_id = 5, milestone_updated_at = Just (read "2015-02-02 19:49:26.013 UTC"), milestone_web_url = Just "https://gitlab.example.com/my-group/my-project/milestones/1"}), merge_request_merge_when_pipeline_succeeds = True, merge_request_merge_status = "can_be_merged", merge_request_sha = "8888888888888888888888888888888888888888", merge_request_merge_commit_sha = Nothing, merge_request_squash_commit_sha = Nothing, merge_request_user_notes_count = 1, merge_request_discussion_locked = Nothing, merge_request_should_remove_source_branch = Just True, merge_request_force_remove_source_branch = Just False, merge_request_allow_collaboration = Just False, merge_request_allow_maintainer_to_push = Just False, merge_request_web_url = "http://gitlab.example.com/my-group/my-project/merge_requests/1", merge_request_time_stats = Just (TimeStats {time_estimate = 0, total_time_spent = 0, human_time_estimate = Nothing, human_total_time_spent = Nothing}), merge_request_squash = False, merge_request_changes_count = Just "1", merge_request_pipeline = Just (Pipeline {pipeline_id = 29626725, sha = "2be7ddb704c7b6b83732fdd5b9f09d5a397b5f8f", pipeline_ref = "patch-28", pipeline_status = "success", pipeline_web_url = Just "https://gitlab.example.com/my-group/my-project/pipelines/29626725"}), merge_request_diverged_commits_count = Just 2, merge_request_rebase_in_progress = Nothing, merge_request_has_conflicts = Nothing, merge_request_blocking_discussions_resolved = Nothing, merge_request_approvals_before_merge = Nothing, merge_request_draft = Just False, merge_request_subscribed = Just False}
