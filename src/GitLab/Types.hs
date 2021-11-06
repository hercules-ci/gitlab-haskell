{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : GitLab.Types
-- Description : Haskell records corresponding to JSON data from GitLab API calls
-- Copyright   : (c) Rob Stewart, Heriot-Watt University, 2019
-- License     : BSD3
-- Maintainer  : robstewart57@gmail.com
-- Stability   : stable
module GitLab.Types
  ( GitLab,
    GitLabState (..),
    GitLabServerConfig (..),
    defaultGitLabServer,
    ArchiveFormat (..),
    Member (..),
    Namespace (..),
    Links (..),
    Owner (..),
    Permissions (..),
    ProjectId,
    Project (..),
    Statistics (..),
    User (..),
    Milestone (..),
    MilestoneState (..),
    TimeStats (..),
    IssueId,
    Issue (..),
    Pipeline (..),
    Commit (..),
    CommitTodo (..),
    CommitStats (..),
    Tag (..),
    Release (..),
    Diff (..),
    Repository (..),
    Job (..),
    Artifact (..),
    Group (..),
    GroupShare (..),
    Branch (..),
    RepositoryFile (..),
    MergeRequest (..),
    Todo (..),
    TodoProject (..),
    TodoAction (..),
    TodoTarget (..),
    TodoTargetType (..),
    TodoState (..),
    Version (..),
    URL,
    EditIssueReq (..),
    Discussion (..),
    CommitNote (..),
    Note (..),
    IssueStatistics (..),
    IssueStats (..),
    IssueCounts (..),
    IssueBoard (..),
    BoardIssue (..),
    BoardIssueLabel (..),
    Visibility (..),
    TestReport (..),
    TestSuite (..),
    TestCase (..),
    TimeEstimate (..),
    TaskCompletionStatus (..),
    References (..),
    Change (..),
    DiffRefs (..),
    DetailedStatus (..),
    License (..),
    ExpirationPolicy (..),
    RepositoryStorage (..),
    Starrer (..),
    ProjectAvatar (..),
  )
where

import Control.Monad.Trans.Reader
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock
import GHC.Generics
import Network.HTTP.Conduit

-- | type synonym for all GitLab actions.
type GitLab a = ReaderT GitLabState IO a

-- | state used by GitLab actions, used internally.
data GitLabState = GitLabState
  { serverCfg :: GitLabServerConfig,
    httpManager :: Manager
  }

-- | configuration data specific to a GitLab server.
data GitLabServerConfig = GitLabServerConfig
  { url :: Text,
    -- | personal access token, see <https://docs.gitlab.com/ee/user/profile/personal_access_tokens.html>
    token :: Text,
    -- | milliseconds
    timeout :: Int,
    -- | how many times to retry a HTTP request before giving up and returning an error.
    retries :: Int,
    -- | write system hook events to files in the system temporary
    -- directory.
    debugSystemHooks :: Bool
  }

-- | default settings, the 'url' and 'token' values will need to be overwritten.
defaultGitLabServer :: GitLabServerConfig
defaultGitLabServer =
  GitLabServerConfig
    { url = "https://gitlab.com",
      token = "",
      timeout = 15000000, -- 15 seconds
      retries = 5,
      debugSystemHooks = False
    }

-- https://docs.gitlab.com/ee/api/repositories.html#get-file-archive
-- tar.gz, tar.bz2, tbz, tbz2, tb2, bz2, tar, and zip

-- | archive format for file archives of repositories.
-- See 'GitLab.API.Repositories.getFileArchive' in 'GitLab.API.Repositories'.
data ArchiveFormat
  = -- | ".tar.gz"
    TarGz
  | -- | ".tar.bz2"
    TarBz2
  | -- | ".tbz"
    Tbz
  | -- | ".tbz2"
    Tbz2
  | -- | ".tb2"
    Tb2
  | -- | ".bz2"
    Bz2
  | -- | ".tar"
    Tar
  | -- | ".zip"
    Zip

instance Show ArchiveFormat where
  show TarGz = ".tar.gz"
  show TarBz2 = ".tar.bz2"
  show Tbz = ".tbz"
  show Tbz2 = ".tbz2"
  show Tb2 = ".tb2"
  show Bz2 = ".bz2"
  show Tar = ".tar"
  show Zip = ".zip"

-- | member of a project.
data Member = Member
  { member_id :: Int,
    member_name :: Text,
    member_username :: Text,
    member_state :: Text,
    member_avatar_uri :: Maybe Text,
    member_web_url :: Maybe Text,
    member_access_level :: Int,
    member_expires_at :: Maybe Text
  }
  deriving (Show, Eq)

-- | namespaces.
data Namespace = Namespace
  { namespace_id :: Int,
    namespace_name :: Text,
    namespace_path :: Text,
    namespace_kind :: Text,
    namespace_full_path :: Text,
    namespace_avatar_url :: Maybe Text,
    namespace_web_url :: Maybe Text,
    namespace_parent_id :: Maybe Int
  }
  deriving (Show, Eq)

-- | links.
data Links = Links
  { links_self :: Text,
    links_issues :: Maybe Text,
    links_merge_requests :: Maybe Text,
    links_repo_branches :: Text,
    links_labels :: Text,
    links_events :: Text,
    links_members :: Text
  }
  deriving (Show, Eq)

-- | owners.
data Owner = Ownwer
  { owner_id :: Int,
    owner_name :: Text,
    owner_username :: Maybe Text,
    owner_state :: Maybe Text,
    owner_avatar_url :: Maybe Text,
    owner_web_url :: Maybe Text,
    owner_created_at :: Maybe UTCTime
  }
  deriving (Show, Eq)

-- | permissions.
data Permissions = Permissions
  { permissions_project_access :: Maybe Object,
    permissions_group_access :: Maybe Object
  }
  deriving (Show, Eq)

-- | projects.
data Project = Project
  { project_id :: Int,
    project_description :: Maybe Text,
    project_name :: Text,
    project_name_with_namespace :: Text,
    project_path :: Text,
    project_path_with_namespace :: Text,
    project_created_at :: Maybe UTCTime,
    project_default_branch :: Maybe Text,
    project_tag_list :: Maybe [Text],
    project_topics :: Maybe [Text],
    project_ssh_url_to_repo :: Maybe Text,
    project_http_url_to_repo :: Maybe Text,
    project_web_url :: Text,
    project_readme_url :: Maybe Text, -- check
    project_avatar_url :: Maybe Text,
    project_license_url :: Maybe Text,
    project_license :: Maybe License,
    project_star_count :: Maybe Int,
    project_runners_token :: Maybe Text, -- "b8547b1dc37721d05889db52fa2f02"
    project_ci_default_git_depth :: Maybe Int,
    project_ci_forward_deployment_enabled :: Maybe Bool,
    project_forks_count :: Maybe Int,
    project_last_activity_at :: Maybe UTCTime,
    project_namespace :: Maybe Namespace,
    project_archived :: Maybe Bool,
    project_visibility :: Maybe Text,
    project_owner :: Maybe Owner,
    project_resolve_outdated_diff_discussions :: Maybe Bool,
    project_container_registry_enabled :: Maybe Bool,
    project_container_registry_access_level :: Maybe Text, -- TODO
    project_container_expiration_policy :: Maybe ExpirationPolicy,
    -- type for "disabled"
    project_issues_enabled :: Maybe Bool,
    project_merge_requests_enabled :: Maybe Bool,
    project_wiki_enabled :: Maybe Bool,
    project_jobs_enabled :: Maybe Bool,
    project_snippets_enabled :: Maybe Bool,
    project_can_create_merge_request_in :: Maybe Bool,
    project_issues_access_level :: Maybe Text, -- TODO a type for "enabled"
    project_repository_access_level :: Maybe Text, -- TODO a type for "enabled"
    project_merge_requests_access_level :: Maybe Text, -- TODO a type for "enabled"
    project_forking_access_level :: Maybe Text, -- TODO a type for "enabled"
    project_analytics_access_level :: Maybe Text, -- TODO a type for "enabled"
    project_wiki_access_level :: Maybe Text, -- TODO a type for "enabled"
    project_builds_access_level :: Maybe Text, -- TODO a type for "enabled"
    project_snippets_access_level :: Maybe Text, -- TODO a type for "enabled"
    project_pages_access_level :: Maybe Text, -- TODO a type for "enabled"
    project_emails_disabled :: Maybe Bool, -- check
    project_shared_runners_enabled :: Maybe Bool,
    project_lfs_enabled :: Maybe Bool,
    project_creator_id :: Maybe Int,
    project_forked_from_project :: Maybe Project,
    project_import_status :: Maybe String,
    project_open_issues_count :: Maybe Int,
    project_public_jobs :: Maybe Bool,
    project_build_timeout :: Maybe Int,
    project_auto_cancel_pending_pipelines :: Maybe Text, -- TODO a type for "enabled"
    project_ci_config_path :: Maybe Text, -- check null
    project_shared_with_groups :: Maybe [Object],
    project_only_allow_merge_if_pipeline_succeeds :: Maybe Bool,
    project_allow_merge_on_skipped_pipeline :: Maybe Bool,
    project_restrict_user_defined_variables :: Maybe Bool,
    project_request_access_enabled :: Maybe Bool,
    project_only_allow_merge_if_all_discussions_are_resolved :: Maybe Bool,
    project_remove_source_branch_after_merge :: Maybe Bool,
    project_printing_merge_requests_link_enabled :: Maybe Bool,
    project_merge_method :: Maybe Text,
    project_squash_option :: Maybe Text, -- TODO type for "default_on"
    project_autoclose_referenced_issues :: Maybe Bool,
    project_suggestion_commit_message :: Maybe Text,
    project_marked_for_deletion_at :: Maybe Text, -- TODO "2020-04-03"
    project_marked_for_deletion_on :: Maybe Text, -- TODO "2020-04-03"
    project_compliance_frameworks :: Maybe [Text],
    project_statistics :: Maybe Statistics,
    project_permissions :: Maybe Permissions,
    project_container_registry_image_prefix :: Maybe Text,
    project__links :: Maybe Links,
    project_mirror :: Maybe Bool,
    project_mirror_overwrites_diverged_branches :: Maybe Bool,
    project_mirror_trigger_builds :: Maybe Bool,
    project_auto_devops_deploy_strategy :: Maybe Text,
    project_auto_devops_enabled :: Maybe Bool,
    project_service_desk_enabled :: Maybe Bool,
    project_approvals_before_merge :: Maybe Int,
    project_mirror_user_id :: Maybe Int,
    project_packages_enabled :: Maybe Bool,
    project_empty_repo :: Maybe Bool,
    project_only_mirror_protected_branches :: Maybe Bool,
    project_repository_storage :: Maybe Text -- TODO type for "default"
  }
  deriving (Show, Eq)

data License = License
  { license_key :: Maybe Text,
    license_name :: Maybe Text,
    license_nickname :: Maybe Text,
    license_html_url :: Maybe Text,
    license_source_url :: Maybe Text
  }
  deriving (Show, Eq)

data ExpirationPolicy = ExpirationPolicy
  { expiration_policy_cadence :: Maybe Text,
    expiration_policy_enabled :: Maybe Bool,
    expiration_policy_keep_n :: Maybe Object, -- TODO
    expiration_policy_older_than :: Maybe Object, -- TODO
    expiration_policy_name_regex :: Maybe Object, -- TODO
    expiration_policy_name_regex_delete :: Maybe Object, -- TODO
    expiration_policy_name_regex_keep :: Maybe Object, -- TODO
    expiration_policy_next_run_at :: Maybe UTCTime
  }
  deriving (Show, Eq)

data RepositoryStorage = RepositoryStorage
  { repository_storage_project_id :: Int,
    repository_storage_disk_path :: Maybe Text,
    repository_storage_created_at :: Maybe UTCTime,
    repository_storage_repository_storage :: Maybe Text
  }
  deriving (Show, Eq)

-- | project statistics.
data Statistics = Statistics
  { statistics_commit_count :: Int,
    statistics_storage_size :: Int,
    statistics_repository_size :: Int,
    statistics_wiki_size :: Maybe Int,
    statistics_lfs_objects_size :: Maybe Int,
    statistics_job_artifacts_size :: Maybe Int,
    statistics_packages_size :: Maybe Int,
    statistics_uploads_size :: Maybe Int,
    statistics_snippets_size :: Maybe Int,
    statistics_pipeline_artifacts_size :: Maybe Int
  }
  deriving (Show, Eq)

-- | registered users.
data User = User
  { user_id :: Int,
    user_username :: Text,
    user_name :: Text,
    user_email :: Maybe Text,
    user_state :: Text,
    user_avatar_url :: Maybe Text,
    user_web_url :: Maybe Text,
    user_discussion_locked :: Maybe Bool, -- only for author of 'TODO' type
    user_created_at :: Maybe UTCTime
  }
  deriving (Show, Eq)

-- | milestone state.
data MilestoneState
  = MSActive
  | MSClosed
  deriving (Show, Eq)

instance FromJSON MilestoneState where
  parseJSON (String "active") = return MSActive
  parseJSON (String "closed") = return MSClosed
  parseJSON x = unexpected x

-- | milestones.
data Milestone = Milestone
  { milestone_project_id :: Maybe Int,
    milestone_group_id :: Maybe Int,
    milestone_description :: Maybe Text,
    milestone_state :: Maybe MilestoneState,
    milestone_due_date :: Maybe Text,
    milestone_start_date :: Maybe Text,
    milestone_iid :: Maybe Int,
    milestone_created_at :: Maybe UTCTime,
    milestone_title :: Text,
    milestone_id :: Int,
    milestone_updated_at :: Maybe UTCTime,
    milestone_web_url :: Maybe URL
  }
  deriving (Show, Eq)

-- instance FromJSON Milestone where
--   parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = drop 10})

-- | time stats.
data TimeStats = TimeStats
  { time_stats_time_estimate :: Int,
    time_stats_total_time_spent :: Int,
    time_stats_human_time_estimate :: Maybe Int,
    time_stats_human_total_time_spent :: Maybe Int
  }
  deriving (Show, Eq)

-- | alias for project id
type ProjectId = Int

-- | alias for issue id
type IssueId = Int

-- | project issues.
data Issue = Issue
  { issue_state :: Text,
    issue_description :: Maybe Text,
    issue_author :: User,
    issue_milestone :: Maybe Milestone,
    issue_project_id :: ProjectId,
    issue_assignees :: Maybe [User],
    issue_assignee :: Maybe User,
    issue_updated_at :: UTCTime,
    issue_closed_at :: Maybe Text,
    issue_closed_by :: Maybe User,
    issue_id :: IssueId,
    issue_title :: Text,
    issue_created_at :: UTCTime,
    issue_iid :: Int,
    issue_labels :: [Text],
    issue_upvotes :: Int,
    issue_downvotes :: Int,
    issue_user_notes_count :: Int,
    issue_due_date :: Maybe Text,
    issue_web_url :: Text,
    issue_confidential :: Bool,
    issue_weight :: Maybe Text, -- Int?
    issue_discussion_locked :: Maybe Bool,
    issue_time_stats :: Maybe TimeStats
  }
  deriving (Show, Eq)

-- | project pipelines
data Pipeline = Pipeline
  { pipeline_id :: Int,
    pipeline_sha :: Text,
    pipeline_ref :: Text,
    pipeline_status :: Text,
    pipeline_web_url :: Maybe Text,
    pipeline_before_sha :: Maybe Text,
    pipeline_tag :: Maybe Bool,
    pipeline_yaml_errors :: Maybe Text,
    pipeline_user :: Maybe User,
    pipeline_created_at :: Maybe UTCTime,
    pipeline_updated_at :: Maybe UTCTime,
    pipeline_started_at :: Maybe UTCTime,
    pipeline_finished_at :: Maybe UTCTime,
    pipelined_committed_at :: Maybe UTCTime,
    pipeline_duration :: Maybe Int,
    pipeline_detailed_status :: Maybe DetailedStatus
  }
  deriving (Show, Eq)

-- | project pipelines
data DetailedStatus = DetailedStatus
  { detailed_status_icon :: Maybe Text, -- "status_pending"
    detailed_status_text :: Maybe Text,
    detailed_status_label :: Maybe Text,
    detailed_status_group :: Maybe Text,
    detailed_status_tooltip :: Maybe Text,
    detailed_status_has_details :: Maybe Bool,
    detailed_status_details_path :: Maybe Text,
    detailed_status_illustration :: Maybe Text,
    detailed_status_favicon :: Maybe Text
  }
  deriving (Show, Eq)

-- | code commits.
data Commit = Commit
  { commit_id :: Text,
    commit_short_id :: Text,
    commit_title :: Text,
    commit_author_name :: Text,
    commit_author_email :: Text,
    commit_authored_date :: Maybe Text, -- ZonedTime ?
    commit_committer_name :: Text,
    commit_committer_email :: Text,
    commit_committed_date :: Maybe Text, -- ZonedTime ?
    commit_created_at :: Maybe Text, -- ZonedTime ?
    commit_message :: Text,
    commit_parent_ids :: Maybe [String],
    commit_last_pipeline :: Maybe Pipeline,
    commit_stats :: Maybe CommitStats,
    commit_status :: Maybe Text,
    commit_web_url :: Maybe Text
  }
  deriving (Show, Eq)

-- | summary of a code commit for TODOs.
data CommitTodo = CommitTodo
  { commit_todo_id :: Text,
    commit_todo_short_id :: Text,
    commit_todo_created_at :: UTCTime,
    commit_todo_parent_ids :: Maybe [String]
  }
  deriving (Show, Eq)

-- | commit stats.
data CommitStats = CommitStats
  { commitstats_additions :: Int,
    commitstats_deletions :: Int,
    commitstats_total :: Int
  }
  deriving (Show, Eq)

-- | tags.
data Tag = Tag
  { tag_commit :: Commit,
    tag_release :: Maybe Release,
    tag_name :: Text,
    tag_target :: Text,
    tag_message :: Maybe Text,
    tag_protected :: Bool
  }
  deriving (Show, Eq)

-- | Release associated with a tag
data Release = Release
  { release_tag_name :: Text,
    release_description :: Text
  }
  deriving (Show, Eq)

-- | diff between two commits.
data Diff = Diff
  { diff_diff :: Text,
    diff_new_path :: Text,
    diff_old_path :: Text,
    diff_a_mode :: Maybe Text,
    diff_b_mode :: Maybe Text,
    diff_new_file :: Bool,
    diff_renamed_file :: Bool,
    diff_deleted_file :: Bool
  }
  deriving (Show, Eq)

-- | repositories.
data Repository = Repository
  { repository_id :: Text,
    repository_name :: Text,
    repository_type :: Text,
    repository_path :: Text,
    mode :: Text
  }
  deriving (Show, Eq)

-- | jobs.
data Job = Job
  { job_commit :: Commit,
    job_coverage :: Maybe Text, -- ?
    job_created_at :: UTCTime,
    job_started_at :: UTCTime,
    job_finished_at :: UTCTime,
    job_duration :: Double,
    job_artifacts_expire_at :: Maybe Text,
    job_id :: Int,
    job_name :: Text,
    job_pipeline :: Pipeline,
    job_ref :: Text,
    job_artifacts :: [Artifact],
    -- , runner :: Maybe Text
    job_stage :: Text,
    job_status :: Text,
    job_tag :: Bool,
    job_web_url :: Text,
    job_user :: User
  }
  deriving (Show, Eq)

-- | artifacts.
data Artifact = Artifact
  { artifact_file_type :: Text,
    artifact_size :: Int,
    artifact_filename :: Text,
    artifact_file_format :: Maybe Text
  }
  deriving (Show, Eq)

-- | groups.
data Group = Group
  { group_id :: Int,
    group_name :: Text,
    group_path :: Maybe Text,
    group_description :: Maybe Text,
    group_visibility :: Maybe Text,
    group_lfs_enabled :: Maybe Bool,
    group_avatar_url :: Maybe Text,
    group_web_url :: Text,
    group_request_access_enabled :: Maybe Bool,
    group_full_name :: Text,
    group_full_path :: Text,
    group_file_template_project_id :: Maybe Int,
    group_parent_id :: Maybe Int
  }
  deriving (Show, Eq)

-- | response to sharing a project with a group.
data GroupShare = GroupShare
  { groupshare_id :: Int,
    groupshare_project_id :: Int,
    groupshare_group_id :: Int,
    groupshare_group_access :: Int,
    groupshare_expires_at :: Maybe Text
  }
  deriving (Show, Eq)

-- | code branches.
data Branch = Branch
  { branch_name :: Text,
    branch_merged :: Bool,
    branch_protected :: Bool,
    branch_default :: Bool,
    branch_developers_can_push :: Bool,
    branch_developers_can_merge :: Bool,
    branch_can_push :: Bool,
    branch_web_url :: Maybe Text,
    branch_commit :: Commit
  }
  deriving (Show, Eq)

-- | files in a repository.
data RepositoryFile = RepositoryFile
  { repository_file_file_name :: Text,
    repository_file_file_path :: Text,
    repository_file_size :: Int,
    encoding :: Text,
    content :: Text,
    content_sha256 :: Text,
    ref :: Text,
    blob_id :: Text,
    repository_file_commit_id :: Text,
    last_commit_id :: Text
  }
  deriving (Generic, Show, Eq)

-- | project merge requests.
data MergeRequest = MergeRequest
  { merge_request_id :: Int,
    merge_request_iid :: Int,
    merge_request_project_id :: Int,
    merge_request_title :: Text,
    merge_request_description :: Text,
    merge_request_state :: Text, -- TODO make a type e.g. 'reopened'
    merge_request_created_at :: UTCTime,
    merge_request_updated_at :: UTCTime,
    merge_request_target_branch :: Text,
    merge_request_source_branch :: Text,
    merge_request_upvotes :: Int,
    merge_request_downvotes :: Int,
    merge_request_author :: User,
    merge_request_assignee :: Maybe User,
    merge_request_assignees :: Maybe [User],
    merge_request_reviewers :: Maybe [User],
    merge_request_source_project_id :: Int,
    merge_request_target_project_id :: Int,
    merge_request_labels :: [Text],
    merge_request_draft :: Maybe Bool,
    merge_request_work_in_progress :: Bool,
    merge_request_milestone :: Maybe Milestone,
    merge_request_merge_when_pipeline_succeeds :: Bool,
    merge_request_merge_status :: Text, -- create type e.g. for "can_be_merged"
    merge_request_merge_error :: Maybe Text,
    merge_request_sha :: Text,
    merge_request_merge_commit_sha :: Maybe Text,
    merge_request_squash_commit_sha :: Maybe Text,
    merge_request_user_notes_count :: Int,
    merge_request_discussion_locked :: Maybe Bool,
    merge_request_should_remove_source_branch :: Maybe Bool,
    merge_request_force_remove_source_branch :: Maybe Bool,
    merge_request_allow_collaboration :: Maybe Bool,
    merge_request_allow_maintainer_to_push :: Maybe Bool,
    merge_request_web_url :: Text,
    merge_request_time_stats :: Maybe TimeStats,
    merge_request_squash :: Bool,
    merge_request_subscribed :: Maybe Bool,
    merge_request_changes_count :: Maybe String,
    merge_request_merged_by :: Maybe User,
    merge_request_merged_at :: Maybe UTCTime,
    merge_request_closed_by :: Maybe User,
    merge_request_closed_at :: Maybe UTCTime,
    merge_request_latest_build_started_at :: Maybe UTCTime,
    merge_request_latest_build_finished_at :: Maybe UTCTime,
    merge_request_first_deployed_to_production_at :: Maybe UTCTime,
    merge_request_pipeline :: Maybe Pipeline,
    merge_request_diverged_commits_count :: Maybe Int,
    merge_request_rebase_in_progress :: Maybe Bool,
    merge_request_first_contribution :: Maybe Bool,
    merge_request_has_conflicts :: Maybe Bool,
    merge_request_blocking_discussions_resolved :: Maybe Bool,
    merge_request_approvals_before_merge :: Maybe Int,
    merge_request_mirror :: Maybe Bool,
    merge_request_task_completion_status :: Maybe TaskCompletionStatus,
    merge_request_references :: Maybe References,
    merge_request_changes :: Maybe [Change],
    merge_request_overflow :: Maybe Bool,
    merge_request_diff_refs :: Maybe DiffRefs
  }
  deriving (Show, Eq)

data TaskCompletionStatus = TaskCompletionStatus
  { task_completion_status_count :: Int,
    task_completion_status_completed_count :: Maybe Int
  }
  deriving (Show, Eq)

data References = References
  { references_short :: Text,
    references_relative :: Text,
    references_full :: Text
  }
  deriving (Show, Eq)

data Change = Change
  { change_old_path :: Text,
    change_new_path :: Text,
    change_a_mode :: Text, -- find type for "100644"
    change_b_mode :: Text, -- find type for "100644"
    change_diff :: Text, -- find type for "--- a/VERSION\\ +++ b/VERSION\\ @@ -1 +1 @@\\ -1.9.7\\ +1.9.8"
    change_new_file :: Bool,
    change_renamed_file :: Bool,
    change_deleted_file :: Bool
  }
  deriving (Show, Eq)

data DiffRefs = DiffRefs
  { diff_refs_base_sha :: Text,
    diff_refs_head_sha :: Text,
    diff_refs_start_sha :: Text
  }
  deriving (Show, Eq)

{- TODO for MergeRequest

  "references": {
    "short": "!1",
    "relative": "!1",
    "full": "my-group/my-project!1"
  },

  "changes": [
    {
    "old_path": "VERSION",
    "new_path": "VERSION",
    "a_mode": "100644",
    "b_mode": "100644",
    "diff": "--- a/VERSION\\ +++ b/VERSION\\ @@ -1 +1 @@\\ -1.9.7\\ +1.9.8",
    "new_file": false,
    "renamed_file": false,
    "deleted_file": false
    }

  "overflow": false

  "diff_refs": {
    "base_sha": "c380d3acebd181f13629a25d2e2acca46ffe1e00",
    "head_sha": "2be7ddb704c7b6b83732fdd5b9f09d5a397b5f8f",
    "start_sha": "c380d3acebd181f13629a25d2e2acca46ffe1e00"
  },

-}

-- | TODO action.
data TodoAction
  = TAAssigned
  | TAMentioned
  | TABuildFailed
  | TAMarked
  | TAApprovalRequired
  | TAUnmergeable
  | TADirectlyAddressed
  deriving (Show, Eq)

instance FromJSON TodoAction where
  parseJSON (String "assigned") = return TAAssigned
  parseJSON (String "mentioned") = return TAMentioned
  parseJSON (String "build_failed") = return TABuildFailed
  parseJSON (String "marked") = return TAMarked
  parseJSON (String "approval_required") = return TAApprovalRequired
  parseJSON (String "unmergeable") = return TAUnmergeable
  parseJSON (String "directly_addressed") = return TADirectlyAddressed
  parseJSON x = unexpected x

-- | TODO targets.
data TodoTarget
  = TTIssue Issue
  | TTMergeRequest MergeRequest
  | TTCommit CommitTodo
  deriving (Show, Eq)

-- | URL is a synonym for 'Text'.
type URL = Text

-- | TODO states.
data TodoState
  = TSPending
  | TSDone
  deriving (Show, Eq)

-- | A project TODO.
data TodoProject = TodoProject
  { todo_project_id :: Int,
    todo_project_description :: Maybe Text,
    todo_project_name :: Text,
    todo_project_name_with_namespace :: Text,
    todo_project_path :: Text,
    todo_project_path_with_namespace :: Text,
    todo_project_created_at :: Maybe UTCTime
  }
  deriving (Show, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "todo_project_"), omitNothingFields = True} ''TodoProject)

-- instance FromJSON TodoProject where
--   parseJSON =
--     genericParseJSON
--       ( defaultOptions
--           { fieldLabelModifier = drop (T.length "todo_project_")
--           }
--       )

-- instance ToJSON TodoProject where
--   toJSON =
--     genericToJSON
--       defaultOptions
--         { fieldLabelModifier = drop (T.length "todo_project_"),
--           omitNothingFields = True
--         }
--   toEncoding =
--     genericToEncoding
--       defaultOptions
--         { fieldLabelModifier = drop (T.length "todo_project_"),
--           omitNothingFields = True
--         }

-- | TODOs.
data Todo = Todo
  { todo_id :: Int,
    todo_project :: TodoProject,
    todo_author :: User,
    todo_action_name :: TodoAction,
    todo_target_type :: TodoTargetType,
    todo_target :: TodoTarget,
    todo_target_url :: URL,
    todo_body :: Text,
    todo_state :: TodoState,
    todo_created_at :: UTCTime
  }
  deriving (Show, Eq)

data TodoTargetType
  = MergeRequestTarget
  | IssueTarget
  | CommitTarget
  deriving (Show, Eq)

-- | version of the GitLab instance.
data Version = Version
  { version :: Text,
    revision :: Text
  }
  deriving (Generic, Show)

-- | An edit issue request.
data EditIssueReq = EditIssueReq
  { edit_issue_id :: ProjectId,
    edit_issue_issue_iid :: IssueId,
    edit_issue_title :: Maybe Text,
    edit_issue_description :: Maybe Text,
    edit_issue_confidential :: Maybe Bool,
    edit_issue_assignee_ids :: Maybe [Int],
    edit_issue_milestone_id :: Maybe Int,
    edit_issue_labels :: Maybe [Text],
    edit_issue_state_event :: Maybe Text,
    edit_issue_updated_at :: Maybe UTCTime,
    edit_issue_due_date :: Maybe Text,
    edit_issue_weight :: Maybe Int,
    edit_issue_discussion_locked :: Maybe Bool,
    edit_issue_epic_id :: Maybe Int,
    edit_issue_epic_iid :: Maybe Int
  }
  deriving (Show)

-- | Discussions https://docs.gitlab.com/ee/api/discussions.html
data Discussion = Discussion
  { discussion_id :: Text,
    discussion_individual_note :: Bool,
    discussion_notes :: [Note]
  }
  deriving (Show, Eq)

data CommitNote = CommitNote
  { commitnote_note :: Text,
    commitnote_author :: User
  }
  deriving (Show, Eq)

-- | Notes
data Note = Note
  { note_id :: Int,
    -- https://docs.gitlab.com/ee/api/discussions.html#list-project-commit-discussion-items
    note_type :: Maybe Text, -- TODO create type for this, e.g. from "DiscussionNote"
    note_body :: Text,
    note_attachment :: Maybe Text,
    note_author :: Owner,
    note_created_at :: UTCTime,
    note_updated_at :: UTCTime,
    note_system :: Bool,
    note_noteable_id :: Maybe Int,
    note_noteable_type :: Maybe Text, -- create type e.g. from "Commit"
    note_noteable_iid :: Maybe Int,
    note_resolved :: Maybe Bool,
    note_resolvable :: Maybe Bool,
    note_resolved_by :: Maybe User -- TODO check
  }
  deriving (Show, Eq)

-- | Statistics and an issue
newtype IssueStatistics = IssueStatistics
  { issues_statistics :: IssueStats
  }
  deriving (Generic, Show)

-- | Issue statistics
newtype IssueStats = IssueStats
  { issues_counts :: IssueCounts
  }
  deriving (Generic, Show)

-- | A count of all, open and closed issues against a project
data IssueCounts = IssueCounts
  { issues_all :: Int,
    issues_closed :: Int,
    issues_opened :: Int
  }
  deriving (Generic, Show)

-- | Project issue boards https://docs.gitlab.com/ee/user/project/issue_board.html
data IssueBoard = IssueBoard
  { board_id :: Int,
    board_name :: Text,
    board_project :: Project,
    board_milestone :: Maybe Milestone,
    board_lists :: [BoardIssue],
    board_group :: Maybe Text, -- not sure, documentation doesn't indicate type
    board_assignee :: Maybe Owner,
    board_labels :: Maybe [BoardIssueLabel],
    board_weight :: Maybe Int
  }
  deriving (Show, Eq)

-- | Issues associated with a project issue board
data BoardIssue = BoardIssue
  { board_issue_id :: Int,
    board_issue_label :: BoardIssueLabel,
    board_issue_position :: Int,
    board_issue_max_issue_count :: Int,
    board_issue_max_issue_weight :: Int,
    -- TODO, the docs don't say what type this should be
    board_issue_limit_metric :: Maybe Int
  }
  deriving (Show, Eq)

-- | Label of an issues for a project issue board
data BoardIssueLabel = BoardIssueLabel
  { board_issue_label_id :: Maybe Int,
    board_issue_label_name :: Text,
    board_issue_label_color :: Text, -- parse into type from e.g. "#F0AD4E"
    board_issue_label_description :: Maybe Text
  }
  deriving (Show, Eq)

-- |  Project visibility.
data Visibility
  = Public
  | Private
  | Internal
  deriving (Show, Eq)

instance FromJSON Visibility where
  parseJSON (String "public") = return Public
  parseJSON (String "private") = return Private
  parseJSON (String "internal") = return Internal
  parseJSON (Number 0) = return Private
  parseJSON (Number 10) = return Internal
  parseJSON (Number 20) = return Public
  parseJSON n = error (show n)

-- | Unit test reports for a CI pipeline https://docs.gitlab.com/ee/ci/unit_test_reports.html
data TestReport = TestReport
  { total_time :: Double,
    total_count :: Int,
    success_count :: Int,
    failed_count :: Int,
    skipped_count :: Int,
    error_count :: Int,
    test_suites :: [TestSuite]
  }
  deriving (Generic, Show, Eq)

-- | Testsuites associated with a test report
data TestSuite = TestSuite
  { testsuite_name :: Text,
    testsuite_total_time :: Double,
    testsuite_success_count :: Int,
    testsuite_failed_count :: Int,
    testsuite_skipped_count :: Int,
    testsuite_error_count :: Int,
    testsuite_test_cases :: [TestCase]
  }
  deriving (Show, Eq)

-- testsuitePrefix :: String -> String
-- testsuitePrefix "testsuite_name" = "name"
-- testsuitePrefix "testsuite_total_time" = "total_time"
-- testsuitePrefix "testsuite_success_count" = "success_count"
-- testsuitePrefix "testsuite_failed_count" = "failed_count"
-- testsuitePrefix "testsuite_skipped_count" = "skipped_count"
-- testsuitePrefix "testsuite_error_count" = "error_count"
-- testsuitePrefix "testsuite_test_cases" = "test_cases"
-- testsuitePrefix s = s

-- | Test case associated with a testsuite
data TestCase = TestCase
  { testcase_status :: Text, -- could turn this into a type e.g. for "success"
    testcase_name :: Text,
    testcase_classname :: Text,
    testcase_execution_time :: Double,
    testcase_system_output :: Maybe Text,
    testcase_stack_trace :: Maybe Text
  }
  deriving (Show, Eq)

-- testcasePrefix :: String -> String
-- testcasePrefix "testcase_status" = "status"
-- testcasePrefix "testcase_name" = "name"
-- testcasePrefix "testcase_classname" = "classname"
-- testcasePrefix "testcase_execution_time" = "execution_time"
-- testcasePrefix "testcase_system_output" = "system_output"
-- testcasePrefix "testcase_stack_trace" = "stack_trace"
-- testcasePrefix s = s

data TimeEstimate = TimeEstimate
  { time_estimate_human_time_estimate :: Maybe Text,
    time_estimate_human_total_time_spent :: Maybe Text,
    time_estimate_time_estimate :: Maybe Int,
    time_estimate_total_time_spent :: Maybe Int
  }
  deriving (Show, Eq)

-----------------------------
-- JSON GitLab parsers below
-----------------------------

bodyNoPrefix :: String -> String
-- bodyNoPrefix "commit_created_at" = "created_at"
-- bodyNoPrefix "commit_id" = "id"
-- bodyNoPrefix "commit_status" = "status"
-- bodyNoPrefix "commit_parent_ids" = "parent_ids"
-- bodyNoPrefix "todo_commit_id" = "id"
-- bodyNoPrefix "todo_commit_short_id" = "short_id"
-- bodyNoPrefix "todo_commit_created_at" = "created_at"
-- bodyNoPrefix "todo_parent_ids" = "parent_ids"
-- bodyNoPrefix "issue_author" = "author"
-- bodyNoPrefix "issue_created_at" = "created_at"
-- bodyNoPrefix "issue_description" = "description"
-- bodyNoPrefix "issue_due_date" = "due_date"
-- bodyNoPrefix "issue_id" = "id"
-- bodyNoPrefix "issue_labels" = "labels"
-- bodyNoPrefix "issue_project_id" = "project_id"
-- bodyNoPrefix "issue_state" = "state"
-- bodyNoPrefix "issue_title" = "title"
-- bodyNoPrefix "issue_web_url" = "web_url"
bodyNoPrefix "link_events" = "events"
bodyNoPrefix "link_labels" = "labels"
-- bodyNoPrefix "member_avatar_url" = "avatar_url"
-- bodyNoPrefix "member_id" = "id"
-- bodyNoPrefix "member_name" = "name"
-- bodyNoPrefix "member_state" = "state"
-- bodyNoPrefix "member_username" = "username"
-- bodyNoPrefix "member_web_url" = "we_url"
-- bodyNoPrefix "namespace_id" = "id"
-- bodyNoPrefix "namespace_name" = "name"
-- bodyNoPrefix "namespace_path" = "path"
-- bodyNoPrefix "owner_avatar_url" = "avatar_url"
-- bodyNoPrefix "owner_id" = "id"
-- bodyNoPrefix "owner_name" = "name"
-- bodyNoPrefix "owner_username" = "username"
-- bodyNoPrefix "owner_web_url" = "web_url"
-- bodyNoPrefix "project_avatar_url" = "avatar_url"
-- bodyNoPrefix "project_created_at" = "created_at"
-- bodyNoPrefix "project_id" = "id"
-- bodyNoPrefix "project_name" = "name"
-- bodyNoPrefix "project_path" = "path"
-- bodyNoPrefix "project_path_with_namespace" = "path_with_namespace"
-- bodyNoPrefix "project_web_url" = "web_url"
-- bodyNoPrefix "repository_id" = "id"
-- bodyNoPrefix "repository_name" = "name"
-- bodyNoPrefix "repository_path" = "path"
-- bodyNoPrefix "repository_type" = "type"
bodyNoPrefix "event_title" = "title"
bodyNoPrefix "event_project_id" = "project_id"
-- bodyNoPrefix "branch_name" = "name"
-- bodyNoPrefix "branch_default" = "default"
-- bodyNoPrefix "branch_commit" = "commit"
bodyNoPrefix "repository_file_file_name" = "file_name"
bodyNoPrefix "repository_file_file_path" = "file_path"
bodyNoPrefix "repository_file_size" = "size"
bodyNoPrefix "repository_file_commit_id" = "commit_id"
-- bodyNoPrefix "project_stats" = "statistics"
bodyNoPrefix "commit_stats" = "stats"
bodyNoPrefix "share_id" = "id"
bodyNoPrefix "share_project_id" = "project_id"
bodyNoPrefix "share_group_id" = "group_id"
bodyNoPrefix "share_group_access" = "group_access"
bodyNoPrefix "share_expires_at" = "expires_at"
-- bodyNoPrefix "group_id" = "id"
-- bodyNoPrefix "group_name" = "name"
-- bodyNoPrefix "group_path" = "path"
-- bodyNoPrefix "group_description" = "description"
-- bodyNoPrefix "group_visibility" = "visibility"
-- bodyNoPrefix "group_lfs_enabled" = "lfs_enabled"
-- bodyNoPrefix "group_avatar_url" = "avatar_url"
-- bodyNoPrefix "group_web_url" = "web_url"
-- bodyNoPrefix "group_request_access_enabled" = "request_access_enabled"
-- bodyNoPrefix "group_full_name" = "full_name"
-- bodyNoPrefix "group_full_path" = "full_path"
-- bodyNoPrefix "group_file_template_project_id" = "file_template_project_id"
-- bodyNoPrefix "group_parent_id" = "parent_id"
-- bodyNoPrefix "job_commit" = "commit"
-- bodyNoPrefix "job_coverage" = "coverage"
-- bodyNoPrefix "job_created_at" = "created_at"
-- bodyNoPrefix "job_started_at" = "started_at"
-- bodyNoPrefix "job_finished_at" = "finished_at"
-- bodyNoPrefix "job_duration" = "duration"
-- bodyNoPrefix "job_artifacts_expire_at" = "artifacts_expire_at"
-- bodyNoPrefix "job_id" = "id"
-- bodyNoPrefix "job_name" = "name"
-- bodyNoPrefix "job_pipeline" = "pipeline"
-- bodyNoPrefix "job_ref" = "ref"
-- bodyNoPrefix "job_artifacts" = "artifacts"
-- bodyNoPrefix "job_stage" = "stage"
-- bodyNoPrefix "job_status" = "status"
-- bodyNoPrefix "job_tag" = "tag"
-- bodyNoPrefix "job_web_url" = "web_url"
-- bodyNoPrefix "job_user" = "user"
-- bodyNoPrefix "discussion_id" = "id"
-- bodyNoPrefix "discussion_individual_note" = "individual_note"
-- bodyNoPrefix "discussion_notes" = "notes"
-- bodyNoPrefix "note_id" = "id"
-- bodyNoPrefix "note_type" = "type"
-- bodyNoPrefix "note_body" = "body"
-- bodyNoPrefix "note_attachment" = "attachment"
-- bodyNoPrefix "note_author" = "author"
-- bodyNoPrefix "note_created_at" = "created_at"
-- bodyNoPrefix "note_updated_at" = "updated_at"
-- bodyNoPrefix "note_system" = "system"
-- bodyNoPrefix "note_noteable_id" = "noteable_id"
-- bodyNoPrefix "note_noteable_type" = "noteable_type"
-- bodyNoPrefix "note_noteable_iid" = "iid"
-- bodyNoPrefix "note_resolvable" = "resolvable"
-- TODO field names for Issues data type
bodyNoPrefix s = s

-- TODO refactor bodyNoPrefix function above into smaller
--    String -> String
-- functions like those below.

-- tagPrefix :: String -> String
-- tagPrefix "tag_commit" = "commit"
-- tagPrefix "tag_release" = "release"
-- tagPrefix "tag_name" = "name"
-- tagPrefix "tag_target" = "target"
-- tagPrefix "tag_message" = "message"
-- tagPrefix "tag_protected" = "protected"
-- tagPrefix s = s

-- releasePrefix :: String -> String
-- releasePrefix "release_tag_name" = "tag_name"
-- releasePrefix "release_description" = "description"
-- releasePrefix s = s

issueStatsPrefix :: String -> String
issueStatsPrefix "issues_all" = "all"
issueStatsPrefix "issues_closed" = "closed"
issueStatsPrefix "issues_opened" = "opened"
issueStatsPrefix "issues_statistics" = "statistics"
issueStatsPrefix "issues_counts" = "counts"
issueStatsPrefix s = s

-- boardsPrefix :: String -> String
-- boardsPrefix "board_id" = "id"
-- boardsPrefix "board_name" = "name"
-- boardsPrefix "board_project" = "project"
-- boardsPrefix "board_milestone" = "milestone"
-- boardsPrefix "board_lists" = "lists"
-- boardsPrefix "board_issue_id" = "id"
-- boardsPrefix "board_issue_label" = "label"
-- boardsPrefix "board_issue_position" = "position"
-- boardsPrefix "board_issue_max_issue_count" = "max_issue_count"
-- boardsPrefix "board_issue_max_issue_weight" = "max_issue_weight"
-- boardsPrefix "board_issue_limit_metric" = "limit_metric"
-- boardsPrefix "board_issue_label_name" = "name"
-- boardsPrefix "board_issue_label_color" = "color"
-- boardsPrefix "board_issue_label_description" = "description"
-- boardsPrefix "project_board_id" = "id"
-- boardsPrefix "project_board_name" = "name"
-- boardsPrefix "project_board_name_with_namespace" = "name_with_namespace"
-- boardsPrefix "project_board_path" = "path"
-- boardsPrefix "project_board_path_with_namespace" = "path_with_namespace"
-- boardsPrefix "project_board_http_url_to_repo" = "http_url_to_repo"
-- boardsPrefix "project_board_web_url" = "web_url"
-- boardsPrefix s = s

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "time_stats_"), omitNothingFields = True} ''TimeStats)

-- instance FromJSON TimeStats where
--   parseJSON =
--     genericParseJSON
--       ( defaultOptions
--           { fieldLabelModifier = drop (T.length "time_stats_")
--           }
--       )

-- instance ToJSON TimeStats where
--   toJSON =
--     genericToJSON
--       defaultOptions
--         { fieldLabelModifier = drop (T.length "time_stats_"),
--           omitNothingFields = True
--         }
--   toEncoding =
--     genericToEncoding
--       defaultOptions
--         { fieldLabelModifier = drop (T.length "time_stats_"),
--           omitNothingFields = True
--         }

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "issue_"), omitNothingFields = True} ''Issue)

-- instance FromJSON Issue where
--   parseJSON =
--     genericParseJSON
--       ( defaultOptions
--           { fieldLabelModifier = drop (T.length "issue_")
--           }
--       )

-- instance ToJSON Issue where
--   toJSON =
--     genericToJSON
--       defaultOptions
--         { fieldLabelModifier = drop (T.length "issue_"),
--           omitNothingFields = True
--         }
--   toEncoding =
--     genericToEncoding
--       defaultOptions
--         { fieldLabelModifier = drop (T.length "issue_"),
--           omitNothingFields = True
--         }

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "user_"), omitNothingFields = True} ''User)

-- instance FromJSON User where
--   parseJSON =
--     genericParseJSON
--       ( defaultOptions
--           { fieldLabelModifier = drop (T.length "user_")
--           }
--       )

-- instance ToJSON User where
--   toJSON =
--     genericToJSON
--       defaultOptions
--         { fieldLabelModifier = drop (T.length "user_"),
--           omitNothingFields = True
--         }
--   toEncoding =
--     genericToEncoding
--       defaultOptions
--         { fieldLabelModifier = drop (T.length "user_"),
--           omitNothingFields = True
--         }

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "commit_"), omitNothingFields = True} ''Commit)

-- instance FromJSON Commit where
--   parseJSON =
--     genericParseJSON
--       ( defaultOptions
--           { fieldLabelModifier = bodyNoPrefix
--           }
--       )

-- instance FromJSON CommitTodo where
--   parseJSON =
--     genericParseJSON
--       ( defaultOptions
--           { fieldLabelModifier = bodyNoPrefix
--           }
--       )

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "tag_"), omitNothingFields = True} ''Tag)

-- instance FromJSON Tag where
--   parseJSON =
--     genericParseJSON
--       ( defaultOptions
--           { fieldLabelModifier = tagPrefix
--           }
--       )

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "release_"), omitNothingFields = True} ''Release)

-- instance FromJSON Release where
--   parseJSON =
--     genericParseJSON
--       ( defaultOptions
--           { fieldLabelModifier = releasePrefix
--           }
--       )

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "commitstats_"), omitNothingFields = True} ''CommitStats)

-- instance FromJSON CommitStats where
--   parseJSON =
--     genericParseJSON
--       ( defaultOptions
--           { fieldLabelModifier = bodyNoPrefix
--           }
--       )

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "pipeline_"), omitNothingFields = True} ''Pipeline)

-- instance FromJSON Pipeline where
--   parseJSON =
--     genericParseJSON
--       ( defaultOptions
--           { fieldLabelModifier = drop (T.length "pipeline_")
--           }
--       )

-- instance ToJSON Pipeline where
--   toJSON =
--     genericToJSON
--       ( defaultOptions
--           { fieldLabelModifier = drop (T.length "pipeline_"),
--             omitNothingFields = True
--           }
--       )
--   toEncoding =
--     genericToEncoding
--       ( defaultOptions
--           { fieldLabelModifier = drop (T.length "pipeline_"),
--             omitNothingFields = True
--           }
--       )

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "member_"), omitNothingFields = True} ''Member)

-- instance FromJSON Member where
--   parseJSON =
--     genericParseJSON
--       ( defaultOptions
--           { fieldLabelModifier = bodyNoPrefix
--           }
--       )

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "permissions_"), omitNothingFields = True} ''Permissions)

-- instance FromJSON Permissions where
--   parseJSON =
--     genericParseJSON
--       ( defaultOptions
--           { fieldLabelModifier = drop (T.length "permissions_")
--           }
--       )

-- instance ToJSON Permissions where
--   toJSON =
--     genericToJSON
--       ( defaultOptions
--           { fieldLabelModifier = drop (T.length "permissions_"),
--             omitNothingFields = True
--           }
--       )
--   toEncoding =
--     genericToEncoding
--       ( defaultOptions
--           { fieldLabelModifier = drop (T.length "permissions_"),
--             omitNothingFields = True
--           }
--       )

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "owner_"), omitNothingFields = True} ''Owner)

-- instance FromJSON Owner where
--   parseJSON =
--     genericParseJSON
--       ( defaultOptions
--           { fieldLabelModifier = drop (T.length "owner_")
--           }
--       )

-- instance ToJSON Owner where
--   toJSON =
--     genericToJSON
--       defaultOptions
--         { fieldLabelModifier = drop (T.length "owner_"),
--           omitNothingFields = True
--         }
--   toEncoding =
--     genericToEncoding
--       defaultOptions
--         { fieldLabelModifier = drop (T.length "owner_"),
--           omitNothingFields = True
--         }

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "links_"), omitNothingFields = True} ''Links)

-- instance FromJSON Links where
--   parseJSON =
--     genericParseJSON
--       ( defaultOptions
--           { fieldLabelModifier = drop (T.length "links_")
--           }
--       )

-- instance ToJSON Links where
--   toJSON =
--     genericToJSON
--       defaultOptions
--         { fieldLabelModifier = drop (T.length "links_"),
--           omitNothingFields = True
--         }
--   toEncoding =
--     genericToEncoding
--       defaultOptions
--         { fieldLabelModifier = drop (T.length "links_"),
--           omitNothingFields = True
--         }

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "namespace_"), omitNothingFields = True} ''Namespace)

-- instance FromJSON Namespace where
--   parseJSON =
--     genericParseJSON
--       ( defaultOptions
--           { fieldLabelModifier = drop (T.length "namespace_")
--           }
--       )

-- instance ToJSON Namespace where
--   toJSON =
--     genericToJSON
--       defaultOptions
--         { fieldLabelModifier = drop (T.length "namespace_"),
--           omitNothingFields = True
--         }
--   toEncoding =
--     genericToEncoding
--       defaultOptions
--         { fieldLabelModifier = drop (T.length "namespace_"),
--           omitNothingFields = True
--         }

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "project_"), omitNothingFields = True} ''Project)

-- instance FromJSON Project where
--   parseJSON =
--     genericParseJSON
--       ( defaultOptions
--           { fieldLabelModifier = drop (T.length "project_")
--           }
--       )

-- instance ToJSON Project where
--   toJSON =
--     genericToJSON
--       defaultOptions
--         { fieldLabelModifier = drop (T.length "project_"),
--           omitNothingFields = True
--         }
--   toEncoding =
--     genericToEncoding
--       defaultOptions
--         { fieldLabelModifier = drop (T.length "project_"),
--           omitNothingFields = True
--         }

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "statistics_"), omitNothingFields = True} ''Statistics)

-- instance FromJSON Statistics where
--   parseJSON =
--     genericParseJSON
--       ( defaultOptions
--           { fieldLabelModifier = drop (T.length "statistics_")
--           }
--       )

-- instance ToJSON Statistics where
--   toJSON =
--     genericToJSON
--       defaultOptions
--         { fieldLabelModifier = drop (T.length "statistics_"),
--           omitNothingFields = True
--         }
--   toEncoding =
--     genericToEncoding
--       defaultOptions
--         { fieldLabelModifier = drop (T.length "statistics_"),
--           omitNothingFields = True
--         }

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "repository_"), omitNothingFields = True} ''Repository)

-- instance FromJSON Repository where
--   parseJSON =
--     genericParseJSON
--       ( defaultOptions
--           { fieldLabelModifier = bodyNoPrefix
--           }
--       )

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "job_"), omitNothingFields = True} ''Job)

-- instance FromJSON Job where
--   parseJSON =
--     genericParseJSON
--       ( defaultOptions
--           { fieldLabelModifier = bodyNoPrefix
--           }
--       )

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "artifact_"), omitNothingFields = True} ''Artifact)

-- instance FromJSON Artifact where
--   parseJSON =
--     genericParseJSON
--       ( defaultOptions
--           { fieldLabelModifier = bodyNoPrefix
--           }
--       )

-- instance FromJSON Group where
--   parseJSON =
--     genericParseJSON
--       ( defaultOptions
--           { fieldLabelModifier = bodyNoPrefix
--           }
--       )

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "groupshare_"), omitNothingFields = True} ''GroupShare)

-- instance FromJSON GroupShare where
--   parseJSON =
--     genericParseJSON
--       ( defaultOptions
--           { fieldLabelModifier = bodyNoPrefix
--           }
--       )

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "branch_"), omitNothingFields = True} ''Branch)

-- instance FromJSON Branch where
--   parseJSON =
--     genericParseJSON
--       ( defaultOptions
--           { fieldLabelModifier = bodyNoPrefix
--           }
--       )

-- instance ToJSON Branch where
--   toJSON =
--     genericToJSON
--       defaultOptions
--         { fieldLabelModifier = bodyNoPrefix,
--           omitNothingFields = True
--         }
--   toEncoding =
--     genericToEncoding
--       defaultOptions
--         { fieldLabelModifier = bodyNoPrefix,
--           omitNothingFields = True
--         }

instance FromJSON RepositoryFile where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "merge_request_"), omitNothingFields = True} ''MergeRequest)

-- instance FromJSON MergeRequest where
--   parseJSON =
--     genericParseJSON
--       ( defaultOptions
--           { fieldLabelModifier = drop (T.length "merge_request_")
--           }
--       )

-- instance ToJSON MergeRequest where
--   toJSON =
--     genericToJSON
--       defaultOptions
--         { fieldLabelModifier = drop (T.length "merge_request_"),
--           omitNothingFields = True
--         }
--   toEncoding =
--     genericToEncoding
--       defaultOptions
--         { fieldLabelModifier = drop (T.length "merge_request_"),
--           omitNothingFields = True
--         }

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "diff_"), omitNothingFields = True} ''Diff)

-- instance FromJSON Diff where
--   parseJSON =
--     genericParseJSON
--       ( defaultOptions
--           { fieldLabelModifier = bodyNoPrefix
--           }
--       )

instance FromJSON Version where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "edit_issue_"), omitNothingFields = True} ''EditIssueReq)

-- instance ToJSON EditIssueReq where
--   toJSON =
--     genericToJSON
--       defaultOptions
--         { fieldLabelModifier = drop (T.length "edit_issue_"),
--           omitNothingFields = True
--         }
--   toEncoding =
--     genericToEncoding
--       defaultOptions
--         { fieldLabelModifier = drop (T.length "edit_issue_"),
--           omitNothingFields = True
--         }

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "discussion_"), omitNothingFields = True} ''Discussion)

-- instance FromJSON Discussion where
--   parseJSON =
--     genericParseJSON
--       ( defaultOptions
--           { fieldLabelModifier = bodyNoPrefix
--           }
--       )

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "note_"), omitNothingFields = True} ''Note)

-- instance FromJSON Note where
--   parseJSON =
--     genericParseJSON
--       ( defaultOptions
--           { fieldLabelModifier = bodyNoPrefix
--           }
--       )

instance FromJSON IssueCounts where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = issueStatsPrefix
          }
      )

instance FromJSON IssueStats where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = issueStatsPrefix
          }
      )

instance FromJSON IssueStatistics where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = issueStatsPrefix
          }
      )

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "board_"), omitNothingFields = True} ''IssueBoard)

-- instance FromJSON IssueBoard where
--   parseJSON =
--     genericParseJSON
--       ( defaultOptions
--           { fieldLabelModifier = drop (T.length "board_")
--           }
--       )

-- instance ToJSON IssueBoard where
--   toJSON =
--     genericToJSON
--       defaultOptions
--         { fieldLabelModifier = drop (T.length "board_"),
--           omitNothingFields = True
--         }
--   toEncoding =
--     genericToEncoding
--       defaultOptions
--         { fieldLabelModifier = drop (T.length "board_"),
--           omitNothingFields = True
--         }

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "board_issue_"), omitNothingFields = True} ''BoardIssue)

-- instance FromJSON BoardIssue where
--   parseJSON =
--     genericParseJSON
--       ( defaultOptions
--           { fieldLabelModifier = drop (T.length "board_issue_")
--           }
--       )

-- instance ToJSON BoardIssue where
--   toJSON =
--     genericToJSON
--       defaultOptions
--         { fieldLabelModifier = drop (T.length "board_issue_"),
--           omitNothingFields = True
--         }
--   toEncoding =
--     genericToEncoding
--       defaultOptions
--         { fieldLabelModifier = drop (T.length "board_issue_"),
--           omitNothingFields = True
--         }

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "board_issue_label_"), omitNothingFields = True} ''BoardIssueLabel)

-- instance FromJSON BoardIssueLabel where
--   parseJSON =
--     genericParseJSON
--       ( defaultOptions
--           { fieldLabelModifier = drop (T.length "board_issue_label_")
--           }
--       )

-- instance ToJSON BoardIssueLabel where
--   toJSON =
--     genericToJSON
--       defaultOptions
--         { fieldLabelModifier = drop (T.length "board_issue_label_"),
--           omitNothingFields = True
--         }
--   toEncoding =
--     genericToEncoding
--       defaultOptions
--         { fieldLabelModifier = drop (T.length "board_issue_label_"),
--           omitNothingFields = True
--         }

instance FromJSON TestReport where
  parseJSON =
    genericParseJSON defaultOptions

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "testsuite_"), omitNothingFields = True} ''TestSuite)

-- instance FromJSON TestSuite where
--   parseJSON =
--     genericParseJSON
--       ( defaultOptions
--           { fieldLabelModifier = testsuitePrefix
--           }
--       )

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "testcase_"), omitNothingFields = True} ''TestCase)

-- instance FromJSON TestCase where
--   parseJSON =
--     genericParseJSON
--       ( defaultOptions
--           { fieldLabelModifier = testcasePrefix
--           }
--       )

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "time_estimate_"), omitNothingFields = True} ''TimeEstimate)

-- instance FromJSON TimeEstimate where
--   parseJSON =
--     genericParseJSON
--       ( defaultOptions
--           { fieldLabelModifier = drop (T.length "time_estimate_")
--           }
--       )

-- instance ToJSON TimeEstimate where
--   toJSON =
--     genericToJSON
--       defaultOptions
--         { fieldLabelModifier = drop (T.length "time_estimate_"),
--           omitNothingFields = True
--         }
--   toEncoding =
--     genericToEncoding
--       defaultOptions
--         { fieldLabelModifier = drop (T.length "time_estimate_"),
--           omitNothingFields = True
--         }

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "milestone_"), omitNothingFields = True} ''Milestone)

-- instance ToJSON Milestone where
--   toJSON =
--     genericToJSON
--       defaultOptions
--         { fieldLabelModifier = drop (T.length "milestone_"),
--           omitNothingFields = True
--         }
--   toEncoding =
--     genericToEncoding
--       defaultOptions
--         { fieldLabelModifier = drop (T.length "milestone_"),
--           omitNothingFields = True
--         }

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "task_completion_status_"), omitNothingFields = True} ''TaskCompletionStatus)

-- instance FromJSON TaskCompletionStatus where
--   parseJSON =
--     genericParseJSON
--       ( defaultOptions
--           { fieldLabelModifier = drop (T.length "task_completion_status_")
--           }
--       )

-- instance ToJSON TaskCompletionStatus where
--   toJSON =
--     genericToJSON
--       defaultOptions
--         { fieldLabelModifier = drop (T.length "task_completion_status_"),
--           omitNothingFields = True
--         }
--   toEncoding =
--     genericToEncoding
--       defaultOptions
--         { fieldLabelModifier = drop (T.length "task_completion_status_"),
--           omitNothingFields = True
--         }

instance ToJSON MilestoneState where
  toJSON MSActive = String "active"
  toJSON MSClosed = String "closed"

instance ToJSON TodoAction where
  toJSON TAAssigned = String "assigned"
  toJSON TAMentioned = String "mentioned"
  toJSON TABuildFailed = String "build_build"
  toJSON TAMarked = String "marked"
  toJSON TAApprovalRequired = String "approval_required"
  toJSON TAUnmergeable = String "unmergeable"
  toJSON TADirectlyAddressed = String "directly_addressed"

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "references_"), omitNothingFields = True} ''References)

-- instance FromJSON References where
--   parseJSON =
--     genericParseJSON
--       ( defaultOptions
--           { fieldLabelModifier = drop (T.length "references_")
--           }
--       )

-- instance ToJSON References where
--   toJSON =
--     genericToJSON
--       defaultOptions
--         { fieldLabelModifier = drop (T.length "references_"),
--           omitNothingFields = True
--         }
--   toEncoding =
--     genericToEncoding
--       defaultOptions
--         { fieldLabelModifier = drop (T.length "references_"),
--           omitNothingFields = True
--         }

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "diff_refs_"), omitNothingFields = True} ''DiffRefs)

-- instance FromJSON DiffRefs where
--   parseJSON =
--     genericParseJSON
--       ( defaultOptions
--           { fieldLabelModifier = drop (T.length "diff_refs_")
--           }
--       )

-- instance ToJSON DiffRefs where
--   toJSON =
--     genericToJSON
--       defaultOptions
--         { fieldLabelModifier = drop (T.length "diff_refs_"),
--           omitNothingFields = True
--         }
--   toEncoding =
--     genericToEncoding
--       defaultOptions
--         { fieldLabelModifier = drop (T.length "diff_refs_"),
--           omitNothingFields = True
--         }

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "change_"), omitNothingFields = True} ''Change)

-- instance FromJSON Change where
--   parseJSON =
--     genericParseJSON
--       ( defaultOptions
--           { fieldLabelModifier = drop (T.length "change_")
--           }
--       )

-- instance ToJSON Change where
--   toJSON =
--     genericToJSON
--       defaultOptions
--         { fieldLabelModifier = drop (T.length "change_"),
--           omitNothingFields = True
--         }
--   toEncoding =
--     genericToEncoding
--       defaultOptions
--         { fieldLabelModifier = drop (T.length "change_"),
--           omitNothingFields = True
--         }

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "detailed_status_"), omitNothingFields = True} ''DetailedStatus)

-- instance FromJSON DetailedStatus where
--   parseJSON =
--     genericParseJSON
--       ( defaultOptions
--           { fieldLabelModifier = drop (T.length "detailed_status_")
--           }
--       )

-- instance ToJSON DetailedStatus where
--   toJSON =
--     genericToJSON
--       defaultOptions
--         { fieldLabelModifier = drop (T.length "detailed_status_"),
--           omitNothingFields = True
--         }
--   toEncoding =
--     genericToEncoding
--       defaultOptions
--         { fieldLabelModifier = drop (T.length "detailed_status_"),
--           omitNothingFields = True
--         }

instance FromJSON Todo where
  parseJSON = withObject "Todo" $ \v ->
    Todo
      <$> v .: "id"
      <*> v .: "project"
      <*> v .: "author"
      <*> v .: "action_name"
      <*> v .: "target_type"
      <*> ( v .: "target_type" >>= \case
              "MergeRequest" -> TTMergeRequest <$> v .: "target"
              "Issue" -> TTIssue <$> v .: "target"
              "Commit" -> TTCommit <$> v .: "target"
              (_ :: Text) -> fail ""
          )
      <*> v .: "target_url"
      <*> v .: "body"
      <*> v .: "state"
      <*> v .: "created_at"

$(deriveToJSON defaultOptions {fieldLabelModifier = drop (T.length "todo_"), omitNothingFields = True} ''Todo)

-- instance ToJSON Todo where
--   toJSON =
--     genericToJSON
--       defaultOptions
--         { fieldLabelModifier = drop (T.length "todo_"),
--           omitNothingFields = True
--         }
--   toEncoding =
--     genericToEncoding
--       defaultOptions
--         { fieldLabelModifier = drop (T.length "todo_"),
--           omitNothingFields = True
--         }

instance FromJSON TodoTargetType where
  parseJSON (String "MergeRequest") = return MergeRequestTarget
  parseJSON (String "Issue") = return IssueTarget
  parseJSON (String "Commit") = return CommitTarget
  parseJSON x = unexpected x

instance ToJSON TodoTargetType where
  toJSON MergeRequestTarget = String "MergeRequest"
  toJSON IssueTarget = String "Issue"
  toJSON CommitTarget = String "Commit"

instance FromJSON TodoState where
  parseJSON (String "pending") = return TSPending
  parseJSON (String "done") = return TSDone
  parseJSON x = unexpected x

instance ToJSON TodoState where
  toJSON TSPending = String "pending"
  toJSON TSDone = String "done"

instance ToJSON TodoTarget where
  toJSON (TTIssue x) = toJSON x
  toJSON (TTMergeRequest x) = toJSON x
  toJSON (TTCommit x) = toJSON x

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "commit_todo_"), omitNothingFields = True} ''CommitTodo)

-- instance ToJSON CommitTodo where
--   toJSON =
--     genericToJSON
--       defaultOptions
--         { fieldLabelModifier = drop (T.length "commit_todo_"),
--           omitNothingFields = True
--         }
--   toEncoding =
--     genericToEncoding
--       defaultOptions
--         { fieldLabelModifier = drop (T.length "commit_todo_"),
--           omitNothingFields = True
--         }

data Starrer = Starrer
  { starrer_starred_since :: UTCTime,
    starrer_user :: User
  }
  deriving (Show, Eq)

data ProjectAvatar = ProjectAvatar
  { project_avatar_avatar_url :: Text
  }
  deriving (Show, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "license_"), omitNothingFields = True} ''License)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "expiration_policy_"), omitNothingFields = True} ''ExpirationPolicy)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "repository_storage_"), omitNothingFields = True} ''RepositoryStorage)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "starrer_"), omitNothingFields = True} ''Starrer)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "project_avatar_"), omitNothingFields = True} ''ProjectAvatar)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "group_"), omitNothingFields = True} ''Group)

$(deriveJSON defaultOptions {fieldLabelModifier = drop (T.length "commitnote_"), omitNothingFields = True} ''CommitNote)
