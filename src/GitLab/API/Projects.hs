{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Projects
-- Description : Queries about projects
-- Copyright   : (c) Rob Stewart, Heriot-Watt University, 2019
-- License     : BSD3
-- Maintainer  : robstewart57@gmail.com
-- Stability   : stable
module GitLab.API.Projects
  ( allProjects,
    projectForks,
    searchProjectId,
    projectsWithName,
    projectWithPathAndName,
    multipleCommitters,
    commitsEmailAddresses,
    commitsEmailAddresses',
    userProjects,
    userProjects',
    projectOfIssue,
    issuesCreatedByUser,
    issuesOnForks,
    projectMemebersCount,
    projectCISuccess,
    namespacePathToUserId,
    projectDiffs,
    projectDiffs',
    addGroupToProject,
    transferProject,
    transferProject',
    editProject,
    editProject',
    projectAttrs,
    projectAttrsParams,
    ProjectAttrs (..),
    EnabledDisabled (..),
    AutoDeployStrategy (..),
    GitStrategy (..),
    ProjectSettingAccessLevel (..),
    MergeMethod (..),
    SquashOption (..),
  )
where

import qualified Data.ByteString.Lazy as BSL
import Data.Either
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.Generics
import GitLab.API.Commits
import GitLab.API.Issues
import GitLab.API.Members
import GitLab.API.Pipelines
import GitLab.API.Users
import GitLab.Types
import GitLab.WebRequests.GitLabWebCalls
import Network.HTTP.Client
import Network.HTTP.Types.URI

-- | gets all projects.
allProjects :: GitLab [Project]
allProjects =
  fromRight (error "allProjects error")
    <$> gitlabGetMany "/projects" [("statistics", Just "true")]

-- | gets all forks of a project. Supports use of namespaces.
--
-- > projectForks "project1"
-- > projectForks "group1/project1"
projectForks ::
  -- | name or namespace of the project
  Text ->
  GitLab (Either (Response BSL.ByteString) [Project])
projectForks projectName = do
  let urlPath =
        "/projects/"
          <> T.decodeUtf8 (urlEncode False (T.encodeUtf8 projectName))
          <> "/forks"
  gitlabGetMany urlPath []

-- | searches for a 'Project' with the given project ID, returns
-- 'Nothing' if a project with the given ID is not found.
searchProjectId ::
  -- | project ID
  Int ->
  GitLab (Either (Response BSL.ByteString) (Maybe Project))
searchProjectId projectId = do
  let urlPath = T.pack ("/projects/" <> show projectId)
  gitlabGetOne urlPath [("statistics", Just "true")]

-- | gets all projects with the given project name.
--
-- > projectsWithName "project1"
projectsWithName ::
  -- | project name being searched for.
  Text ->
  GitLab [Project]
projectsWithName projectName = do
  results <- gitlabGetMany "/projects" [("search", Just (T.encodeUtf8 projectName))]
  case results of
    Left _ -> error "projectsWithName error"
    Right projects ->
      return $
        filter (\project -> projectName == project_path project) projects

-- | gets a project with the given name for the given full path of the
--   namespace. E.g.
--
-- > projectWithPathAndName "user1" "project1"
--
-- looks for "user1/project1"
--
-- > projectWithPathAndName "group1/subgroup1" "project1"
--
-- looks for "project1" within the namespace with full path "group1/subgroup1"
projectWithPathAndName :: Text -> Text -> GitLab (Either (Response BSL.ByteString) (Maybe Project))
projectWithPathAndName namespaceFullPath projectName = do
  gitlabGetOne
    ( "/projects/"
        <> T.decodeUtf8
          (urlEncode False (T.encodeUtf8 (namespaceFullPath <> "/" <> projectName)))
    )
    [("statistics", Just "true")]

-- | returns 'True' if a project has multiple committers, according to
-- the email addresses of the commits.
multipleCommitters :: Project -> GitLab Bool
multipleCommitters project = do
  emailAddresses <- commitsEmailAddresses project
  return (length (nub emailAddresses) > 1)

-- | gets the email addresses in the author information in all commit
-- for a project.
commitsEmailAddresses :: Project -> GitLab [Text]
commitsEmailAddresses project = do
  result <- commitsEmailAddresses' (project_id project)
  return (fromRight (error "commitsEmailAddresses error") result)

-- | gets the email addresses in the author information in all commit
-- for a project defined by the project's ID.
commitsEmailAddresses' :: Int -> GitLab (Either (Response BSL.ByteString) [Text])
commitsEmailAddresses' projectId = do
  -- (commits :: [Commit]) <- projectCommits' projectId
  attempt <- projectCommits' projectId
  case attempt of
    Left resp -> return (Left resp)
    Right (commits :: [Commit]) ->
      return (Right (map commit_author_email commits))

-- | gets all projects for a user given their username.
--
-- > userProjects "harry"
userProjects' :: Text -> GitLab (Maybe [Project])
userProjects' username = do
  userMaybe <- searchUser username
  case userMaybe of
    Nothing -> return Nothing
    Just usr -> do
      result <- gitlabGetMany (urlPath (user_id usr)) []
      case result of
        Left _ -> error "userProjects' error"
        Right projs -> return (Just projs)
  where
    urlPath usrId = "/users/" <> T.pack (show usrId) <> "/projects"

-- | gets all projects for a user.
--
-- > userProjects myUser
userProjects :: User -> GitLab (Maybe [Project])
userProjects theUser =
  userProjects' (user_username theUser)

-- | gets the 'GitLab.Types.Project' against which the given 'Issue'
-- was created.
projectOfIssue :: Issue -> GitLab Project
projectOfIssue issue = do
  let prId = fromJust (error "projectOfIssue error") (issue_project_id issue)
  result <- searchProjectId prId
  case fromRight (error "projectOfIssue error") result of
    Nothing -> error "projectOfIssue error"
    Just proj -> return proj

-- | finds all issues created by a user.
--
-- > issuesCreatedByUser "user1"
--
-- returns a (user,projects) tuple, where user is the 'User' found
-- for the given searched username, and a list of 'Project's that the
-- user has created issues for.
issuesCreatedByUser :: Text -> GitLab (Maybe (User, [Project]))
issuesCreatedByUser username = do
  user_maybe <- searchUser username
  case user_maybe of
    Nothing -> return Nothing
    Just usr -> do
      usersIssues <- userIssues usr
      projects <- mapM projectOfIssue usersIssues
      return (Just (usr, projects))

-- | searches for all projects with the given name, and returns a list
-- of triples of: 1) the found project, 2) the list of issues for the
-- found projects, and 3) a list of users who've created issues.
issuesOnForks ::
  -- | name or namespace of the project
  Text ->
  GitLab [(Project, [Issue], [User])]
issuesOnForks projectName = do
  projects <- projectsWithName projectName
  mapM processProject projects
  where
    processProject ::
      Project ->
      GitLab (Project, [Issue], [User])
    processProject proj = do
      (openIssues :: [Issue]) <- projectIssues proj defaultIssueFilters
      let authors = map (fromJust (error "issuesOnForks error") . issue_author) openIssues
      return (proj, openIssues, authors)

-- | returns a (namespace,members) tuple for the given 'Project',
-- where namespace is the namespace of the project
-- e.g. "user1/project1", and members is a list of (username,name)
-- tuples about all members of the project.
projectMemebersCount :: Project -> GitLab (Text, [(Text, Text)])
projectMemebersCount project = do
  friends <- count
  return (namespace_name (fromJust (project_namespace project)), friends)
  where
    count = do
      let addr =
            "/projects/" <> T.pack (show (project_id project)) <> "/members/all"
      (res :: [Member]) <- fromRight (error "projectMembersCount error") <$> gitlabGetMany addr []
      return (map (\x -> (fromJust (member_username x), fromJust (member_name x))) res)

-- | returns 'True' is the last commit for a project passes all
-- continuous integration tests.
projectCISuccess ::
  -- | the name or namespace of the project
  Project ->
  GitLab Bool
projectCISuccess project = do
  pipes <- pipelines project
  case pipes of
    [] -> return False
    (x : _) -> return (pipeline_status x == "success")

-- | searches for a username, and returns a user ID for that user, or
-- 'Nothing' if a user cannot be found.
namespacePathToUserId ::
  -- | name or namespace of project
  Text ->
  GitLab (Maybe Int)
namespacePathToUserId namespacePath = do
  user_maybe <- searchUser namespacePath
  case user_maybe of
    Nothing -> return Nothing
    Just usr -> return (Just (user_id usr))

-- | gets all diffs in a project for a given commit SHA.
projectDiffs :: Project -> Text -> GitLab (Either (Response BSL.ByteString) [Diff])
projectDiffs proj =
  projectDiffs' (project_id proj)

-- | gets all diffs in a project for a given project ID, for a given
-- commit SHA.
projectDiffs' :: Int -> Text -> GitLab (Either (Response BSL.ByteString) [Diff])
projectDiffs' projId commitSha =
  gitlabGetMany
    ( "/projects/"
        <> T.pack (show projId)
        <> "/repository/commits/"
        <> commitSha
        <> "/diff/"
    )
    []

-- | add a group to a project.
addGroupToProject ::
  -- | group ID
  Int ->
  -- | project ID
  Int ->
  -- | level of access granted
  AccessLevel ->
  GitLab (Either (Response BSL.ByteString) (Maybe GroupShare))
addGroupToProject groupId projectId access =
  gitlabPost addr params
  where
    params :: [GitLabParam]
    params =
      [ ("group_id", Just (T.encodeUtf8 (T.pack (show groupId)))),
        ("group_access", Just (T.encodeUtf8 (T.pack (show access))))
      ]
    addr =
      "/projects/"
        <> T.pack (show projectId)
        <> "/share"

-- | transfer a project to a new namespace.
transferProject ::
  -- | project
  Project ->
  -- | namespace where to transfer project to
  Text ->
  GitLab (Either (Response BSL.ByteString) Project)
transferProject prj = transferProject' (project_id prj)

-- | edit a project.
transferProject' ::
  -- | project ID
  Int ->
  -- | namespace where to transfer project to
  Text ->
  GitLab (Either (Response BSL.ByteString) Project)
transferProject' projId namespaceString = do
  let urlPath =
        "/projects/"
          <> T.pack (show projId)
          <> "/transfer"
  result <-
    gitlabPut
      urlPath
      [ ("id", Just (T.encodeUtf8 (T.pack (show projId)))),
        ("namespace", Just (T.encodeUtf8 namespaceString))
      ]
  case result of
    Left resp -> return (Left resp)
    Right Nothing -> error "transferProject error"
    Right (Just proj) -> return (Right proj)

-- | Edit a project. The 'projectAttrs' value has default project
-- search values, which is a record that can be modified with 'Just'
-- values.
--
-- For example to disable project specific email notifications:
--
-- > editProject myProject (projectAttrs { project_edit_emails_disabled = Just True })
editProject ::
  -- | project
  Project ->
  -- | project attributes
  ProjectAttrs ->
  GitLab (Either (Response BSL.ByteString) Project)
editProject prj = editProject' (project_id prj)

-- | Edit a project. The 'projectAttrs' value has default project
-- search values, which is a record that can be modified with 'Just'
-- values.
--
-- For example to disable project specific email notifications for a
-- project with project ID 11744514:
--
-- > editProject' 11744514 (projectAttrs { project_edit_emails_disabled = Just True })
editProject' ::
  -- | project ID
  Int ->
  -- | project attributes
  ProjectAttrs ->
  GitLab (Either (Response BSL.ByteString) Project)
editProject' projId attrs = do
  let urlPath =
        "/projects/"
          <> T.pack (show projId)
  result <-
    gitlabPut
      urlPath
      (projectAttrsParams attrs)
  case result of
    Left resp -> return (Left resp)
    Right Nothing -> error "editProject error"
    Right (Just proj) -> return (Right proj)

-- | A default set of project attributes to override with the
-- 'editProject' functions. Only the project ID value is set is a
-- search parameter, all other search parameters are not set and can
-- be overwritten.
projectAttrs ::
  -- | project ID
  Int ->
  ProjectAttrs
projectAttrs projId =
  ProjectAttrs Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing projId Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

projectAttrsParams :: ProjectAttrs -> [GitLabParam]
projectAttrsParams filters =
  catMaybes
    [ (\b -> Just ("allow_merge_on_skipped_pipeline", textToBS (showBool b))) =<< project_edit_allow_merge_on_skipped_pipeline filters,
      (\x -> Just ("analytics_access_level", textToBS (T.pack (show x)))) =<< project_edit_analytics_access_level filters,
      (\i -> Just ("approvals_before_merge", textToBS (T.pack (show i)))) =<< project_edit_approvals_before_merge filters,
      (\x -> Just ("auto_cancel_pending_pipelines", textToBS (T.pack (show x))))
        =<< project_edit_auto_cancel_pending_pipelines
          filters,
      (\x -> Just ("auto_devops_deploy_strategy", textToBS (T.pack (show x))))
        =<< project_edit_auto_devops_deploy_strategy filters,
      (\b -> Just ("auto_devops_enabled", textToBS (showBool b))) =<< project_edit_auto_devops_enabled filters,
      (\b -> Just ("autoclose_referenced_issues", textToBS (showBool b))) =<< project_edit_autoclose_referenced_issues filters,
      (\t -> Just ("build_coverage_regex", textToBS t)) =<< project_edit_build_coverage_regex filters,
      (\x -> Just ("build_git_strategy", textToBS (T.pack (show x)))) =<< project_edit_build_git_strategy filters,
      (\i -> Just ("build_timeout", textToBS (T.pack (show i)))) =<< project_edit_build_timeout filters,
      (\x -> Just ("builds_access_level", textToBS (T.pack (show x)))) =<< project_edit_builds_access_level filters,
      (\t -> Just ("ci_config_path", textToBS t)) =<< project_edit_ci_config_path filters,
      (\i -> Just ("ci_default_git_depth", textToBS (T.pack (show i)))) =<< project_edit_ci_default_git_depth filters,
      (\b -> Just ("ci_forward_deployment_enabled", textToBS (showBool b))) =<< project_edit_ci_forward_deployment_enabled filters,
      (\x -> Just ("container_registry_access_level", textToBS (T.pack (show x)))) =<< project_edit_container_registry_access_level filters,
      (\t -> Just ("default_branch", textToBS t)) =<< project_edit_default_branch filters,
      (\t -> Just ("description", textToBS t)) =<< project_edit_description filters,
      (\b -> Just ("emails_disabled", textToBS (showBool b))) =<< project_edit_emails_disabled filters,
      (\t -> Just ("external_authorization_classification_label", textToBS t)) =<< project_edit_external_authorization_classification_label filters,
      (\x -> Just ("forking_access_level", textToBS (T.pack (show x)))) =<< project_edit_forking_access_level filters,
      Just ("id", textToBS (T.pack (show (project_edit_id filters)))),
      (\t -> Just ("import_url", textToBS t)) =<< project_edit_import_url filters,
      (\x -> Just ("issues_access_level", textToBS (T.pack (show x)))) =<< project_edit_issues_access_level filters,
      (\b -> Just ("lfs_enabled", textToBS (showBool b))) =<< project_edit_lfs_enabled filters,
      (\x -> Just ("merge_method", textToBS (T.pack (show x)))) =<< project_edit_merge_method filters,
      (\x -> Just ("merge_requests_access_level", textToBS (T.pack (show x)))) =<< project_edit_merge_requests_access_level filters,
      (\b -> Just ("mirror_overwrites_diverged_branches", textToBS (showBool b))) =<< project_edit_mirror_overwrites_diverged_branches filters,
      (\b -> Just ("mirror_trigger_builds", textToBS (showBool b))) =<< project_edit_mirror_trigger_builds filters,
      (\i -> Just ("mirror_user_id", textToBS (T.pack (show i)))) =<< project_edit_mirror_user_id filters,
      (\b -> Just ("mirror", textToBS (showBool b))) =<< project_edit_mirror filters,
      (\t -> Just ("name", textToBS t)) =<< project_edit_name filters,
      (\x -> Just ("operations_access_level", textToBS (T.pack (show x)))) =<< project_edit_operations_access_level filters,
      (\b -> Just ("only_allow_merge_if_all_discussions_are_resolved", textToBS (showBool b))) =<< project_edit_only_allow_merge_if_all_discussions_are_resolved filters,
      (\b -> Just ("only_allow_merge_if_pipeline_succeeds", textToBS (showBool b))) =<< project_edit_only_allow_merge_if_pipeline_succeeds filters,
      (\b -> Just ("only_mirror_protected_branches", textToBS (showBool b))) =<< project_edit_only_mirror_protected_branches filters,
      (\b -> Just ("packages_enabled", textToBS (showBool b))) =<< project_edit_packages_enabled filters,
      (\x -> Just ("pages_access_level", textToBS (T.pack (show x)))) =<< project_edit_pages_access_level filters,
      (\x -> Just ("requirements_access_level", textToBS (T.pack (show x)))) =<< project_edit_requirements_access_level filters,
      (\b -> Just ("restrict_user_defined_variables", textToBS (showBool b))) =<< project_edit_restrict_user_defined_variables filters,
      (\t -> Just ("path", textToBS t)) =<< project_edit_path filters,
      (\b -> Just ("public_builds", textToBS (showBool b))) =<< project_edit_public_builds filters,
      (\b -> Just ("remove_source_branch_after_merge", textToBS (showBool b))) =<< project_edit_remove_source_branch_after_merge filters,
      (\x -> Just ("repository_access_level", textToBS (T.pack (show x)))) =<< project_edit_repository_access_level filters,
      (\t -> Just ("repository_storage", textToBS t)) =<< project_edit_repository_storage filters,
      (\b -> Just ("request_access_enabled", textToBS (showBool b))) =<< project_edit_request_access_enabled filters,
      (\b -> Just ("resolve_outdated_diff_discussions", textToBS (showBool b))) =<< project_edit_resolve_outdated_diff_discussions filters,
      (\b -> Just ("service_desk_enabled", textToBS (showBool b))) =<< project_edit_service_desk_enabled filters,
      (\b -> Just ("shared_runners_enabled", textToBS (showBool b))) =<< project_edit_shared_runners_enabled filters,
      (\b -> Just ("show_default_award_emojis", textToBS (showBool b))) =<< project_edit_show_default_award_emojis filters,
      (\x -> Just ("snippets_access_level", textToBS (T.pack (show x)))) =<< project_edit_snippets_access_level filters,
      (\x -> Just ("squash_option", textToBS (T.pack (show x)))) =<< project_edit_squash_option filters,
      (\t -> Just ("suggestion_commit_message", textToBS t)) =<< project_edit_suggestion_commit_message filters,
      (\x -> Just ("visibility", textToBS (T.pack (show x)))) =<< project_edit_visibility filters,
      (\x -> Just ("wiki_access_level", textToBS (T.pack (show x)))) =<< project_edit_wiki_access_level filters,
      (\t -> Just ("issues_template", textToBS t)) =<< project_edit_issues_template filters,
      (\t -> Just ("merge_requests_template", textToBS t)) =<< project_edit_merge_requests_template filters,
      (\b -> Just ("keep_latest_artifact", textToBS (showBool b))) =<< project_edit_keep_latest_artifact filters
    ]
  where
    textToBS = Just . T.encodeUtf8
    showBool :: Bool -> Text
    showBool True = "true"
    showBool False = "false"

-- | Attributes for updating when editing a project with the
-- 'editProject' functions.
data ProjectAttrs = ProjectAttrs
  { -- | Set whether or not merge requests can be merged with skipped jobs.
    project_edit_allow_merge_on_skipped_pipeline :: Maybe Bool,
    -- | One of disabled, private or enabled.
    project_edit_analytics_access_level :: Maybe Text,
    -- | How many approvers should approve merge request by default.
    project_edit_approvals_before_merge :: Maybe Int,
    -- | Auto-cancel pending pipelines.
    project_edit_auto_cancel_pending_pipelines :: Maybe EnabledDisabled,
    -- | Auto Deploy strategy (continuous, manual, or timed_incremental).
    project_edit_auto_devops_deploy_strategy :: Maybe AutoDeployStrategy,
    -- | Enable Auto DevOps for this project.
    project_edit_auto_devops_enabled :: Maybe Bool,
    -- | Set whether auto-closing referenced issues on default branch.
    project_edit_autoclose_referenced_issues :: Maybe Bool,
    -- | Test coverage parsing.
    project_edit_build_coverage_regex :: Maybe Text,
    -- | The Git strategy. Defaults to fetch.
    project_edit_build_git_strategy :: Maybe GitStrategy,
    -- | The maximum amount of time, in seconds, that a job can run.
    project_edit_build_timeout :: Maybe Int,
    -- | One of disabled, private, or enabled.
    project_edit_builds_access_level :: Maybe ProjectSettingAccessLevel,
    -- | The path to CI configuration file.
    project_edit_ci_config_path :: Maybe Text,
    -- | Default number of revisions for shallow cloning.
    project_edit_ci_default_git_depth :: Maybe Int,
    -- | When a new deployment job starts, skip older deployment jobs that are still pending.
    project_edit_ci_forward_deployment_enabled :: Maybe Bool,
    -- | Set visibility of container registry, for this project, to one of disabled, private or enabled.
    project_edit_container_registry_access_level :: Maybe ProjectSettingAccessLevel,
    -- | The default branch name.
    project_edit_default_branch :: Maybe Text,
    -- | Short project description.
    project_edit_description :: Maybe Text,
    -- | Disable email notifications.
    project_edit_emails_disabled :: Maybe Bool,
    -- | The classification label for the project.
    project_edit_external_authorization_classification_label :: Maybe Text,
    -- | One of disabled, private, or enabled.
    project_edit_forking_access_level :: Maybe ProjectSettingAccessLevel,
    -- | The ID or URL-encoded path of the project.
    project_edit_id :: Int,
    -- | URL to import repository from.
    project_edit_import_url :: Maybe Text,
    -- | One of disabled, private, or enabled.
    project_edit_issues_access_level :: Maybe ProjectSettingAccessLevel,
    -- | Enable LFS.
    project_edit_lfs_enabled :: Maybe Bool,
    -- | Set the merge method used.
    project_edit_merge_method :: Maybe MergeMethod,
    -- | One of disabled, private, or enabled.
    project_edit_merge_requests_access_level :: Maybe ProjectSettingAccessLevel,
    -- | Pull mirror overwrites diverged branches.
    project_edit_mirror_overwrites_diverged_branches :: Maybe Bool,
    -- | Pull mirroring triggers builds.
    project_edit_mirror_trigger_builds :: Maybe Bool,
    -- | User responsible for all the activity surrounding a pull mirror event. (admins only)
    project_edit_mirror_user_id :: Maybe Int,
    -- | Enables pull mirroring in a project.
    project_edit_mirror :: Maybe Bool,
    -- | The name of the project.
    project_edit_name :: Maybe Text,
    -- | One of disabled, private, or enabled.
    project_edit_operations_access_level :: Maybe ProjectSettingAccessLevel,
    -- | Set whether merge requests can only be merged when all the discussions are resolved.
    project_edit_only_allow_merge_if_all_discussions_are_resolved :: Maybe Bool,
    -- | Set whether merge requests can only be merged with successful jobs.
    project_edit_only_allow_merge_if_pipeline_succeeds :: Maybe Bool,
    -- | Only mirror protected branches.
    project_edit_only_mirror_protected_branches :: Maybe Bool,
    -- | Enable or disable packages repository feature.
    project_edit_packages_enabled :: Maybe Bool,
    -- | One of disabled, private, enabled, or public.
    project_edit_pages_access_level :: Maybe ProjectSettingAccessLevel,
    -- | One of disabled, private, enabled or public.
    project_edit_requirements_access_level :: Maybe ProjectSettingAccessLevel,
    -- | Allow only maintainers to pass user-defined variables when triggering a pipeline. For example when the pipeline is triggered in the UI, with the API, or by a trigger token.
    project_edit_restrict_user_defined_variables :: Maybe Bool,
    -- | Custom repository name for the project. By default generated based on name.
    project_edit_path :: Maybe Text,
    -- | If true, jobs can be viewed by non-project members.
    project_edit_public_builds :: Maybe Bool,
    -- | Enable Delete source branch option by default for all new merge requests.
    project_edit_remove_source_branch_after_merge :: Maybe Bool,
    -- | One of disabled, private, or enabled.
    project_edit_repository_access_level :: Maybe ProjectSettingAccessLevel,
    -- | Which storage shard the repository is on. (admins only)
    project_edit_repository_storage :: Maybe Text,
    -- | Allow users to request member access.
    project_edit_request_access_enabled :: Maybe Bool,
    -- | Automatically resolve merge request diffs discussions on lines changed with a push.
    project_edit_resolve_outdated_diff_discussions :: Maybe Bool,
    -- | Enable or disable Service Desk feature.
    project_edit_service_desk_enabled :: Maybe Bool,
    -- | Enable shared runners for this project.
    project_edit_shared_runners_enabled :: Maybe Bool,
    -- | Show default award emojis.
    project_edit_show_default_award_emojis :: Maybe Bool,
    -- | One of disabled, private, or enabled.
    project_edit_snippets_access_level :: Maybe ProjectSettingAccessLevel,
    -- | One of never, always, default_on, or default_off.
    project_edit_squash_option :: Maybe SquashOption,
    -- | The commit message used to apply merge request suggestions.
    project_edit_suggestion_commit_message :: Maybe Text,
    project_edit_visibility :: Maybe Visibility,
    -- | One of disabled, private, or enabled.
    project_edit_wiki_access_level :: Maybe ProjectSettingAccessLevel,
    -- | Default description for Issues. Description is parsed with GitLab Flavored Markdown. See Templates for issues and merge requests.
    project_edit_issues_template :: Maybe Text,
    -- | Default description for Merge Requests. Description is parsed with GitLab Flavored Markdown.
    project_edit_merge_requests_template :: Maybe Text,
    -- | Disable or enable the ability to keep the latest artifact for this project.
    project_edit_keep_latest_artifact :: Maybe Bool
  }
  deriving (Generic, Show, Eq)

-- | Is auto-cancel pending pipelines enabled or disabled
data EnabledDisabled
  = Enabled
  | Disabled
  deriving (Eq)

instance Show EnabledDisabled where
  show Enabled = "enabled"
  show Disabled = "disabled"

-- | Auto Deploy strategy: continuous, manual, or timed_incremental,
-- for the 'editProject' functions
data AutoDeployStrategy
  = Continuous
  | Manual
  | TimedIncremental
  deriving (Eq)

instance Show AutoDeployStrategy where
  show Continuous = "continuous"
  show Manual = "manual"
  show TimedIncremental = "timed_incremental"

-- | The Git strategy, defaults to fetch, for the 'editProject' functions
data GitStrategy
  = Clone
  | Fetch
  | None
  deriving (Eq)

instance Show GitStrategy where
  show Clone = "clone"
  show Fetch = "fetch"
  show None = "none"

-- | The project access level setting, for the 'editProject' functions
data ProjectSettingAccessLevel
  = DisabledAccess
  | PrivateAccess
  | EnabledAccess
  | PublicAccess
  deriving (Eq)

instance Show ProjectSettingAccessLevel where
  show DisabledAccess = "disabled"
  show PrivateAccess = "private"
  show EnabledAccess = "enabled"
  show PublicAccess = "public"

-- | The project git merge method, for the 'editProject' functions
data MergeMethod
  = Merge
  | RebaseMerge
  | FF
  deriving (Eq)

instance Show MergeMethod where
  show Merge = "merge"
  show RebaseMerge = "rebase_merge"
  show FF = "ff"

-- | The project git merge squash option, for the 'editProject' functions
data SquashOption
  = NeverSquash
  | AlwaysSquash
  | DefaultOnSquash
  | DefaultOffSquash
  deriving (Eq)

instance Show SquashOption where
  show NeverSquash = "never"
  show AlwaysSquash = "always"
  show DefaultOnSquash = "default_on"
  show DefaultOffSquash = "default_off"
