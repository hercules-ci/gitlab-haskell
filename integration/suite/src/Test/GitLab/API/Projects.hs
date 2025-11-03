{-# LANGUAGE OverloadedStrings #-}

module Test.GitLab.API.Projects (spec) where

import Test.Hspec
import qualified GitLab
import Test.Helpers.Assertions
import Test.Helpers.Fixtures
import Network.HTTP.Client (responseStatus)
import Network.HTTP.Types (status404)

spec :: GitLab.GitLabServerConfig -> Spec
spec cfg = do
  describe "Project operations" $ do
    it "handles 404 for non-existent project" $ do
      responseOrError <- GitLab.runGitLab cfg $ GitLab.project 999999999
      parsedOrHttpError <- expectRight "Could not query non-existent project" responseOrError
      httpResponse <- expectLeft "Expected 404 for non-existent project" parsedOrHttpError
      showing httpResponse $ do
        responseStatus httpResponse `shouldBe` status404

    it "handles invalid project ID (0)" $ do
      responseOrError <- GitLab.runGitLab cfg $ GitLab.project 0
      parsedOrHttpError <- expectRight "Could not query project with ID 0" responseOrError
      httpResponse <- expectLeft "Expected 404 for invalid project ID" parsedOrHttpError
      showing httpResponse $ do
        responseStatus httpResponse `shouldBe` status404

    it "can create, read, and delete a project" $ do
      deletedProject <- withProject cfg $ \project -> do
        showing project $ do
          -- Force full evaluation via Show
          length (show project) `shouldSatisfy` (> 0)
          -- Asserted fields (out of 80+):
          shouldBePositive (GitLab.project_id project)
          shouldBeNonEmpty (GitLab.project_name project)

        -- Read the project back
        responseOrError <- GitLab.runGitLab cfg $
          GitLab.project (GitLab.project_id project)
        parsedOrHttpError <- expectRight "Could not read back new project" responseOrError
        projectOrNotFound <- expectRight "Could not read back new project (HTTP error)" parsedOrHttpError
        readProject <- expectJust "Could not read back new project (not found)" projectOrNotFound
        showing readProject $ do
          -- Force full evaluation via Show
          length (show readProject) `shouldSatisfy` (> 0)
          -- Asserted fields (out of 80+):
          GitLab.project_id readProject `shouldBe` GitLab.project_id project
          GitLab.project_name readProject `shouldBe` GitLab.project_name project
          GitLab.project_path readProject `shouldBe` GitLab.project_path project

        return project

      -- Verify project is deleted (GitLab marks projects for deletion)
      deletedResponseOrError <- GitLab.runGitLab cfg $
        GitLab.project (GitLab.project_id deletedProject)
      case deletedResponseOrError of
        Right (Right (Just markedProject)) ->
          showing markedProject $ do
            -- Project still accessible but marked for deletion
            GitLab.project_marked_for_deletion_at markedProject `shouldSatisfy` (/= Nothing)
        Right (Left response) ->
          responseStatus response `shouldBe` status404
        other ->
          expectationFailure $ "Unexpected response when querying deleted project: " ++ show other

    it "can update a project" $ do
      withProject cfg $ \project -> do
        -- Update project description
        let attrs = (GitLab.defaultProjectAttrs (GitLab.project_id project))
              { GitLab.project_edit_description = Just "Updated description" }
        responseOrError <- GitLab.runGitLab cfg $
          GitLab.editProject project attrs
        projectOrHttpError <- expectRight "Could not update project" responseOrError
        updatedProject <- expectRight "Could not update project (HTTP error)" projectOrHttpError
        showing updatedProject $ do
          -- Force full evaluation via Show
          length (show updatedProject) `shouldSatisfy` (> 0)
          -- Asserted fields (out of 80+):
          GitLab.project_id updatedProject `shouldBe` GitLab.project_id project
          GitLab.project_description updatedProject `shouldBe` Just "Updated description"

    it "can list projects" $ do
      -- Use simple=true to avoid GitLab 500 error with statistics
      withProject cfg $ \_project -> do
        let attrs = GitLab.defaultProjectSearchAttrs
              { GitLab.projectSearchFilter_simple = Just True
              }
        responseOrError <- GitLab.runGitLab cfg (GitLab.projects attrs)
        projectList <- expectRight "Could not list projects" responseOrError
        showing projectList $ do
          -- Should contain at least the project we just created
          length projectList `shouldSatisfy` (>= 1)

    it "can search for a project by name" $ do
      -- Use simple=true to avoid GitLab 500 error with statistics
      withProject cfg $ \project -> do
        let projectName = GitLab.project_name project
            attrs = GitLab.defaultProjectSearchAttrs
              { GitLab.projectSearchFilter_simple = Just True
              , GitLab.projectSearchFilter_search = Just projectName
              }
        responseOrError <- GitLab.runGitLab cfg (GitLab.projects attrs)
        projectList <- expectRight "Could not search for project" responseOrError
        showing projectList $ do
          -- Should find exactly the one project we created
          length projectList `shouldBe` 1
          case projectList of
            [foundProject] -> do
              GitLab.project_id foundProject `shouldBe` GitLab.project_id project
              GitLab.project_name foundProject `shouldBe` projectName
            _ -> expectationFailure "Expected exactly one project in search results"
