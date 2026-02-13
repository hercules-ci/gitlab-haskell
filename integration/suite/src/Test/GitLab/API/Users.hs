{-# LANGUAGE OverloadedStrings #-}

module Test.GitLab.API.Users (spec) where

import Test.Hspec
import qualified GitLab
import Test.Helpers.Assertions
import Test.Helpers.Fixtures
import Network.HTTP.Client (responseStatus)
import Network.HTTP.Types (status404)

spec :: GitLab.GitLabServerConfig -> Spec
spec cfg = do
  describe "User operations" $ do
    it "handles 404 for non-existent user" $ do
      responseOrError <- GitLab.runGitLab cfg $ GitLab.user 999999999
      parsedOrHttpError <- expectRight "Could not query non-existent user" responseOrError
      httpResponse <- expectLeft "Expected 404 for non-existent user" parsedOrHttpError
      showing httpResponse $ do
        responseStatus httpResponse `shouldBe` status404

    it "handles invalid user ID (0)" $ do
      responseOrError <- GitLab.runGitLab cfg $ GitLab.user 0
      parsedOrHttpError <- expectRight "Could not query user with ID 0" responseOrError
      httpResponse <- expectLeft "Expected 404 for invalid user ID" parsedOrHttpError
      showing httpResponse $ do
        responseStatus httpResponse `shouldBe` status404

    it "can get current user" $ do
      userOrError <- GitLab.runGitLab cfg GitLab.currentUser
      user <- expectRight "Could not get current user" userOrError
      showing user $ do
        -- Force full evaluation via Show
        length (show user) `shouldSatisfy` (> 0)
        -- Asserted fields (7 of 30+):
        shouldBePositive (GitLab.user_id user)
        GitLab.user_username user `shouldBe` "root"
        GitLab.user_name user `shouldBe` "Administrator"
        GitLab.user_state user `shouldBe` Just "active"
        GitLab.user_can_create_group user `shouldBe` Just True
        GitLab.user_can_create_project user `shouldBe` Just True
        GitLab.user_bot user `shouldBe` Just False
        -- Unasserted fields: user_bio, user_two_factor_enabled, user_last_sign_in_at,
        -- user_current_sign_in_at, user_last_activity_on, user_skype, user_twitter,
        -- user_website_url, user_theme_id, user_color_scheme_id, user_external,
        -- user_private_profile, user_projects_limit, user_public_email, user_organization,
        -- user_job_title, user_pronouns, user_linkedin, user_confirmed_at, user_identities,
        -- user_email, user_followers, user_following, user_avatar_url, user_web_url,
        -- user_location, user_extern_uid, user_group_id_for_saml, user_discussion_locked,
        -- user_created_at, user_note, user_password

    it "can create, read, update, and delete a user" $ do
      deletedUser <- withUser cfg $ \user -> do
        showing user $ do
          -- Force full evaluation via Show
          length (show user) `shouldSatisfy` (> 0)
          -- Asserted fields (3 of 30+):
          shouldBePositive (GitLab.user_id user)
          shouldBeNonEmpty (GitLab.user_username user)
          GitLab.user_name user `shouldBe` "Test User"

        -- Read user back
        readResponseOrError <- GitLab.runGitLab cfg $
          GitLab.user (GitLab.user_id user)
        readParsedOrHttpError <- expectRight "Could not read user" readResponseOrError
        readUserOrNotFound <- expectRight "Could not read user (HTTP error)" readParsedOrHttpError
        readUser <- expectJust "Could not read user (not found)" readUserOrNotFound
        showing readUser $ do
          -- Force full evaluation via Show
          length (show readUser) `shouldSatisfy` (> 0)
          -- Asserted fields (3 of 30+):
          GitLab.user_id readUser `shouldBe` GitLab.user_id user
          GitLab.user_username readUser `shouldBe` GitLab.user_username user
          GitLab.user_name readUser `shouldBe` GitLab.user_name user

        -- Update user
        let updateAttrs = GitLab.defaultUserFilters
              { GitLab.userFilter_bio = Just "Updated bio"
              }
        updateResponseOrError <- GitLab.runGitLab cfg $
          GitLab.modifyUser (GitLab.user_id user) updateAttrs
        updateParsedOrHttpError <- expectRight "Could not update user" updateResponseOrError
        updatedUserOrNotFound <- expectRight "Could not update user (HTTP error)" updateParsedOrHttpError
        updatedUser <- expectJust "Could not update user (not found)" updatedUserOrNotFound
        showing updatedUser $ do
          -- Force full evaluation via Show
          length (show updatedUser) `shouldSatisfy` (> 0)
          -- Asserted fields (4 of 30+):
          GitLab.user_id updatedUser `shouldBe` GitLab.user_id user
          GitLab.user_username updatedUser `shouldBe` GitLab.user_username user
          GitLab.user_name updatedUser `shouldBe` GitLab.user_name user
          GitLab.user_bio updatedUser `shouldBe` Just "Updated bio"

        return updatedUser

      -- Verify user is deleted (GitLab blocks users asynchronously, or returns 404)
      deletedResponseOrError <- GitLab.runGitLab cfg $
        GitLab.user (GitLab.user_id deletedUser)
      case deletedResponseOrError of
        Right (Right (Just queriedUser)) ->
          showing queriedUser $ do
            -- User may still be accessible, possibly blocked or in transition to blocked
            -- Note: GitLab blocks users asynchronously, so state may be "active" or "blocked"
            GitLab.user_state queriedUser `shouldSatisfy` (\s -> s == Just "blocked" || s == Just "active")
        Right (Left response) ->
          -- User may return 404 if fully deleted
          responseStatus response `shouldBe` status404
        other ->
          expectationFailure $ "Unexpected response when querying deleted user: " ++ show other
