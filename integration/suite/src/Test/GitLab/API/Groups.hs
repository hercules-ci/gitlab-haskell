{-# LANGUAGE OverloadedStrings #-}

module Test.GitLab.API.Groups (spec) where

import Test.Hspec
import qualified GitLab
import Test.Helpers.Assertions
import Test.Helpers.Fixtures
import Network.HTTP.Client (responseStatus)
import Network.HTTP.Types (status404)

spec :: GitLab.GitLabServerConfig -> Spec
spec cfg = do
  describe "Group operations" $ do
    it "handles 404 for non-existent group" $ do
      responseOrError <- GitLab.runGitLab cfg $ GitLab.group 999999999
      parsedOrHttpError <- expectRight "Could not query non-existent group" responseOrError
      httpResponse <- expectLeft "Expected 404 for non-existent group" parsedOrHttpError
      showing httpResponse $ do
        responseStatus httpResponse `shouldBe` status404

    it "handles invalid group ID (0)" $ do
      responseOrError <- GitLab.runGitLab cfg $ GitLab.group 0
      parsedOrHttpError <- expectRight "Could not query group with ID 0" responseOrError
      httpResponse <- expectLeft "Expected 404 for invalid group ID" parsedOrHttpError
      showing httpResponse $ do
        responseStatus httpResponse `shouldBe` status404

    it "can create, read, update, and delete a group" $ do
      deletedGroup <- withGroup cfg $ \group -> do
        showing group $ do
          -- Force full evaluation via Show
          length (show group) `shouldSatisfy` (> 0)
          -- Asserted fields (2 of 30+):
          shouldBePositive (GitLab.group_id group)
          shouldBeNonEmpty (GitLab.group_name group)

        -- Read group back
        readResponseOrError <- GitLab.runGitLab cfg $
          GitLab.group (GitLab.group_id group)
        readParsedOrHttpError <- expectRight "Could not read group" readResponseOrError
        readGroupOrNotFound <- expectRight "Could not read group (HTTP error)" readParsedOrHttpError
        readGroup <- expectJust "Could not read group (not found)" readGroupOrNotFound
        showing readGroup $ do
          -- Force full evaluation via Show
          length (show readGroup) `shouldSatisfy` (> 0)
          -- Asserted fields (2 of 30+):
          GitLab.group_id readGroup `shouldBe` GitLab.group_id group
          GitLab.group_name readGroup `shouldBe` GitLab.group_name group

        -- Update group
        let updateAttrs = GitLab.defaultGroupFilters
              { GitLab.groupFilter_description = Just "Updated description"
              }
        updateResponseOrError <- GitLab.runGitLab cfg $
          GitLab.updateGroup (GitLab.group_id group) updateAttrs
        updateParsedOrHttpError <- expectRight "Could not update group" updateResponseOrError
        updatedGroupOrNotFound <- expectRight "Could not update group (HTTP error)" updateParsedOrHttpError
        updatedGroup <- expectJust "Could not update group (not found)" updatedGroupOrNotFound
        showing updatedGroup $ do
          -- Force full evaluation via Show
          length (show updatedGroup) `shouldSatisfy` (> 0)
          -- Asserted fields (3 of 30+):
          GitLab.group_id updatedGroup `shouldBe` GitLab.group_id group
          GitLab.group_name updatedGroup `shouldBe` GitLab.group_name group
          GitLab.group_description updatedGroup `shouldBe` Just "Updated description"

        return updatedGroup

      -- Verify group is deleted (GitLab may soft-delete groups or return 404)
      deletedResponseOrError <- GitLab.runGitLab cfg $
        GitLab.group (GitLab.group_id deletedGroup)
      case deletedResponseOrError of
        Right (Right (Just _queriedGroup)) ->
          -- Group may still be accessible, possibly marked for deletion
          -- GitLab soft-deletes groups asynchronously
          return ()
        Right (Left response) ->
          -- Group may return 404 if fully deleted
          responseStatus response `shouldBe` status404
        other ->
          expectationFailure $ "Unexpected response when querying deleted group: " ++ show other

    it "can list groups" $ do
      withGroup cfg $ \_group -> do
        let attrs = GitLab.defaultListGroupsFilters
        groupsOrError <- GitLab.runGitLab cfg $ GitLab.groups attrs
        groups <- expectRight "Could not list groups" groupsOrError
        showing groups $ do
          -- Should contain at least the group we just created
          length groups `shouldSatisfy` (>= 1)

    it "can search for a group by name" $ do
      withGroup cfg $ \group -> do
        let groupName = GitLab.group_name group
            attrs = GitLab.defaultListGroupsFilters
              { GitLab.listGroupsFilter_search = Just groupName
              }
        groupsOrError <- GitLab.runGitLab cfg $ GitLab.groups attrs
        groups <- expectRight "Could not search for group" groupsOrError
        showing groups $ do
          -- Should find the one group we created
          length groups `shouldSatisfy` (>= 1)
          -- Find our group in the results
          let foundGroups = filter (\g -> GitLab.group_id g == GitLab.group_id group) groups
          case foundGroups of
            [foundGroup] -> do
              GitLab.group_id foundGroup `shouldBe` GitLab.group_id group
              GitLab.group_name foundGroup `shouldBe` groupName
            [] -> expectationFailure "Expected to find the created group in search results"
            _ -> expectationFailure "Expected exactly one matching group in search results"

    it "can filter groups by active status" $ do
      -- Create and delete a group (marking it for deletion)
      deletedGroupId <- withGroup cfg $ \group -> do
        return (GitLab.group_id group)

      -- List only active groups - deleted group should NOT appear
      let activeAttrs = GitLab.defaultListGroupsFilters
            { GitLab.listGroupsFilter_active = Just True
            }
      activeGroupsOrError <- GitLab.runGitLab cfg $ GitLab.groups activeAttrs
      activeGroups <- expectRight "Could not list active groups" activeGroupsOrError
      showing activeGroups $ do
        -- Deleted group should NOT be in active groups
        let foundDeleted = filter (\g -> GitLab.group_id g == deletedGroupId) activeGroups
        length foundDeleted `shouldBe` 0
