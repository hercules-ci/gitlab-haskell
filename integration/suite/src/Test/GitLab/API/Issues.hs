{-# LANGUAGE OverloadedStrings #-}

module Test.GitLab.API.Issues (spec) where

import Test.Hspec
import qualified GitLab
import Test.Helpers.Assertions
import Test.Helpers.Fixtures
import Network.HTTP.Client (responseStatus)
import Network.HTTP.Types (status404, status304)

spec :: GitLab.GitLabServerConfig -> Spec
spec cfg = do
  describe "Issue operations" $ do
    it "handles 404 for non-existent issue" $ do
      responseOrError <- GitLab.runGitLab cfg $ GitLab.issue 999999999
      parsedOrHttpError <- expectRight "Could not query non-existent issue" responseOrError
      httpResponse <- expectLeft "Expected 404 for non-existent issue" parsedOrHttpError
      showing httpResponse $ do
        responseStatus httpResponse `shouldBe` status404

    it "can create, read, update, and delete an issue" $ do
      withProject cfg $ \project -> do
        deletedIssue <- withIssue cfg project $ \issue -> do
          showing issue $ do
            -- Force full evaluation via Show
            length (show issue) `shouldSatisfy` (> 0)
            -- Asserted fields (3 of 30+):
            shouldBePositive (GitLab.issue_id issue)
            shouldBeNonEmpty (GitLab.issue_title issue)
            GitLab.issue_state issue `shouldBe` "opened"

          -- Read issue back (using iid, the project-internal issue ID)
          readResponseOrError <- GitLab.runGitLab cfg $
            GitLab.projectIssue project (GitLab.issue_iid issue)
          readParsedOrHttpError <- expectRight "Could not read issue" readResponseOrError
          readIssueOrNotFound <- expectRight "Could not read issue (HTTP error)" readParsedOrHttpError
          readIssue <- expectJust "Could not read issue (not found)" readIssueOrNotFound
          showing readIssue $ do
            -- Force full evaluation via Show
            length (show readIssue) `shouldSatisfy` (> 0)
            -- Asserted fields (3 of 30+):
            GitLab.issue_id readIssue `shouldBe` GitLab.issue_id issue
            GitLab.issue_title readIssue `shouldBe` GitLab.issue_title issue
            GitLab.issue_state readIssue `shouldBe` "opened"

          -- Update issue (using iid, the project-internal issue ID)
          let updateAttrs = (GitLab.defaultIssueAttrs (GitLab.project_id project))
                { GitLab.set_issue_description = Just "Updated description"
                }
          updateResponseOrError <- GitLab.runGitLab cfg $
            GitLab.editIssue project (GitLab.issue_iid issue) updateAttrs
          updatedIssueOrHttpError <- expectRight "Could not update issue" updateResponseOrError
          updatedIssue <- expectRight "Could not update issue (HTTP error)" updatedIssueOrHttpError
          showing updatedIssue $ do
            -- Force full evaluation via Show
            length (show updatedIssue) `shouldSatisfy` (> 0)
            -- Asserted fields (4 of 30+):
            GitLab.issue_id updatedIssue `shouldBe` GitLab.issue_id issue
            GitLab.issue_title updatedIssue `shouldBe` GitLab.issue_title issue
            GitLab.issue_description updatedIssue `shouldBe` Just "Updated description"
            GitLab.issue_state updatedIssue `shouldBe` "opened"

          return updatedIssue

        -- Verify issue is deleted (should return 404, using iid)
        deletedResponseOrError <- GitLab.runGitLab cfg $
          GitLab.projectIssue project (GitLab.issue_iid deletedIssue)
        parsedOrHttpError <- expectRight "Could not query deleted issue" deletedResponseOrError
        httpResponse <- expectLeft "Expected 404 for deleted issue" parsedOrHttpError
        showing httpResponse $ do
          responseStatus httpResponse `shouldBe` status404

    it "can list project issues" $ do
      withProject cfg $ \project -> do
        withIssue cfg project $ \_issue -> do
          let attrs = GitLab.defaultIssueFilters
          issuesOrError <- GitLab.runGitLab cfg $ GitLab.projectIssues project attrs
          issues <- expectRight "Could not list project issues" issuesOrError
          showing issues $ do
            -- Should contain at least the issue we just created
            length issues `shouldSatisfy` (>= 1)

    it "can filter issues by state" $ do
      withProject cfg $ \project -> do
        withIssue cfg project $ \issue -> do
          -- Close the issue first (using iid, the project-internal issue ID)
          let closeAttrs = (GitLab.defaultIssueAttrs (GitLab.project_id project))
                { GitLab.set_issue_state_event = Just "close"
                }
          closeResponseOrError <- GitLab.runGitLab cfg $
            GitLab.editIssue project (GitLab.issue_iid issue) closeAttrs
          closeOrHttpError <- expectRight "Could not close issue" closeResponseOrError
          closedIssue <- expectRight "Could not close issue (HTTP error)" closeOrHttpError

          showing closedIssue $ do
            GitLab.issue_state closedIssue `shouldBe` "closed"

          -- List closed issues
          let attrs = GitLab.defaultIssueFilters
                { GitLab.issueFilter_state = Just GitLab.IssueClosed
                }
          issuesOrError <- GitLab.runGitLab cfg $ GitLab.projectIssues project attrs
          issues <- expectRight "Could not list closed issues" issuesOrError
          showing issues $ do
            length issues `shouldSatisfy` (>= 1)
            -- All issues should be closed
            all (\i -> GitLab.issue_state i == "closed") issues `shouldBe` True

    it "can reopen a closed issue" $ do
      withProject cfg $ \project -> do
        withIssue cfg project $ \issue -> do
          -- Close the issue
          let closeAttrs = (GitLab.defaultIssueAttrs (GitLab.project_id project))
                { GitLab.set_issue_state_event = Just "close"
                }
          closeOrHttpError <- GitLab.runGitLab cfg $
            GitLab.editIssue project (GitLab.issue_iid issue) closeAttrs
          closedIssueOrHttpError <- expectRight "Could not close issue" closeOrHttpError
          closedIssue <- expectRight "Could not close issue (HTTP error)" closedIssueOrHttpError
          showing closedIssue $ do
            GitLab.issue_state closedIssue `shouldBe` "closed"

          -- Reopen the issue
          let reopenAttrs = (GitLab.defaultIssueAttrs (GitLab.project_id project))
                { GitLab.set_issue_state_event = Just "reopen"
                }
          reopenOrHttpError <- GitLab.runGitLab cfg $
            GitLab.editIssue project (GitLab.issue_iid issue) reopenAttrs
          reopenedIssueOrHttpError <- expectRight "Could not reopen issue" reopenOrHttpError
          reopenedIssue <- expectRight "Could not reopen issue (HTTP error)" reopenedIssueOrHttpError
          showing reopenedIssue $ do
            GitLab.issue_state reopenedIssue `shouldBe` "opened"

    it "can assign issues to users" $ do
      withProject cfg $ \project -> do
        withIssue cfg project $ \issue -> do
          -- Get current user to assign to
          currentUserOrError <- GitLab.runGitLab cfg GitLab.currentUser
          currentUser <- expectRight "Could not get current user" currentUserOrError

          -- Assign issue to current user
          let assignAttrs = (GitLab.defaultIssueAttrs (GitLab.project_id project))
                { GitLab.set_issue_assignee_id = Just (GitLab.user_id currentUser)
                }
          assignOrHttpError <- GitLab.runGitLab cfg $
            GitLab.editIssue project (GitLab.issue_iid issue) assignAttrs
          assignedIssueOrHttpError <- expectRight "Could not assign issue" assignOrHttpError
          assignedIssue <- expectRight "Could not assign issue (HTTP error)" assignedIssueOrHttpError
          showing assignedIssue $ do
            -- Check that assignee is set
            GitLab.issue_assignee assignedIssue `shouldSatisfy` (/= Nothing)
            case GitLab.issue_assignee assignedIssue of
              Just assignee -> GitLab.user_id assignee `shouldBe` GitLab.user_id currentUser
              Nothing -> expectationFailure "Expected assignee to be set"

    it "can add labels to issues" $ do
      withProject cfg $ \project -> do
        withIssue cfg project $ \issue -> do
          -- Add labels to issue
          let labelAttrs = (GitLab.defaultIssueAttrs (GitLab.project_id project))
                { GitLab.set_issue_labels = Just ["bug", "urgent"]
                }
          labelOrHttpError <- GitLab.runGitLab cfg $
            GitLab.editIssue project (GitLab.issue_iid issue) labelAttrs
          labeledIssueOrHttpError <- expectRight "Could not add labels to issue" labelOrHttpError
          labeledIssue <- expectRight "Could not add labels to issue (HTTP error)" labeledIssueOrHttpError
          showing labeledIssue $ do
            GitLab.issue_labels labeledIssue `shouldBe` Just ["bug", "urgent"]

          -- Update labels
          let newLabelAttrs = (GitLab.defaultIssueAttrs (GitLab.project_id project))
                { GitLab.set_issue_labels = Just ["bug", "documentation"]
                }
          updatedLabelOrHttpError <- GitLab.runGitLab cfg $
            GitLab.editIssue project (GitLab.issue_iid issue) newLabelAttrs
          updatedLabelIssueOrHttpError <- expectRight "Could not update labels" updatedLabelOrHttpError
          updatedLabelIssue <- expectRight "Could not update labels (HTTP error)" updatedLabelIssueOrHttpError
          showing updatedLabelIssue $ do
            GitLab.issue_labels updatedLabelIssue `shouldBe` Just ["bug", "documentation"]

    it "can subscribe and unsubscribe from issues" $ do
      withProject cfg $ \project -> do
        withIssue cfg project $ \issue -> do
          -- First unsubscribe (user is auto-subscribed when creating an issue)
          unsubscribeResponseOrError <- GitLab.runGitLab cfg $
            GitLab.unsubscribeIssue project (GitLab.issue_iid issue)
          unsubscribedOrHttpError <- expectRight "Could not unsubscribe from issue" unsubscribeResponseOrError
          unsubscribedIssueOrNotFound <- expectRight "Could not unsubscribe from issue (HTTP error)" unsubscribedOrHttpError
          unsubscribedIssue <- expectJust "Could not unsubscribe from issue (not found)" unsubscribedIssueOrNotFound
          showing unsubscribedIssue $ do
            GitLab.issue_subscribed unsubscribedIssue `shouldBe` Just False

          -- Verify by reading the issue back
          readAfterUnsubscribeOrError <- GitLab.runGitLab cfg $
            GitLab.projectIssue project (GitLab.issue_iid issue)
          readUnsubscribedOrHttpError <- expectRight "Could not read issue after unsubscribe" readAfterUnsubscribeOrError
          readUnsubscribedIssue <- expectRight "Could not read issue after unsubscribe (HTTP error)" readUnsubscribedOrHttpError
          verifiedUnsubscribed <- expectJust "Issue not found after unsubscribe" readUnsubscribedIssue
          showing verifiedUnsubscribed $ do
            GitLab.issue_subscribed verifiedUnsubscribed `shouldBe` Just False

          -- Now subscribe to issue
          subscribeResponseOrError <- GitLab.runGitLab cfg $
            GitLab.subscribeIssue project (GitLab.issue_iid issue)
          subscribedOrHttpError <- expectRight "Could not subscribe to issue" subscribeResponseOrError
          subscribedIssueOrNotFound <- expectRight "Could not subscribe to issue (HTTP error)" subscribedOrHttpError
          subscribedIssue <- expectJust "Could not subscribe to issue (not found)" subscribedIssueOrNotFound
          showing subscribedIssue $ do
            GitLab.issue_subscribed subscribedIssue `shouldBe` Just True

          -- Verify by reading the issue back
          readAfterSubscribeOrError <- GitLab.runGitLab cfg $
            GitLab.projectIssue project (GitLab.issue_iid issue)
          readSubscribedOrHttpError <- expectRight "Could not read issue after subscribe" readAfterSubscribeOrError
          readSubscribedIssue <- expectRight "Could not read issue after subscribe (HTTP error)" readSubscribedOrHttpError
          verifiedSubscribed <- expectJust "Issue not found after subscribe" readSubscribedIssue
          showing verifiedSubscribed $ do
            GitLab.issue_subscribed verifiedSubscribed `shouldBe` Just True

    it "handles 304 when subscribing to already-subscribed issue" $ do
      withProject cfg $ \project -> do
        withIssue cfg project $ \issue -> do
          -- User is auto-subscribed when creating an issue
          -- GitLab returns 304 (Not Modified) when subscribing again
          subscribeResponseOrError <- GitLab.runGitLab cfg $
            GitLab.subscribeIssue project (GitLab.issue_iid issue)
          parsedOrHttpError <- expectRight "Could not subscribe to issue" subscribeResponseOrError

          -- GitLab returns Left (HTTP response) with 304 status for "already subscribed"
          case parsedOrHttpError of
            Left response -> do
              showing response $ do
                responseStatus response `shouldBe` status304
            Right _ -> do
              expectationFailure "Expected 304 Not Modified for already-subscribed issue, but got success response"

    it "can move an issue to a different project" $ do
      withProject cfg $ \sourceProject -> do
        withProject cfg $ \targetProject -> do
          withIssue cfg sourceProject $ \issue -> do
            let originalIssueId = GitLab.issue_id issue
                originalTitle = GitLab.issue_title issue

            -- Move issue to target project
            moveResponseOrError <- GitLab.runGitLab cfg $
              GitLab.moveIssue sourceProject (GitLab.issue_iid issue) (GitLab.project_id targetProject)
            movedOrHttpError <- expectRight "Could not move issue" moveResponseOrError
            movedIssueOrNotFound <- expectRight "Could not move issue (HTTP error)" movedOrHttpError
            movedIssue <- expectJust "Could not move issue (not found)" movedIssueOrNotFound

            showing movedIssue $ do
              -- GitLab creates a new issue with a new ID when moving
              GitLab.issue_id movedIssue `shouldSatisfy` (/= originalIssueId)
              -- Title should be preserved
              GitLab.issue_title movedIssue `shouldBe` originalTitle
              -- Issue should be in target project
              GitLab.issue_project_id movedIssue `shouldBe` Just (GitLab.project_id targetProject)

            -- Verify original issue is closed in source project (GitLab closes it, doesn't delete)
            sourceCheckOrError <- GitLab.runGitLab cfg $
              GitLab.projectIssue sourceProject (GitLab.issue_iid issue)
            sourceParsedOrHttpError <- expectRight "Could not check source project" sourceCheckOrError
            sourceIssueOrNotFound <- expectRight "Could not check source project (HTTP error)" sourceParsedOrHttpError
            sourceIssue <- expectJust "Original issue not found in source project" sourceIssueOrNotFound
            showing sourceIssue $ do
              -- Original issue should be closed after move
              GitLab.issue_state sourceIssue `shouldBe` "closed"
              -- Should still have the same ID and title
              GitLab.issue_id sourceIssue `shouldBe` originalIssueId
              GitLab.issue_title sourceIssue `shouldBe` originalTitle

            -- Verify issue exists in target project by listing
            targetIssuesOrError <- GitLab.runGitLab cfg $
              GitLab.projectIssues targetProject GitLab.defaultIssueFilters
            targetIssues <- expectRight "Could not list target project issues" targetIssuesOrError
            showing targetIssues $ do
              -- Find the moved issue by title in the target project
              let foundIssues = filter (\i -> GitLab.issue_title i == originalTitle) targetIssues
              length foundIssues `shouldBe` 1
              case foundIssues of
                [foundIssue] -> GitLab.issue_id foundIssue `shouldBe` GitLab.issue_id movedIssue
                _ -> expectationFailure "Expected exactly one moved issue in target project"

    it "can clone an issue to a different project" $ do
      withProject cfg $ \sourceProject -> do
        withProject cfg $ \targetProject -> do
          withIssue cfg sourceProject $ \issue -> do
            let originalIssueId = GitLab.issue_id issue
                originalTitle = GitLab.issue_title issue

            -- Clone issue to target project
            cloneResponseOrError <- GitLab.runGitLab cfg $
              GitLab.cloneIssue sourceProject (GitLab.issue_iid issue) (GitLab.project_id targetProject)
            clonedOrHttpError <- expectRight "Could not clone issue" cloneResponseOrError
            clonedIssueOrNotFound <- expectRight "Could not clone issue (HTTP error)" clonedOrHttpError
            clonedIssue <- expectJust "Could not clone issue (not found)" clonedIssueOrNotFound

            showing clonedIssue $ do
              -- Cloned issue should have a new ID
              GitLab.issue_id clonedIssue `shouldSatisfy` (/= originalIssueId)
              -- Title should be preserved
              GitLab.issue_title clonedIssue `shouldBe` originalTitle
              -- Should be in target project
              GitLab.issue_project_id clonedIssue `shouldBe` Just (GitLab.project_id targetProject)

            -- Verify original issue still exists and is OPEN in source project (unlike move)
            sourceCheckOrError <- GitLab.runGitLab cfg $
              GitLab.projectIssue sourceProject (GitLab.issue_iid issue)
            sourceParsedOrHttpError <- expectRight "Could not check source project" sourceCheckOrError
            sourceIssueOrNotFound <- expectRight "Could not check source project (HTTP error)" sourceParsedOrHttpError
            sourceIssue <- expectJust "Original issue not found in source project" sourceIssueOrNotFound
            showing sourceIssue $ do
              -- Original issue should still be open after clone (unlike move which closes it)
              GitLab.issue_state sourceIssue `shouldBe` "opened"
              -- Should still have the same ID and title
              GitLab.issue_id sourceIssue `shouldBe` originalIssueId
              GitLab.issue_title sourceIssue `shouldBe` originalTitle

            -- Verify cloned issue exists in target project
            targetIssuesOrError <- GitLab.runGitLab cfg $
              GitLab.projectIssues targetProject GitLab.defaultIssueFilters
            targetIssues <- expectRight "Could not list target project issues" targetIssuesOrError
            showing targetIssues $ do
              -- Find the cloned issue by title in the target project
              let foundIssues = filter (\i -> GitLab.issue_title i == originalTitle) targetIssues
              length foundIssues `shouldBe` 1
              case foundIssues of
                [foundIssue] -> GitLab.issue_id foundIssue `shouldBe` GitLab.issue_id clonedIssue
                _ -> expectationFailure "Expected exactly one cloned issue in target project"

    -- TODO: Add tests for issue milestones
    -- Blocked on: No GitLab.API.Milestones module exists
    -- Would test: set_issue_milestone_id, create milestone, assign to issue

    -- TODO: Add tests for merge request relationships
    -- Blocked on: No GitLab.API.MergeRequests module or MR test infrastructure
    -- Would test: issueMergeRequests, issueMergeRequestsThatClose

    -- TODO: Add tests for issue statistics
    -- Blocked on: Nothing - APIs exist (issueStatisticsProject, issueStatisticsGroup)
    -- Would test: Get issue counts/statistics for projects and groups

    -- TODO: Add tests for issue participants
    -- Blocked on: No comment/note API integration tests yet
    -- Would test: issueParticipants after multiple users comment

    -- TODO: Add tests for userIssues
    -- Blocked on: Authenticating as different users (need API tokens for created users)
    -- Would test: Create users with auth tokens, create issues as those users, query by user ID
    -- Note: withUser creates users but we use root's token - can't create issues as them
