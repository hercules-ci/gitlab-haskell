{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.GitLab.API.RepositoryFiles (spec) where

import Test.Hspec
import qualified GitLab
import Test.Helpers.Assertions
import Test.Helpers.Fixtures
import Network.HTTP.Client (responseStatus)
import Network.HTTP.Types (status404)
import qualified Data.Text as T

spec :: GitLab.GitLabServerConfig -> Spec
spec cfg = do
  describe "Repository file operations" $ do
    it "can create, read, update, and delete a file" $ do
      withProject cfg $ \project -> do
        let filePath = "README.md"
            branchName = "main"  -- GitLab default branch
            initialContent = "# Test Project\n\nThis is a test."
            createCommitMsg = "Add README"
            updatedContent = "# Test Project\n\nUpdated content."
            updateCommitMsg = "Update README"
            deleteCommitMsg = "Delete README"

        -- Create file
        createResponseOrError <- GitLab.runGitLab cfg $
          GitLab.createRepositoryFile project filePath branchName initialContent createCommitMsg
        createOrHttpError <- expectRight "Could not create file" createResponseOrError
        createdFileOrNotFound <- expectRight "Could not create file (HTTP error)" createOrHttpError
        createdFile <- expectJust "Could not create file (not found)" createdFileOrNotFound
        showing createdFile $ do
          GitLab.repository_file_simple_file_path createdFile `shouldBe` filePath
          GitLab.repository_file_simple_branch createdFile `shouldBe` branchName

        -- Read file back
        readResponseOrError <- GitLab.runGitLab cfg $
          GitLab.repositoryFile project filePath branchName
        readOrHttpError <- expectRight "Could not read file" readResponseOrError
        readFileOrNotFound <- expectRight "Could not read file (HTTP error)" readOrHttpError
        fileInfo <- expectJust "Could not read file (not found)" readFileOrNotFound
        showing fileInfo $ do
          GitLab.repository_file_file_path fileInfo `shouldBe` filePath
          GitLab.repository_file_file_name fileInfo `shouldBe` "README.md"
          GitLab.repository_file_size fileInfo `shouldSatisfy` (> 0)
          -- Content is Base64 encoded
          GitLab.repository_file_content fileInfo `shouldSatisfy` (\c -> T.length c > 0)

        -- Update file
        updateResponseOrError <- GitLab.runGitLab cfg $
          GitLab.updateRepositoryFile project filePath branchName updatedContent updateCommitMsg
        updateOrHttpError <- expectRight "Could not update file" updateResponseOrError
        updatedFileOrNotFound <- expectRight "Could not update file (HTTP error)" updateOrHttpError
        updatedFile <- expectJust "Could not update file (not found)" updatedFileOrNotFound
        showing updatedFile $ do
          GitLab.repository_file_simple_file_path updatedFile `shouldBe` filePath

        -- Verify update by reading file back
        updatedReadResponseOrError <- GitLab.runGitLab cfg $
          GitLab.repositoryFile project filePath branchName
        updatedReadOrHttpError <- expectRight "Could not read updated file" updatedReadResponseOrError
        updatedFileInfoOrNotFound <- expectRight "Could not read updated file (HTTP error)" updatedReadOrHttpError
        updatedFileInfo <- expectJust "Could not read updated file (not found)" updatedFileInfoOrNotFound
        showing updatedFileInfo $ do
          GitLab.repository_file_file_path updatedFileInfo `shouldBe` filePath
          -- Content is Base64 encoded - just verify it's non-empty
          GitLab.repository_file_content updatedFileInfo `shouldSatisfy` (\c -> T.length c > 0)

        -- Delete file
        deleteResponseOrError <- GitLab.runGitLab cfg $
          GitLab.deleteRepositoryFile project filePath branchName deleteCommitMsg
        deleteOrHttpError <- expectRight "Could not delete file" deleteResponseOrError
        (_ :: Maybe ()) <- expectRight "Could not delete file (HTTP error)" deleteOrHttpError
        return ()

        -- Verify file is deleted (should return 404)
        deletedResponseOrError <- GitLab.runGitLab cfg $
          GitLab.repositoryFile project filePath branchName
        deletedOrHttpError <- expectRight "Could not check deleted file" deletedResponseOrError
        httpResponse <- expectLeft "Expected 404 for deleted file" deletedOrHttpError
        showing httpResponse $ do
          responseStatus httpResponse `shouldBe` status404

    it "handles 404 for non-existent files" $ do
      withProject cfg $ \project -> do
        let filePath = "does-not-exist.txt"
            branchName = "main"

        -- Try to read non-existent file with repositoryFile
        readResponseOrError <- GitLab.runGitLab cfg $
          GitLab.repositoryFile project filePath branchName
        readOrHttpError <- expectRight "Could not read non-existent file" readResponseOrError
        httpResponse <- expectLeft "Expected 404 for non-existent file" readOrHttpError
        showing httpResponse $ do
          responseStatus httpResponse `shouldBe` status404

        -- Try to read non-existent file with repositoryFileRawFile
        rawResponseOrError <- GitLab.runGitLab cfg $
          GitLab.repositoryFileRawFile project filePath branchName
        rawOrHttpError <- expectRight "Could not read non-existent raw file" rawResponseOrError
        rawHttpResponse <- expectLeft "Expected 404 for non-existent raw file" rawOrHttpError
        showing rawHttpResponse $ do
          responseStatus rawHttpResponse `shouldBe` status404

    it "can handle empty files" $ do
      withProject cfg $ \project -> do
        let filePath = "empty.txt"
            branchName = "main"
            emptyContent = ""
            commitMsg = "Add empty file"

        -- Create empty file
        createResponseOrError <- GitLab.runGitLab cfg $
          GitLab.createRepositoryFile project filePath branchName emptyContent commitMsg
        createOrHttpError <- expectRight "Could not create empty file" createResponseOrError
        createdFileOrNotFound <- expectRight "Could not create empty file (HTTP error)" createOrHttpError
        createdFile <- expectJust "Could not create empty file (not found)" createdFileOrNotFound

        let createdBranch = GitLab.repository_file_simple_branch createdFile

        -- Read back via repositoryFile (Base64-encoded)
        readResponseOrError <- GitLab.runGitLab cfg $
          GitLab.repositoryFile project filePath createdBranch
        readOrHttpError <- expectRight "Could not read empty file" readResponseOrError
        readFileOrNotFound <- expectRight "Could not read empty file (HTTP error)" readOrHttpError
        fileInfo <- expectJust "Could not read empty file (not found)" readFileOrNotFound
        showing fileInfo $ do
          GitLab.repository_file_file_path fileInfo `shouldBe` filePath
          GitLab.repository_file_size fileInfo `shouldBe` 0

        -- Read back via repositoryFileRawFile
        rawResponseOrError <- GitLab.runGitLab cfg $
          GitLab.repositoryFileRawFile project filePath createdBranch
        rawOrHttpError <- expectRight "Could not read empty raw file" rawResponseOrError
        rawContent <- expectRight "Could not read empty raw file (HTTP error)" rawOrHttpError
        showing rawContent $ do
          rawContent `shouldBe` mempty
