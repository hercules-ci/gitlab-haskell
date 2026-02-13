{-# LANGUAGE OverloadedStrings #-}

module Test.GitLab.API.Version (spec) where

import Test.Hspec
import qualified GitLab
import qualified Data.Text
import Test.Helpers.Assertions

spec :: GitLab.GitLabServerConfig -> Spec
spec cfg = do
  describe "Version operations" $ do
    it "can fetch GitLab version" $ do
      responseOrError <- GitLab.runGitLab cfg GitLab.gitlabVersion
      parsedOrHttpError <- expectRight "Could not fetch GitLab version" responseOrError
      versionOrNotFound <- expectRight "Could not fetch GitLab version (HTTP error)" parsedOrHttpError
      version <- expectJust "Could not fetch GitLab version (not found)" versionOrNotFound
      showing version $ do
        -- Force full evaluation via Show
        length (show version) `shouldSatisfy` (> 0)
        -- Asserted fields (all):
        GitLab.version_version version `shouldSatisfy` (not . Data.Text.null)
        GitLab.version_revision version `shouldSatisfy` (not . Data.Text.null)

