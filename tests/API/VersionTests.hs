{-# LANGUAGE FlexibleInstances #-}

module API.VersionTests (versionTests) where

import API.Common
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import GitLab
import GitLab.SystemHooks.GitLabSystemHooks
import GitLab.SystemHooks.Types
import Test.Tasty
import Test.Tasty.HUnit

-- | https://docs.gitlab.com/ee/api/version.html
versionTests :: [TestTree]
versionTests =
  let fname = "data/api/version/version.json"
   in gitlabJsonParserTests
        "version"
        fname
        (parseOne =<< BSL.readFile fname :: IO Version)
        ( do
            decodedFile <- parseOne =<< BSL.readFile fname :: IO Version
            parseOne (encode decodedFile) :: IO Version
        )
