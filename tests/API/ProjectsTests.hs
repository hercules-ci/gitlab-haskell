{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module API.ProjectsTests (projectsTests) where

import API.Common
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import GitLab
import GitLab.SystemHooks.GitLabSystemHooks
import GitLab.SystemHooks.Types
import Test.Tasty
import Test.Tasty.HUnit

-- | https://docs.gitlab.com/ee/api/projects.html
projectsTests :: [TestTree]
projectsTests =
  concat $
    [ let fname = "data/api/projects/list-all-projects.json"
       in gitlabJsonParserTests
            "list-all-projects"
            fname
            (parseOne =<< BSL.readFile fname :: IO [Project])
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO [Project]
                parseOne (encode decodedFile) :: IO [Project]
            )
    ]

{-

Untested data files:

archive-project.json
get-single-project.json
list-projects-starred-by-user.json
unarchive-project.json
get-path-to-repository-storage.json
languages.json
list-user-projects.json
unstar-project.json
get-project-hook.json
star-project.json
upload-file.json
get-project-push-rules.json
list-forks-of-project.json
starrers-of-project.json
upload-project-avatar.json
get-project-users.json
list-project-groups.json
transfer-project-new-namespace.json

-}
