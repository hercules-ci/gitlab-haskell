{-# LANGUAGE FlexibleInstances #-}

module API.RepositoriesTests (repositoriesTests) where

import API.Common
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import GitLab
import GitLab.SystemHooks.GitLabSystemHooks
import GitLab.SystemHooks.Types
import Test.Tasty
import Test.Tasty.HUnit

-- | https://docs.gitlab.com/ee/api/repositories.html
repositoriesTests :: [TestTree]
repositoriesTests =
  concat
    [ let fname = "data/api/repositories/contributors.json"
       in gitlabJsonParserTests
            "contributors"
            fname
            (parseOne =<< BSL.readFile fname :: IO [Contributor])
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO [Contributor]
                parseOne (encode decodedFile) :: IO [Contributor]
            ),
      let fname = "data/api/repositories/list-repository-tree.json"
       in gitlabJsonParserTests
            "list-repository-tree"
            fname
            (parseOne =<< BSL.readFile fname :: IO [Repository])
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO [Repository]
                parseOne (encode decodedFile) :: IO [Repository]
            ),
      let fname = "data/api/repositories/merge-base.json"
       in gitlabJsonParserTests
            "merge-base"
            fname
            (parseOne =<< BSL.readFile fname :: IO Commit)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO Commit
                parseOne (encode decodedFile) :: IO Commit
            )
    ]

{-

compare-branches-tags-commits.json
generate-changelog-data.json

-}
