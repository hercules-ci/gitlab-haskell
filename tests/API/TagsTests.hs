{-# LANGUAGE FlexibleInstances #-}

module API.TagsTests (tagsTests) where

import API.Common
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import GitLab
import GitLab.SystemHooks.GitLabSystemHooks
import GitLab.SystemHooks.Types
import Test.Tasty
import Test.Tasty.HUnit

-- | https://docs.gitlab.com/ee/api/tags.html
tagsTests :: [TestTree]
tagsTests =
  concat
    [ let fname = "data/api/tags/list-project-repository-tags.json"
       in gitlabJsonParserTests
            "list-project-repository-tags"
            fname
            (parseOne =<< BSL.readFile fname :: IO [Tag])
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO [Tag]
                parseOne (encode decodedFile) :: IO [Tag]
            ),
      let fname = "data/api/tags/single-repository-tag.json"
       in gitlabJsonParserTests
            "single-repository-tag"
            fname
            (parseOne =<< BSL.readFile fname :: IO Tag)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO Tag
                parseOne (encode decodedFile) :: IO Tag
            ),
      let fname = "data/api/tags/create-new-tag.json"
       in gitlabJsonParserTests
            "create-new-tag"
            fname
            (parseOne =<< BSL.readFile fname :: IO Tag)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO Tag
                parseOne (encode decodedFile) :: IO Tag
            )
    ]
