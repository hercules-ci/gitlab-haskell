{-# LANGUAGE FlexibleInstances #-}

module API.PipelinesTests (pipelinesTests) where

import API.Common
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import GitLab
import GitLab.SystemHooks.GitLabSystemHooks
import GitLab.SystemHooks.Types
import Test.Tasty
import Test.Tasty.HUnit

-- | https://docs.gitlab.com/ee/api/pipelines.html
pipelinesTests :: [TestTree]
pipelinesTests =
  []
