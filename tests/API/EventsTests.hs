{-# LANGUAGE FlexibleInstances #-}

module API.EventsTests (eventsTests) where

import API.Common
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import GitLab
import GitLab.SystemHooks.GitLabSystemHooks
import GitLab.SystemHooks.Types
import Test.Tasty
import Test.Tasty.HUnit

-- | https://docs.gitlab.com/ee/api/events.html
eventsTests :: [TestTree]
eventsTests =
  []

{-

get-user-contributions-events.json
list-current-authenticated-users-events.json
list-projects-visible-events.json

-}
