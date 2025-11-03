{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec ( describe, parallel, Spec )
import qualified GitLab
import Test.Helpers.Environment ( getGitLabURL, getGitLabToken )
import qualified Test.GitLab.API.Groups
import qualified Test.GitLab.API.Projects
import qualified Test.GitLab.API.Users
import qualified Test.GitLab.API.Version
import qualified Test.Hspec.Runner as R
import System.Environment (getArgs)
import Data.Maybe (fromMaybe)
import GHC.Conc (getNumCapabilities)

main :: IO ()
main = do
  gitlabUrl <- getGitLabURL
  token <- getGitLabToken

  let cfg = GitLab.defaultGitLabServer
        { GitLab.url = gitlabUrl
        , GitLab.token = GitLab.AuthMethodOAuth token
        }

  testCfg <- do
    let defConfig = R.defaultConfig {
            -- Show execution time
            R.configTimes = True
          }
    parsedCfg <- getArgs >>= R.readConfig defConfig

    numCores <- getNumCapabilities
    -- Be considerate of the GitLab VM size if test runner is large machine
    -- Over approx 2× the number of VM cores we get diminishing returns and
    -- even more unreliable test case timings due to high load.
    let limit = 16
        defaultJobs = min limit numCores
    pure parsedCfg {
        R.configConcurrentJobs = Just $ fromMaybe defaultJobs $ R.configConcurrentJobs parsedCfg
      }

  R.hspecWith testCfg $ parallel $ spec cfg

spec :: GitLab.GitLabServerConfig -> Spec
spec cfg = do
  describe "GitLab API calls" $ do
    Test.GitLab.API.Version.spec cfg
    Test.GitLab.API.Users.spec cfg
    Test.GitLab.API.Projects.spec cfg
    Test.GitLab.API.Groups.spec cfg
