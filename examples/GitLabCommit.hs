{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.Traversable
import GitLab

main :: IO ()
main = do
  void $
    runGitLab
      ( defaultGitLabServer
          { url = "https://gitlab.com",
            token = AuthMethodToken "insert your token here"
          }
      )
      ( do
          projectData <- projectsAndIssues
          liftIO (print projectData)
      )

projectsAndIssues :: GitLab [(Project, [Issue])]
projectsAndIssues = do
  Just usr <- searchUser "robstewart57"
  Just projs <- userProjects usr defaultProjectSearchAttrs
  for
    projs
    ( \proj -> do
        issues <- projectIssues proj defaultIssueFilters
        return (proj, issues)
    )
