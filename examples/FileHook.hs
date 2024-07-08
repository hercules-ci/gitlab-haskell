{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (void)
import GitLab

main :: IO ()
main =
  runGitLab
    ( defaultGitLabServer
        { url = "https://gitlab.example.com",
          token = AuthMethodToken "abcde12345"
        }
    )
    ( receive
        [ matchIf
            "create an issue against any project named test_project123"
            ( \projEvent@ProjectCreate {} -> do
                return (projectCreate_name projEvent == "test_project123")
            )
            ( \projEvent@ProjectCreate {} -> do
                Right (Just proj) <- project (projectCreate_project_id projEvent)
                void $ newIssue proj "the title" "the description" (defaultIssueAttrs (project_id proj))
            )
        ]
    )
