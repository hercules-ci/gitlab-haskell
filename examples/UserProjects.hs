{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import Data.Maybe (fromJust)
import GitLab

main :: IO ()
main = do
  myProjects <-
    runGitLab
      ( defaultGitLabServer
          { url = "https://gitlab.com",
            token = AuthMethodToken "insert your token here"
          }
      )
      (searchUser "robstewart57" >>= \usr -> userProjects (fromJust usr) defaultProjectSearchAttrs)
  print myProjects
