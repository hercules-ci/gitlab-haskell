{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Helpers.Environment
  ( getGitLabURL
  , getGitLabPassword
  , getGitLabToken
  ) where

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import Network.HTTP.Client (defaultManagerSettings, httpLbs, method, newManager, parseRequest, requestBody, requestHeaders, responseBody, RequestBody(..))
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)

getGitLabURL :: IO T.Text
getGitLabURL = T.pack . fromMaybe "http://localhost:8427" <$> lookupEnv "GITLAB_URL"

getGitLabPassword :: IO String
getGitLabPassword = fromMaybe "glhs-insecure-test-password" <$> lookupEnv "GITLAB_PASSWORD"

getGitLabToken :: IO T.Text
getGitLabToken = lookupEnv "GITLAB_TOKEN" >>= \case
  Just t -> return (T.pack t)
  Nothing -> do
    -- Try to get OAuth token using password grant
    putStrLn "No GITLAB_TOKEN set, obtaining OAuth token via password grant..."
    gitlabUrl <- getGitLabURL
    password <- getGitLabPassword
    result <- getOAuthToken (T.unpack gitlabUrl) "root" password
    case result of
      Just token -> do
        putStrLn "Successfully obtained OAuth token"
        return token
      Nothing -> do
        putStrLn "Failed to obtain OAuth token"
        error "GITLAB_TOKEN not set and OAuth token acquisition failed"

-- | Get an OAuth access token using password grant flow.
-- Note: OAuth tokens empirically expire after 2 hours (7200 seconds) as indicated
-- by the expires_in field in the response. A fresh token is obtained on each test run.
getOAuthToken :: String -> String -> String -> IO (Maybe T.Text)
getOAuthToken baseUrl username password = do
  manager <- newManager defaultManagerSettings
  let tokenUrl = baseUrl ++ "/oauth/token"
      body = BS.concat
        [ "grant_type=password"
        , "&username=", BS.pack username
        , "&password=", BS.pack password
        ]
  req <- parseRequest tokenUrl
  let req' = req
        { method = "POST"
        , requestBody = RequestBodyBS body
        , requestHeaders = [("Content-Type", "application/x-www-form-urlencoded")]
        }
  response <- httpLbs req' manager
  case Aeson.decode (responseBody response) of
    Just (Aeson.Object obj) ->
      case KM.lookup "access_token" obj of
        Just (Aeson.String token) -> return (Just token)
        _ -> return Nothing
    _ -> return Nothing
