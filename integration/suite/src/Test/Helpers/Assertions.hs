{-# LANGUAGE OverloadedStrings #-}

module Test.Helpers.Assertions
  ( shouldBePositive
  , shouldBeNonEmpty
  , showing
  , expectRight
  , expectLeft
  , expectJust
  ) where

import qualified Data.Text as T
import Control.Exception (catch, throwIO)
import Test.HUnit.Lang (HUnitFailure(..), FailureReason(..))
import Test.Hspec (expectationFailure, shouldSatisfy)

-- | Assert that an Int is positive (> 0)
shouldBePositive :: Int -> IO ()
shouldBePositive n = n `shouldSatisfy` (> 0)

-- | Assert that a Text is non-empty
shouldBeNonEmpty :: T.Text -> IO ()
shouldBeNonEmpty t = t `shouldSatisfy` (not . T.null)

-- | Annotate test assertions with a value that will be shown on failure.
-- This provides context when assertions fail, showing the full value being tested.
--
-- Example:
-- @
-- showing project $ do
--   GitLab.project_id project \`shouldSatisfy\` (> 0)
--   GitLab.project_name project \`shouldNotBe\` ""
-- @
showing :: (Show a) => a -> IO r -> IO r
showing value assertion =
  assertion `catch` \(HUnitFailure loc reason) ->
    let valueStr = "Context value:\n" ++ show value
        newReason = case reason of
          Reason msg -> Reason (msg ++ "\n\n" ++ valueStr)
          ExpectedButGot preface expected actual ->
            let newPreface = case preface of
                  Nothing -> Just valueStr
                  Just p -> Just (p ++ "\n\n" ++ valueStr)
            in ExpectedButGot newPreface expected actual
    in throwIO $ HUnitFailure loc newReason

-- | Extract a Right value from an Either, or fail with a meaningful error message.
expectRight :: (Show e) => String -> Either e a -> IO a
expectRight _ (Right value) = return value
expectRight context (Left err) = do
  expectationFailure $ context ++ ": " ++ show err
  error "expectRight: unreachable"

-- | Extract a Left value from an Either, or fail with a meaningful error message.
expectLeft :: (Show a) => String -> Either e a -> IO e
expectLeft _ (Left value) = return value
expectLeft context (Right val) = do
  expectationFailure $ context ++ ": Expected Left, got Right: " ++ show val
  error "expectLeft: unreachable"

-- | Extract a Just value from a Maybe, or fail with a meaningful error message.
expectJust :: String -> Maybe a -> IO a
expectJust _ (Just value) = return value
expectJust context Nothing = do
  expectationFailure $ context ++ ": Got Nothing"
  error "expectJust: unreachable"
