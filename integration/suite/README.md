# GitLab Haskell Integration Test Suite

This directory contains integration tests for the `gitlab-haskell` library.

Information about the test environment can be found in [the parent README](../README.md).

## Running Tests

**Note**: These integration tests are **not** run by `cabal test`. They must be run explicitly with `cabal run integration-suite` as shown below.

The tests require a running GitLab instance. A NixOS lightweight VM is provided for this purpose.

```bash
# From the repository root
nix run .#gitlab-dev-vm
```

In another terminal:

```bash
cd integration/suite
cabal run integration-suite
```

## Test suite structure

The test suite is organized into modules mirroring the GitLab API:

- `Test.GitLab.API.Version`: Version and basic connectivity tests
- `Test.GitLab.API.Users`: User CRUD operations and error handling
- `Test.GitLab.API.Projects`: Project CRUD operations and error handling

### Helper modules

- `Test.Helpers.Environment`: GitLab environment configuration (URL, password, OAuth token)
- `Test.Helpers.Assertions`: Test assertion helpers (`expectRight`, `expectLeft`, `expectJust`, `showing`, `shouldBe*`)
- `Test.Helpers.Fixtures`: Test fixtures for resource management (`withProject`, `withUser`, `generateRandomName`)

## Testing Guidelines

### Assert Types on Ignored Values

**Rule**: When using pattern matching or helper functions that ignore a value (like `_`), always assert the type with a type annotation.

**Rationale**: This prevents silent type mismatches that can hide bugs. For example, if an API changes from `Either a (Maybe b)` to `Either a (Either c (Maybe b))`, a test using `_` will still compile but may not be testing what you think.

**Examples**:

```haskell
-- Good: Type annotation makes the expected structure explicit
deleteProject project = do
  result <- GitLab.runGitLab cfg $ GitLab.deleteProject project
  parsedOrHttpError <- expectRight "Failed to delete project" result
  (_ :: Maybe ()) <- expectRight "HTTP error during delete" parsedOrHttpError
  return ()

-- Bad: Type is implicit, could silently accept wrong structure
deleteProject project = do
  result <- GitLab.runGitLab cfg $ GitLab.deleteProject project
  parsedOrHttpError <- expectRight "Failed to delete project" result
  _ <- expectRight "HTTP error during delete" parsedOrHttpError
  return ()
```

### Use Helper Functions Consistently

- `expectRight`: Extract a Right value or fail the test
- `expectLeft`: Extract a Left value or fail the test
- `expectJust`: Extract a Just value or fail the test
- `showing`: Annotate assertions with context that will be shown on failure

### Use `showing` for Context on Failure

**Rule**: Use `showing` when assertions don't reveal the full object being tested.

**When to use**:

Whenever assertion statements will not report the full object.

- Asserting on individual fields of a response object (User, Project, etc.)
- Asserting on HTTP responses where only the status is checked, because a bad response may have important info that's not in the status.

**When NOT to use**:
- With `expectRight`, `expectLeft`, `expectJust` - these already provide good error messages
- With `shouldBe` on the entire value instead of just a field. Then `shouldBe` reports the whole unexpected value.

**Examples**:

```haskell
-- Good: Assertions on fields don't show full object
user <- expectRight "Could not get user" userOrError
showing user $ do
  GitLab.user_id user `shouldSatisfy` (> 0)
  GitLab.user_username user `shouldBe` "root"

-- Good: HTTP response status doesn't show full response
httpResponse <- expectLeft "Expected 404" parsedOrHttpError
showing httpResponse $ do
  responseStatus httpResponse `shouldBe` status404

-- Bad: expectRight already shows both sides on failure
showing userOrError $ do
  user <- expectRight "Could not get user" userOrError
  -- ...
```

### Domain-Specific Assertions

Use domain-specific helpers for common assertions:

- `shouldBePositive`: Assert that an Int is positive (> 0)
- `shouldBeNonEmpty`: Assert that a Text is non-empty

### Resource Cleanup

Use the bracket pattern for resources that need cleanup:

- `withProject`: Creates a project, runs a test, and cleans up
- `withUser`: Creates a user, runs a test, and cleans up

**Important**: Cleanup failures MUST fail the test. This ensures test isolation and prevents resource leaks.

## GitLab Soft-Delete Behavior

GitLab uses "soft deletes" for some resources:

- **Projects**: Marked for deletion with `marked_for_deletion_at` timestamp, may still be accessible
- **Users**: Blocked with `state = "blocked"` instead of being fully deleted

Tests should account for both soft-delete and hard-delete responses when verifying deletion.
