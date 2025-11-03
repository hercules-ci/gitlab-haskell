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

### Running Tests Selectively

The test suite uses hspec, which supports filtering tests via command-line options.

**How `--match` works**: Substring matching on the full test path (e.g., `"Issue operations/can move an issue to a different project"`).

Examples:
```bash
# Match by describe block
cabal run integration-suite -- --match "Issue operations"

# Match by test name substring
cabal run integration-suite -- --match "can create"

# Match across path boundaries using /
cabal run integration-suite -- --match "Issue operations/move"
cabal run integration-suite -- --match "operations/can"

# Exact test path (useful for rerunning a single test)
cabal run integration-suite -- --match "/GitLab API calls/Issue operations/can move an issue to a different project/"

# Other useful options
cabal run integration-suite -- --skip "404"        # Skip matching tests
cabal run integration-suite -- --dry-run           # Show what would run
cabal run integration-suite -- --fail-fast         # Stop on first failure
cabal run integration-suite -- --rerun             # Rerun failed tests
cabal run integration-suite -- -j4                 # Run with 4 parallel jobs
cabal run integration-suite -- --help              # Show all options
```

Patterns are case-sensitive. The `/` is a literal character in the path, not special syntax.

## Test suite structure

Tests are organized in `src/Test/GitLab/API/` mirroring the GitLab API structure.

Helper modules provide reusable test infrastructure:

- `Test.Helpers.Environment`: GitLab environment configuration (URL, password, OAuth token)
- `Test.Helpers.Assertions`: Test assertion helpers (`expectRight`, `expectLeft`, `expectJust`, `showing`, `shouldBe*`)
- `Test.Helpers.Fixtures`: Resource management with bracket pattern (`withProject`, `withUser`, `withGroup`, `withIssue`)

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
- `withGroup`: Creates a group, runs a test, and cleans up
- `withIssue`: Creates an issue within a project, runs a test, and cleans up

**Important principles**:

1. **Cleanup MUST succeed**: All `with*` fixtures require successful deletion. This tests delete operations and ensures proper cleanup.
2. **No lenient cleanup by default**: We do not accept 404s or ignore errors during cleanup. This means:
   - Fixtures actively test that delete operations work
   - If you need to test delete explicitly within the fixture scope, return the resource and continue testing after the fixture completes
   - If future tests need alternate deletion flows (e.g., multi-step deletion), consider adding explicit lenient variants like `withIssueLenient` rather than changing the default behavior
3. **Test isolation**: Each test gets fresh resources and must clean up completely to avoid interfering with other tests.

### Testing Both Success and Failure

Test both positive and negative cases:

- **404 handling**: Test that non-existent resources return appropriate errors
- **Invalid IDs**: Test edge cases like ID 0 or negative IDs
- **State verification**: After mutations, read back the resource to verify changes persisted
- **Unexpected responses are opportunities**: When the API returns something unexpected (like 304 instead of 200), add a test for it rather than changing the test. These edge cases document important API behavior.

### Multi-Resource Tests

For tests requiring multiple resources (e.g., moving issues between projects):

- Nest `with*` fixtures: `withProject $ \project1 -> withProject $ \project2 -> ...`
- Each fixture manages its own lifecycle independently
- Cleanup happens in reverse order (LIFO)
