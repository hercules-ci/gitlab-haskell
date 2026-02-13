# Integration Tests

This directory contains integration tests for the gitlab-haskell library.

## Test Environment

The integration tests use a NixOS VM running GitLab for testing. The VM configuration is defined in `flake.nix` (`nixosConfigurations.gitlab-dev`) and `gitlab-module.nix`.

### VM Configuration

- **Root password**: `glhs-insecure-test-password` (configured via `GITLAB_ROOT_PASSWORD`)
- **Default user**: root
- **Port forwarding**: VM binds to host ports
  - 8427 on the host for HTTP
  - 8422 on the host for SSH (may / may not be used)

### Running the VM

From the repository root:

```bash
nix run .#gitlab-dev-vm
```

The VM will:
1. Start up and initialize GitLab
2. Wait for GitLab to become ready
3. Display "GitLab is ready!" when initialization is complete

The VM creates and/or uses `nixos.qcow2` in the working directory.
Maybe it could be made to be stateless.

### Running the Tests

When the VM is running, you can run the tests outside the VM, to iterate quickly without gitlab startup overhead, which is significant.
In a separate terminal, once the VM is ready:

```bash
cabal run integration-suite
```

The tests will:
1. Check for `GITLAB_TOKEN` environment variable
2. If not set, obtain an OAuth token via password grant flow
3. Run the test suite against the GitLab instance

### Environment Variables

- `GITLAB_URL`: GitLab instance URL (default: `http://localhost:8427`)
- `GITLAB_TOKEN`: OAuth access token (if not set, automatically obtained)
- `GITLAB_PASSWORD`: Root password (default: `glhs-insecure-test-password`)

### Nix sandboxed

You may also run the tests in the Nix build sandbox.
This has the overhead of having to start GitLab, but proves that the test is self-contained, and is useful for CI.

```bash
nix build .#integrationTest
```

## Test Suite Structure

See [suite](suite/README.md).
