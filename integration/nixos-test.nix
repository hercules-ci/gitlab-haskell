# Run with:
#   nix build .#integrationTest

{
  # Access `perSystem`
  # https://flake.parts/module-arguments#withsystem
  withSystem,
  ...
}:

{
  # https://nixos.org/manual/nixos/unstable/#sec-nixos-tests
  _class = "nixosTest";

  name = "gitlab-haskell-integration";

  # https://nixos.org/manual/nixos/unstable/#test-opt-nodes
  nodes = {
    gitlab =
      # https://search.nixos.org/options
      { pkgs, ... }:
      {
        imports = [ ./gitlab-module.nix ];

        networking.firewall.allowedTCPPorts = [ 80 ];
      };

    tester =
      # https://search.nixos.org/options
      { pkgs, ... }:
      {
        environment.systemPackages = [
          # Get the test suite for the guest system (pkgs), e.g. still linux even on darwin host
          (withSystem pkgs.stdenv.hostPlatform.system (
            # Module args of https://flake.parts/options/flake-parts.html#opt-perSystem
            { config, ... }: config.packages.integration-suite
          ))
        ];
        environment.variables.GITLAB_URL = "http://gitlab";
      };
  };

  # https://nixos.org/manual/nixos/unstable/#test-opt-testScript
  testScript = ''
    start_all()

    # Wait for GitLab services to be ready
    gitlab.wait_for_unit("gitaly.service")
    gitlab.wait_for_unit("gitlab-workhorse.service")
    gitlab.wait_for_unit("gitlab.service")
    gitlab.wait_for_unit("gitlab-sidekiq.service")
    gitlab.wait_for_file("/run/gitlab/gitlab-workhorse.socket")
    gitlab.wait_until_succeeds("curl -v --fail http://localhost/users/sign_in")

    # Run the integration test suite (it will obtain its own OAuth token)
    tester.wait_for_unit("multi-user.target")
    tester.succeed("integration-suite")
  '';
}
