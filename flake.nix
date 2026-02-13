{
  description = "A Haskell library for the GitLab web API";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    hercules-ci-effects.url = "github:hercules-ci/hercules-ci-effects";
  };

  outputs =
    inputs:
    # https://flake.parts
    inputs.flake-parts.lib.mkFlake { inherit inputs; } (
      { lib, withSystem, ... }:
      {
        systems = [
          "x86_64-linux"
          "aarch64-linux"
          "x86_64-darwin"
          "aarch64-darwin"
        ];
        herculesCI.ciSystems = [ "x86_64-linux" ];
        imports = [
          inputs.haskell-flake.flakeModule
          inputs.hercules-ci-effects.flakeModule
        ];

        # https://flake.parts/options/flake-parts.html#opt-perSystem
        perSystem =
          {
            self',
            pkgs,
            system,
            ...
          }:
          {
            # haskell-flake configuration
            # Docs: https://flake.parts/options/haskell-flake.html
            # Implicitly defines devShells.default
            haskellProjects.default = {
            };

            checks = {
              # Sandboxed VM test that runs the integration test suite against
              # an ephemeral instance over a virtual network.
              # This is mostly for CI, because the VM does not survive the build
              # sandbox. Run with `nix build .#integrationTest`, thanks to alias
              # in `packages`.
              integrationTest = pkgs.testers.runNixOSTest {
                imports = [ ./integration/nixos-test.nix ];
                _module.args.withSystem = withSystem;
              };
            };

            packages = {
              default = self'.packages.gitlab-haskell;

              # Convenience for nix build .#integrationTest
              integrationTest = self'.checks.integrationTest;
            };

            apps = {
              gitlab-dev-vm = {
                type = "app";
                program = "${lib.getExe inputs.self.nixosConfigurations.gitlab-dev.config.system.build.vm}";
                meta.description = ''
                  Runs a GitLab instance in a local VM
                  Leaves a nixos.qcow2 file in the working directory.
                  Run with:

                      nix run .#gitlab-dev-vm
                '';
              };
            };
          };

        flake.nixosConfigurations = {

          # NixOS configuration for VM with GitLab instance in it, for local dev.
          gitlab-dev = inputs.nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
            modules = [
              (
                { modulesPath, ... }:
                {
                  imports = [
                    "${modulesPath}/virtualisation/qemu-vm.nix"
                    ./integration/gitlab-module.nix
                  ];

                  # Appears in window name
                  networking.hostName = "gitlab-dev";

                  # Passwordless root login for local development
                  users.users.root.initialHashedPassword = "";
                  services.getty.autologinUser = "root";

                  # Install GitLab startup monitor script
                  environment.systemPackages = [
                    (inputs.nixpkgs.legacyPackages.x86_64-linux.writeShellScriptBin "wait-for-gitlab" ''
                      echo "Waiting for GitLab to become ready..."
                      while ! curl -sf http://localhost/users/sign_in > /dev/null 2>&1; do
                        sleep 2
                      done
                      echo "GitLab is ready!"
                    '')
                  ];

                  # Add hint to MOTD
                  users.motd = ''
                    GitLab development VM

                    Run 'wait-for-gitlab' to monitor GitLab startup.
                    Once ready, run tests from the host with: cabal run integration-suite
                  '';

                  # Forward GitLab HTTP and SSH ports to host (localhost only)
                  virtualisation.forwardPorts = [
                    {
                      from = "host";
                      host.address = "127.0.0.1";
                      host.port = 8427;
                      guest.port = 80;
                    }
                    {
                      from = "host";
                      host.address = "127.0.0.1";
                      host.port = 8422;
                      guest.port = 22;
                    }
                  ];

                  # Larger disk for development
                  virtualisation.diskSize = 8192;
                }
              )
            ];
          };
        };
      }
    );
}
