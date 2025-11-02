{
  description = "A Haskell library for the GitLab web API";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      imports = [ inputs.haskell-flake.flakeModule ];

      perSystem = { self', ... }: {
        haskellProjects.default = {
        };

        packages.default = self'.packages.gitlab-haskell;
      };
    };
}
