# NixOS module with GitLab for gitlab-haskell integration tests
# This is included in the `nix run .#gitlab-dev-vm` VM as well as the sandboxed
# NixOS VM test (`nix build .#integrationTest`)

{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.services.gitlab;
in

{
  # https://search.nixos.org/options
  _class = "nixos";

  config = {
    # Guest-level firewall
    networking.firewall.allowedTCPPorts = [ 80 ];

    virtualisation.memorySize = lib.mkDefault 6144;
    virtualisation.cores = lib.mkDefault 8;
    virtualisation.useNixStoreImage = lib.mkDefault true;
    virtualisation.writableStore = lib.mkDefault false;

    # Generate GitLab secrets before services start
    systemd.services.install-gitlab-files = {
      path = [ pkgs.libressl ];
      before = [
        "gitlab.service"
        "gitaly.service"
        "gitlab-workhorse.service"
        "gitlab-sidekiq.service"
      ];
      wantedBy = [ "multi-user.target" ];
      script = ''
        mkdir -p /var/lib/gitlab-secrets
        [ -f /var/lib/gitlab-secrets/secret ] || tr -cd a-zA-Z0-9 </dev/urandom | head -c 72 > /var/lib/gitlab-secrets/secret
        [ -f /var/lib/gitlab-secrets/db ] || tr -cd a-zA-Z0-9 </dev/urandom | head -c 72 > /var/lib/gitlab-secrets/db
        [ -f /var/lib/gitlab-secrets/otp ] || tr -cd a-zA-Z0-9 </dev/urandom | head -c 72 > /var/lib/gitlab-secrets/otp
        [ -f /var/lib/gitlab-secrets/jws ] || openssl genrsa 2048 > /var/lib/gitlab-secrets/jws
        [ -f /var/lib/gitlab-secrets/active-record-primary ] || tr -cd a-zA-Z0-9 </dev/urandom | head -c 32 > /var/lib/gitlab-secrets/active-record-primary
        [ -f /var/lib/gitlab-secrets/active-record-deterministic ] || tr -cd a-zA-Z0-9 </dev/urandom | head -c 32 > /var/lib/gitlab-secrets/active-record-deterministic
        [ -f /var/lib/gitlab-secrets/active-record-salt ] || tr -cd a-zA-Z0-9 </dev/urandom | head -c 32 > /var/lib/gitlab-secrets/active-record-salt
      '';
    };

    # GitLab configuration
    # https://nixos.org/manual/nixos/unstable/#module-services-gitlab
    # https://search.nixos.org/options?query=services.gitlab
    services.postgresql.package = pkgs.postgresql_16;
    services.gitlab.enable = true;
    services.gitlab.https = false;
    services.gitlab.port = if cfg.https then 443 else 80;
    services.gitlab.initialRootPasswordFile = pkgs.writeText "unsafe-root-pass" "glhs-insecure-test-password";
    services.gitlab.secrets.secretFile = "/var/lib/gitlab-secrets/secret";
    services.gitlab.secrets.dbFile = "/var/lib/gitlab-secrets/db";
    services.gitlab.secrets.otpFile = "/var/lib/gitlab-secrets/otp";
    services.gitlab.secrets.jwsFile = "/var/lib/gitlab-secrets/jws";
    services.gitlab.secrets.activeRecordPrimaryKeyFile = "/var/lib/gitlab-secrets/active-record-primary";
    services.gitlab.secrets.activeRecordDeterministicKeyFile = "/var/lib/gitlab-secrets/active-record-deterministic";
    services.gitlab.secrets.activeRecordSaltFile = "/var/lib/gitlab-secrets/active-record-salt";
    services.gitlab.extraEnv = {
      # Reduce log verbosity
      GITLAB_LOG_LEVEL = "warn";
    };
    systemd.services.gitlab-sidekiq.serviceConfig.LogFilterPatterns = [
      "~wiki/Best-Practices"
      ''~"severity":"INFO"''
      ''~"message":"No existing merge request to be cleaned up."''
    ];
    systemd.services.gitlab-workhorse.serviceConfig.LogFilterPatterns = [
      ''~ HTTP/1.1" 2''
    ];
    systemd.services.gitaly.serviceConfig.LogFilterPatterns = [
      "~ level=info "
    ];

    # Nginx reverse proxy
    # https://search.nixos.org/options?query=services.nginx
    services.nginx = {
      enable = true;
      recommendedGzipSettings = true;
      recommendedOptimisation = true;
      recommendedProxySettings = true;
      recommendedTlsSettings = true;
      virtualHosts."localhost" = {
        locations."/" = {
          proxyPass = "http://unix:/run/gitlab/gitlab-workhorse.socket";
          extraConfig = lib.mkIf cfg.https ''
            proxy_set_header X-Forwarded-Proto https;
            proxy_set_header X-Forwarded-Ssl on;
          '';
        };
      };
    };

    # Stub sendmail to silence email errors
    security.wrappers.sendmail = {
      source = pkgs.writeShellScript "sendmail" ''
        # Discard all mail silently
        cat > /dev/null
      '';
      owner = "root";
      group = "root";
    };

    # No package manager needed
    nix.enable = lib.mkDefault false;
  };
}
