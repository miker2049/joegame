{ modulesPath, config, lib, pkgs, ip6addr, ... }: {
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    (modulesPath + "/profiles/qemu-guest.nix")
    ./joegame1-disks.nix
  ];
  boot.loader.grub = {
    # no need to set devices, disko will add all devices that have a EF02 partition to the list already
    # devices = [ ];
    efiSupport = true;
    efiInstallAsRemovable = true;
  };
  services.openssh.enable = true;
  services.openssh.ports = [ 36044 ];
  services.fail2ban.enable = true;
  services.tailscale.enable = true;

  environment.systemPackages =
    map lib.lowPrio [ pkgs.curl pkgs.gitMinimal pkgs.vim ];

  users.users.root.openssh.authorizedKeys.keys = [
    # change this to your ssh key
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAjB5b57tRpdHygEN7cX0LDLxsP7FEtxkfXKoOlBTtvy mik@framework"
  ];
  networking.firewall.allowedTCPPorts = [ 80 443 36044 ];
  networking.useNetworkd = true;
  systemd.network.enable = true;

  systemd.network.networks."10-wan" = {
    matchConfig.Name = "enp1s0"; # either ens3 (amd64) or enp1s0 (arm64)
    networkConfig.DHCP = "ipv4";
    address = [
      # replace this address with the one assigned to your instance
      ip6addr
    ];
    dns = [
      # nat64.net
      "2a01:4f8:c2c:123f::1"
      "2a00:1098:2b::1"
      "2a00:1098:2c::1"
      # hetzner
      "2a01:4ff:ff00::add:2"
      "2a01:4ff:ff00::add:1"
    ];
    routes = [{ Gateway = "fe80::1"; }];
  };

  nix = {
    # gc = {
    #   automatic = true;
    #   dates = "weekly";
    #   options = "--delete-older-than 30d";
    # };

    settings = {
      experimental-features = [ "nix-command" "flakes" ];
      auto-optimise-store = true;
      substituters =
        [ "https://nix-community.cachix.org" "https://cache.nixos.org/" ];
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
    };
  };

  system.stateVersion = "24.05";
}
