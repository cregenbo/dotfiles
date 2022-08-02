{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-22.05;
    home-manager.url = github:nix-community/home-manager/release-22.05;
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, home-manager, ... }@attrs:
    let
      stateVersion = "22.05";
    in {
      nixosConfigurations.desktop-home = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = attrs // { inherit stateVersion; };
        modules = [ ./configuration.nix ];
      };
    };
}
