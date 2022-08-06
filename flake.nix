{
  description = "System configurations flake";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs;
    home-manager.url = github:nix-community/home-manager;
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, home-manager, ... }@attrs:
    let
      stateVersion = "22.05";
    in {
      nixosConfigurations.desktop-home = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = attrs // { inherit stateVersion; };
        modules = [
          ./configuration.nix
          home-manager.nixosModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.extraSpecialArgs = { inherit stateVersion; };
            home-manager.users.chris = import ./home.nix;
          }
        ];
      };
    };
}
