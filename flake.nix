{
  description = "My Home Manager configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    nixgl.url = "github:guibou/nixgl";
  };

  outputs = { home-manager, ... }:
    let
      system = "x86_64-linux";
      username = "chris";
    in {
      homeConfigurations.${username} = home-manager.lib.homeManagerConfiguration {
        configuration = import ./home.nix;

        inherit system username;
        homeDirectory = "/home/${username}";
        stateVersion = "22.05"
      };
  };
}
