{ config, pkgs, ... }:

{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "chris";
  home.homeDirectory = "/home/chris";

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "22.05";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    enableSyntaxHighlighting = true;
    history.size = 100000;
    history.path = "${config.xdg.dataHome}/zsh/history";
    shellAliases = {
      man = "batman";
      ssh = "kitty +kitten ssh";
      emacsclient = "TERM=xterm; emacsclient";
    };
  };

  programs.fzf.enable = true;

  home.packages = with pkgs; [
    btop
    bat
    bat-extras.batman
    ripgrep
    fd
  ];

  nixpkgs.config.allowUnfree = true;
  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
    }))
  ];

  services.emacs.enable = true;
  programs.emacs = {
    enable = true;
    package = pkgs.emacsGcc;
    extraPackages = epkgs: with epkgs; [
      magit
      evil
      use-package
      all-the-icons
      kaolin-themes
      nix-mode
      org
    ];
  };

  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;
    plugins = with pkgs.vimPlugins; [
      vim-nix
      nvim-cmp
      comment-nvim
      vim-airline
    ];
  };

  programs.starship.enable = true;

  programs.exa.enable = true;
  programs.exa.enableAliases = true;

  home.sessionPath = [ 
    "$HOME/local/jetbrains"
  ];
  home.sessionVariables = {
    EDITOR = "nvim";
  };
  home.file.".emacs.d/init.el".source = ./init.el;
}
