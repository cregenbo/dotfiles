{ config, pkgs, stdenv, ... }:

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
    oh-my-zsh.enable = true;
    oh-my-zsh.plugins = [ "git" "sudo" "aws" "vi-mode" ];
    shellAliases = {
      man = "batman";
      ssh = "kitty +kitten ssh";
      emacsclient = "TERM=xterm; emacsclient";
      e = "emacsclient -c";
      ls = "exa --all --icons --colour-scale";
      lt = "ls --tree";
      llt = "ll --tree";
    };
  };

  fonts.fontconfig.enable = true;

  programs.fzf.enable = true;

  qt.enable = true;
  gtk.enable = true;
  
  home.packages = with pkgs; [
    qutebrowser
    dmenu
    anki-bin
    nerdfonts
    htop
    btop
    bat
    bat-extras.batman
    ripgrep
    fd
    zsh-you-should-use
    graphviz
    xdot
    sshfs
    elmPackages.elm
    hcloud
    awscli2
    pandoc
    httpie
    i3lock
  ];

  services.screen-locker = {
    enable = true;
    lockCmd = "${pkgs.i3lock}/bin/i3lock -n -c 000000";
    inactiveInterval = 30;
  };

  programs.zathura.enable = true;

  nixpkgs.config.allowUnfree = true;

  nixpkgs.overlays = [
    (import (builtins.fetchGit {
      url = "https://github.com/nix-community/emacs-overlay.git";
      ref = "master";
      rev = "46353b3bce539bd99eace5584f804320661ec18a";
    }))
  ];

  services.emacs.enable = true;
  services.emacs.defaultEditor = true;
  programs.emacs = {
    enable = true;
    package = pkgs.emacsGcc;
    extraPackages = epkgs: with epkgs; [
      magit
      avy
      pulsar
      helpful
      evil
      evil-collection
      evil-surround
      evil-nerd-commenter
      use-package
      all-the-icons
      modus-themes
      doom-modeline
      nix-mode
      org-roam
      anki-editor
      general
      corfu
      vertico
      marginalia
      consult
      embark
      orderless
      which-key
      no-littering
      projectile
      hydra
      lsp-mode
      lsp-ui
      yasnippet
      elm-mode
      rainbow-delimiters
      visual-fill-column
      org-bullets
      elfeed
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

  services.syncthing.enable = true;

  programs.starship.enable = true;

  programs.exa.enable = true;

  programs.kitty = {
    enable = true;
    settings = {
      enable_audio_bell = false;
      font_size = 13;
      font_family = "JetBrainsMono Nerd Font";
    };
  };

  home.sessionPath = [ 
    "$HOME/local/jetbrains"
  ];
  home.sessionVariables = {
    QT_XCB_GL_INTEGRATION = "none"; # Disable QT GLX, otherwise Anki won't start
  };
  home.file.".emacs.d/init.el".source = ./init.el;
  home.file.".xmonad/xmonad.hs".source = ./xmonad.hs;
  home.file.".xmonad/xmobar.hs".source = ./xmobar.hs;
  # home.file.".config/kitty/kitty.conf".source = ./kitty.conf;
  home.file.".config/picom/picom.conf".source = ./picom.conf;
  home.file.".config/qutebrowser/config.py".source = ./qutebrowser/config.py;
}
