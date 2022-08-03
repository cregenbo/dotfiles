{ config, pkgs, stdenv, stateVersion, ... }:

{
  imports = [
    ./modules/gtk.nix
    ./modules/gaming.nix
    ./modules/emacs.nix
  ];

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
  home.stateVersion = stateVersion;

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  home.shellAliases = {
    g = "git";
    man = "batman";
    ssh = "kitty +kitten ssh";
    ls = "exa --all --icons --colour-scale";
    lt = "ls --tree";
    llt = "ll --tree";
    ncdu = "ncdu --color dark";
  };

  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = true;

  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    enableSyntaxHighlighting = true;
    history.size = 100000;
    history.path = "${config.xdg.dataHome}/zsh/history";
    oh-my-zsh.enable = true;
    oh-my-zsh.plugins = [ "git" "sudo" "aws" "vi-mode" ];
  };

  programs.fzf.enable = true;

  qt.enable = true;
  
  home.packages = with pkgs; [
    zsh-you-should-use
    graphviz
    xdot
    sshfs
    pandoc
    httpie
    jetbrains.phpstorm

    # Fonts
    nerdfonts
    fontpreview

    # Desktop Apps
    anki-bin
    megasync
    mpv
    pavucontrol
    pcmanfm
    gnome.gnome-terminal
    libsForQt5.konsole
    glxinfo
    go-sct

    # Window Manager
    i3lock
    dmenu
    haskellPackages.xmobar
    feh

    # Wallpaper manager
    variety

    # Browser
    qutebrowser
    brave
    
    # Database
    jetbrains.datagrip

    # Java
    jetbrains.idea-ultimate
    jdk

    # Rust
    rustc
    cargo
    rust-analyzer
    rustfmt
    clippy
    gcc # Rust needs cc linker
    binutils # Cargo needs ar
    webkitgtk
    pkg-config
    dbus

    # Web dev
    jetbrains.webstorm

    # Cloud
    hcloud
    awscli2
    terraform
  ];

  xsession = {
    enable = true;
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      config = ./xmonad.hs;
    };
  };

  services.screen-locker = {
    enable = true;
    lockCmd = "${pkgs.i3lock}/bin/i3lock -n -c 000000";
    inactiveInterval = 30;
  };

  programs.zathura.enable = true;

  nixpkgs.config.allowUnfree = true;

  # services.polybar.enable = true;
  # services.polybar.script = "polybar top &";
  # services.polybar.settings = {
  #   "bar/top" = {
  #     modules-right = "date";
  #     width = "100%";
  #     tray-position = "right";
  #     font-0 = "JetBrainsMono Nerd Font:size=20";
  #   };

  #   "module/date" = {
  #     type = "internal/date";
  #     date = "%d.%m.%y";
  #     time = "%H:%M";
  #   };
  # };


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
      font_size = 20;
      font_family = "JetBrainsMono Nerd Font";
    };
    theme = "Space Gray Eighties";
  };

  home.sessionPath = [
    "$HOME/.cargo/bin"
  ];

  services.picom = {
    enable = true;
    fade = true;
    backend = "glx";
    inactiveOpacity = "0.8";
    inactiveDim = "0.1";
    blur = true;
    shadow = true;
    vSync = true;
  };

  home.file.".xmonad/xmobar.hs".source = ./xmobar.hs;
  home.file.".config/qutebrowser/config.py".source = ./qutebrowser/config.py;
}
