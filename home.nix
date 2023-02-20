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

  fonts.fontconfig.enable = true;

  manual.html.enable = true;
  manual.json.enable = true;

  programs.git.enable = true;
  programs.git.userEmail = "chrisregenboog@protonmail.com";
  programs.git.userName = "Chris Regenboog";
  programs.git.difftastic.enable = true;
  programs.git.extraConfig = {
    core.autoclrf = true;
  };

  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = true;

  programs.fish = {
    enable = true;
    functions = {
      fish_greeting.body = "";
    };
    interactiveShellInit = ''
      fish_vi_key_bindings
    '';
    plugins = [
      { name = "pisces"; src = pkgs.fishPlugins.pisces.src; }
      { name = "fzf-fish"; src = pkgs.fishPlugins.fzf-fish.src; }
    ];
  };

  qt.enable = true;

  programs.gpg.enable = true;
  services.gpg-agent.enable = true;

  programs.rofi = {
    enable = true;
    theme = "DarkBlue";
    font = "JetBrainsMono Nerd Font 40";
  };
  
  home.packages = with pkgs; [
    glances
    graphviz
    xdot
    sshfs
    pandoc
    httpie
    wireshark
    neofetch
    imagemagick
    mediainfo
    ffmpegthumbnailer
    virt-manager
    # ltex-ls
    exercism
    warpd
    rnix-lsp
    qmk
    via # keyboard config tool
    maim # screenshot tool

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
    qbittorrent

    # Window Manager
    dmenu
    haskellPackages.xmobar
    feh
    nitrogen

    # Browser
    qutebrowser
    brave
    nyxt
    
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
    nodePackages.tailwindcss

    # Elm
    elmPackages.elm
    elmPackages.elm-language-server
    elmPackages.elm-format
    elmPackages.elm-test
    elmPackages.elm-review
    elmPackages.elm-live

    # Haskell
    haskell-language-server
    (haskellPackages.ghcWithPackages (p: [ p.xmonad p.xmonad-contrib ] ))
    stack

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


  xdg = {
    mimeApps.enable = true;
    userDirs = {
      enable = true;
      createDirectories = true;
    };
  };

  services.betterlockscreen = {
    enable = true;
    inactiveInterval = 60;
  };

  programs.zathura.enable = true;

  nixpkgs.config.allowUnfree = true;

  services.polybar.enable = true;
  services.polybar.script = "polybar top &";
  services.polybar.config = ./polybar.ini;
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

  programs.helix.enable = true;

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
      nvim-treesitter
    ];
  };

  programs.vscode = {
    enable = true;
  };

  services.syncthing.enable = true;
  services.syncthing.tray.enable = true;

  programs.starship.enable = true;

  programs.exa.enable = true;

  programs.kitty = {
    enable = true;
    settings = {
      enable_audio_bell = false;
      font_size = 22;
      font_family = "JetBrainsMono Nerd Font";
      shell_integration = true;
    };
    theme = "Modus Vivendi";
  };

  home.sessionPath = [
    "$HOME/.cargo/bin"
  ];

  services.picom = {
    enable = true;
    fade = true;
    backend = "glx";
    inactiveOpacity = 0.9;
    # inactiveDim = "0.1";
    # blur = true;
    shadow = true;
    vSync = true;
    opacityRules = [
        "100:class_g = 'i3lock'"
        "100:class_g = 'brave-browser'"
    ];
  };

  home.file.".xmonad/xmobar.hs".source = ./xmobar.hs;
  home.file.".config/qutebrowser/config.py".source = ./qutebrowser/config.py;
}
