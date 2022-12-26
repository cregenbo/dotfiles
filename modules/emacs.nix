{ pkgs, ... } :

{
  services.emacs.enable = true;
  services.emacs.defaultEditor = true;
  programs.emacs = {
    enable = true;
    package = pkgs.emacsNativeComp;
    extraPackages = epkgs: with epkgs; [
      # Miscellaneous
      elfeed
      pdf-tools
      dirvish
      json-mode
      yaml-mode
      dashboard
      lsp-ltex
      flycheck
      consult-flycheck

      # Core
      no-littering
      super-save
      helpful
      orderless
      avy
      corfu
      vertico
      marginalia
      consult
      embark
      embark-consult
      which-key

      # Keybindings
      general
      evil
      evil-collection
      evil-surround
      evil-nerd-commenter
      hydra

      # Org
      org-roam
      org-bullets
      anki-editor

      # Eye Candy
      visual-fill-column
      pulsar
      modus-themes
      diminish
      doom-modeline
      all-the-icons
      rainbow-delimiters

      # Dev
      yasnippet
      magit
      projectile
      lsp-mode
      lsp-ui
      envrc
      elm-mode
      nix-mode
      rustic
      haskell-mode
    ];
  };

  home.packages = with pkgs; [
    emacs-all-the-icons-fonts
  ];


  home.file.".emacs.d/init.el".source = ./init.el;
  home.shellAliases = {
    # emacsclient = "TERM=xterm; emacsclient";
    e = "emacsclient -c";
  };
}
