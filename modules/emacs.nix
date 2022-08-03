{ pkgs, ... } :

{
  services.emacs.enable = true;
  services.emacs.defaultEditor = true;
  programs.emacs = {
    enable = true;
    package = pkgs.emacsNativeComp;
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
      diminish
      super-save

      rustic
    ];
  };

  home.file.".emacs.d/init.el".source = ./init.el;
  home.shellAliases = {
    emacsclient = "TERM=xterm; emacsclient";
    e = "emacsclient -c";
  };
}
