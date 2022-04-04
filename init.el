(require 'package)
(setq package-archives nil)
(package-initialize)
(require 'use-package)

(setq standard-indent 2)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

(use-package evil
    :config
    (evil-mode)
)

(use-package kaolin-themes
    :config
    (load-theme 'kaolin-aurora t)
)

(use-package all-the-icons)

(use-package nix-mode
    :mode "\\.nix\\'")

(use-package org)
