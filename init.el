(require 'package)
(setq package-archives nil)
(package-initialize)
(require 'use-package)

(setq standard-indent 2)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

(setq visible-bell t)

(column-number-mode)
(global-display-line-numbers-mode t)

(require 'no-littering)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode))

(use-package evil-collection
  :after evil
  :init
  (evil-collection-init))

(require 'evil-surround)
(global-evil-surround-mode +1)

(load-theme 'kaolin-aurora t)
(doom-modeline-mode)

(use-package all-the-icons)

(use-package nix-mode
    :mode "\\.nix\\'")

(use-package org)

(use-package general
  :after evil
  :config
  (general-create-definer dotfiles/leader
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"))

(use-package corfu
  :custom
  (corfu-auto t)
  :init
  (corfu-global-mode))

(use-package vertico
  :init
  (vertico-mode))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package consult)

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))

(setq which-key-idle-delay 0.0)

(require 'which-key)
(which-key-mode)

(require 'avy)

(dotfiles/leader
  "b" '(:ignore t :which-key "buffer")
  "bb" 'switch-to-buffer
  "bp" 'previous-buffer
  "bn" 'next-buffer
  "bk" 'kill-buffer
  "bs" 'save-buffer
  "w" '(:ignore t :which-key "window")
  "wo" 'delete-other-windows
  "wl" 'evil-window-right
  "j" '((lambda () (interactive) (evil-avy-goto-char)) :which-key "evil-avy-goto-char")
  "c" 'evilnc-comment-or-uncomment-lines
  "f" 'find-file
  "e" 'eval-buffer
  ";" 'execute-extended-command
  )

(defun savebuf(begin end length)
  (if (and (buffer-file-name) (buffer-modified-p))
       (save-buffer)))
(add-hook 'after-change-functions 'savebuf)
