(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(require 'use-package)
(setq use-package-always-ensure t)

(setq standard-indent 2)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(set-fringe-mode 20)

(setq visible-bell t)

(use-package super-save
  :init
  (setq super-save-auto-save-when-idle t)
  (setq super-save-idle-duration 1)
  (setq auto-save-default nil)
  :config
  (super-save-mode +1))

(set-face-attribute 'default nil :family "JetBrainsMono Nerd Font" :height 130)
(set-face-attribute 'fixed-pitch nil :family "JetBrainsMono Nerd Font" :height 130)
(set-face-attribute 'variable-pitch nil :family "Ubuntu Nerd Font" :height 140)

(use-package dashboard
  :init
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-center-content t)
  (setq dashboard-set-heading-icons t) 
  (setq dashboard-set-file-icons t) 
  (setq dashboard-set-navigator t)
  (setq dashboard-items '((recents . 5) (projects . 5) (agenda . 5)))
  :config
  (dashboard-setup-startup-hook))

(use-package no-littering)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-redo)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package all-the-icons)

(use-package doom-modeline
  :config
  (doom-modeline-mode))

(use-package nix-mode)

(use-package modus-themes
  :config
  (modus-themes-load-vivendi))

(use-package general
  :after evil
  :config
  (general-create-definer dotfiles/leader
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))

(use-package vertico
  :config
  (vertico-mode))

(use-package marginalia
  :config
  (marginalia-mode))

(use-package consult)

(use-package corfu
  :init
  (setq corfu-auto t)
  :config
  (global-corfu-mode))

(use-package diminish)

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :init
  (setq which-key-idle-delay 0.0)
  :config
  (which-key-mode))

(use-package avy)

(defun dotfiles/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1)
  )

(use-package org
  :hook (org-mode . dotfiles/org-mode-setup)
  :init
  ;;(setq org-default-notes-file (concat org-directory "/notes.org"))
  )

(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.5))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.4))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.3))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.2))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.1)))))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package org-roam
  :after org
  :init
  (setq org-roam-directory (concat org-directory "/roam"))
  (setq org-roam-mode-sections
	(list #'org-roam-backlinks-section
	      #'org-roam-reflinks-section
	      #'org-roam-unlinked-references-section))
  (add-to-list 'display-buffer-alist
	       '("\\*roam\\*"
		 (display-buffer-in-direction)
		 (direction . right)
		 (window-width . 0.33)
		 (window-height .fit-window-to-buffer)))
  (setq org-roam-dailies-directory "daily/")
  (setq org-roam-dailies-capture-templates
	'(
	  ("d" "default" entry
           "* %?"
           :target (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n"))
	  ("t" "default" entry
 
           :target (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n"))
	  ))
  :config
  (org-roam-db-autosync-mode))

(use-package helpful)

(use-package hydra
  :defer t)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(defhydra hydra-window-scale (:timeout 4)
  "scale window"
  ("j" shrink-window "decrease vertically")
  ("k" enlarge-window "increase vertically")
  ("h" shrink-window-horizontally "decrease horizontally")
  ("l" enlarge-window-horizontally "increase horizontally")
  ("f" nil "finished" :exit t))

(use-package projectile
  :config
  (projectile-mode 1))

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package pulsar
  :config
  (pulsar-global-mode 1)
  (advice-add 'evil-next-line :after 'pulsar-pulse-line)
  (advice-add 'evil-previous-line :after 'pulsar-pulse-line)
  )


(defun dotfiles/org-mode-visual-fill ()
  (setq visual-fill-column-width 120
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . dotfiles/org-mode-visual-fill))

(use-package kbd-mode
  :load-path "~/.emacs.d/elisp/")

(use-package rustic)

(general-def
  :states '(normal motion visual)
  :keymaps 'rust-mode-map
  :prefix "SPC"
  "m" '(:ignore t :which-key "rust-mode")
  "mt" 'rustic-cargo-test
  "ma" 'lsp-execute-code-action
  "md" 'lsp-describe-thing-at-point
  "mf" 'rustic-format-dwim
  )

(use-package magit)

(use-package haskell-mode)

(use-package lsp-mode
  :hook (
	 (haskell-mode . lsp)
	 (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred))

(use-package lsp-ui :commands lsp-ui-mode)

(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package dap-mode)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package evil-nerd-commenter)

(general-def
  :states '(normal motion visual)
  :keymaps 'override
  :prefix "SPC"
  :global-prefix "C-SPC"
  "b" '(:ignore t :which-key "buffer")
  "bb" 'consult-buffer
  "bp" 'previous-buffer
  "bn" 'next-buffer
  "bk" 'kill-buffer
  "bs" 'save-buffer
  "p" '(:ignore t :which-key "project")
  "pf" 'projectile-find-file
  "pp" 'projectile-switch-project
  "w" '(:ignore t :which-key "window")
  "wo" 'delete-other-windows
  "wd" 'evil-window-delete
  "wl" 'evil-window-right
  "wh" 'evil-window-left
  "wk" 'evil-window-up
  "wj" 'evil-window-down
  "wa" '(hydra-window-scale/body :which-key "scale-window")
  "ws" '(:ignore t :which-key "split")
  "wss" 'evil-window-vsplit
  "wsh" 'evil-window-split
  "h" '(:ignore t :which-key "help")
  "hf" 'describe-function
  "hk" 'helpful-key
  "hs" 'helpful-symbol
  "hd" 'shortdoc-display-group
  "hm" 'describe-mode
  "hp" 'describe-package
  "q" '(:ignore t :which-key "quit")
  "qq" 'evil-save-and-quit
  "o" '(:ignore t :which-key "org")
  "oc" 'org-roam-capture
  "oa" 'org-agenda
  "os" 'org-store-link
  "ot" 'org-todo
  "of" 'org-roam-node-find
  "od" '(:ignore t :which-key "dailies")
  "odf" 'org-roam-dailies-find-today
  "odc" 'org-roam-dailies-capture-today
  "j" '(:ignore t :which-key "jump")
  "jj" '((lambda () (interactive) (evil-avy-goto-char)) :which-key "evil-avy-goto-char")
  "jg" 'consult-ripgrep
  "g" '(:ignore t :which-key "git")
  "gg" 'magit
  "l" 'ielm
  "c" 'evilnc-comment-or-uncomment-lines
  "f" 'find-file
  "e" 'eval-buffer
  ";" 'execute-extended-command
  "\\" 'indent-region
  "v" 'check-parens
  "s" '(hydra-text-scale/body :which-key "scale-text"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(zen-mode lsp-haskell lsp-ui dap-mode lsp-mode evil-nerd-commenter rainbow-delimiters rustic haskell-mode yasnippet which-key visual-fill-column vertico use-package super-save pulsar projectile org-roam org-bullets orderless no-littering nix-mode modus-themes marginalia hydra helpful general evil-surround evil-collection doom-modeline diminish dashboard corfu consult avy all-the-icons)))

