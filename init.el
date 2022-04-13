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

;; (column-number-mode)
;; (global-display-line-numbers-mode t)

(set-face-attribute 'default nil :height 130)

(use-package no-littering)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-keybinding nil)
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
  :init
  (modus-themes-load-themes)
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
  (corfu-global-mode))

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :init
  (setq which-key-idle-delay 0.0)
  :config
  (which-key-mode))

(use-package avy)

(use-package org
  :init
  (setq org-default-notes-file (concat org-directory "/notes.org")))

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
           "* foo bar baz test %?"
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

(use-package projectile
  :config
  (projectile-mode 1))

(use-package yasnippet
  :config
  (yas-global-mode 1))

(dotfiles/leader
  "b" '(:ignore t :which-key "buffer")
  "bb" 'consult-buffer
  "bp" 'previous-buffer
  "bn" 'next-buffer
  "bk" 'kill-buffer
  "bs" 'save-buffer
  "w" '(:ignore t :which-key "window")
  "wo" 'delete-other-windows
  "wl" 'evil-window-right
  "wh" 'evil-window-left
  "wk" 'evil-window-up
  "wj" 'evil-window-down
  "h" '(:ignore t :which-key "help")
  "hf" 'describe-function
  "hk" 'helpful-key
  "hs" 'helpful-symbol
  "q" '(:ignore t :which-key "quit")
  "qq" 'evil-save-and-quit
  "o" '(:ignore t :which-key "org")
  "oc" 'org-roam-capture
  "oa" 'org-agenda
  "os" 'org-store-link
  "ot" 'org-todo
  "od" '(:ignore t :which-key "dailies")
  "odf" 'org-roam-dailies-find-today
  "odc" 'org-roam-dailies-capture-today
  "j" '(:ignore t :which-key "jump")
  "jj" '((lambda () (interactive) (evil-avy-goto-char)) :which-key "evil-avy-goto-char")
  "jg" 'consult-ripgrep
  "c" 'evilnc-comment-or-uncomment-lines
  "f" 'find-file
  "e" 'eval-buffer
  ";" 'execute-extended-command
  "\\" 'indent-region
  "v" 'check-parens
  "s" '(hydra-text-scale/body :which-key "scale-text")
  )

(defun savebuf(begin end length)
  (if (and (buffer-file-name) (buffer-modified-p))
       (save-buffer)))
(add-hook 'after-change-functions 'savebuf)

(use-package pulsar
  :init
  (setq pulsar-pulse-functions
	'(evil-previous-line evil-next-line))
  :config
  (pulsar-global-mode 1))

(defun dotfiles/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . dotfiles/org-mode-visual-fill))
