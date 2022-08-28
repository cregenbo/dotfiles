(require 'package)
(setq package-archives nil)
(package-initialize)

(setq standard-indent 2)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(set-fringe-mode 50)

(setq visible-bell t)

(require 'no-littering)

(auto-save-mode -1)
(setq super-save-auto-save-when-idle t)
(setq super-save-idle-duration 1)
(super-save-mode +1)

;; (auto-save-visited-mode)
;; (setq auto-save-visited-interval 1)

;; (column-number-mode)
;; (global-display-line-numbers-mode t)

(set-face-attribute 'default nil :family "JetBrainsMono Nerd Font" :height 130)
(set-face-attribute 'fixed-pitch nil :family "JetBrainsMono Nerd Font" :height 130)
(set-face-attribute 'variable-pitch nil :family "Ubuntu Nerd Font" :height 140)

(dashboard-setup-startup-hook)
(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
(setq dashboard-startup-banner 'logo)
(setq dashboard-center-content t)
(setq dashboard-set-heading-icons t) 
(setq dashboard-set-file-icons t) 
(setq dashboard-set-navigator t)
(setq dashboard-items '((recents . 5) (projects . 5) (agenda . 5)))

(setq evil-want-integration t)
(setq evil-want-C-u-scroll t)
(setq evil-want-keybinding nil)
(setq evil-undo-system 'undo-redo)
(evil-mode 1)
(evil-collection-init)
(global-evil-surround-mode 1)

(doom-modeline-mode)

(modus-themes-load-themes)
(modus-themes-load-vivendi)

(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles partial-completion))))

(general-def
  :keymaps 'vertico-map
  "C-j" 'vertico-next
  "C-k" 'vertico-previous
  )

(general-def
  :keymaps 'minibuffer-local-map
  "M-h" 'backward-kill-word
  )

(vertico-mode)
(marginalia-mode)

(add-hook 'embark-collect-mode-hook 'consult-preview-at-point-mode)

(global-corfu-mode)
(setq corfu-auto t)

(setq which-key-idle-delay 0.0)
(which-key-mode)

(envrc-global-mode)

(require 'flycheck)

(defun my/lsp-mode-setup-completion ()
  (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults)) '(orderless))) ;; Configure orderless

(setq lsp-completion-provider :none) ;; we use Corfu!
(add-hook 'lsp-completion-mode-hook 'my/lsp-mode-setup-completion)

(setq elm-sort-imports-on-save t)
(setq elm-mode-hook '(elm-format-on-save-mode lsp-deferred))

(setq lsp-ltex-version "15.2.0")
(add-hook 'text-mode-hook 'lsp-deferred)

(pdf-tools-install)
(dirvish-override-dired-mode)

(require 'org)

(defun dotfiles/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode)
  (visual-line-mode)
  (org-bullets-mode)
  (setq visual-fill-column-width 120
        visual-fill-column-center-text t)
  (visual-fill-column-mode)
  )

(setq org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●"))

(add-hook 'org-mode-hook 'dotfiles/org-mode-setup)

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
	("j" "journal" plain (file "~/org/templates/journal.org")
         :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n")
	 :unnarrowed t)
	))

(org-roam-db-autosync-mode)

(projectile-mode 1)
(yas-global-mode 1)
(pulsar-global-mode 1)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

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
 "ws" '(:ignore t :which-key "split")
 "wss" 'evil-window-vsplit
 "wsh" 'evil-window-split
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
 "of" 'org-roam-node-find
 "od" '(:ignore t :which-key "dailies")
 "odf" 'org-roam-dailies-find-today
 "odc" 'org-roam-dailies-capture-today
 "j" '(:ignore t :which-key "jump")
 "jj" '((lambda () (interactive) (evil-avy-goto-char)) :which-key "evil-avy-goto-char")
 "jg" 'consult-ripgrep
 "g" '(:ignore t :which-key "git")
 "gg" 'magit
 "c" 'evilnc-comment-or-uncomment-lines
 "f" 'find-file
 "e" 'eval-buffer
 ";" 'execute-extended-command
 "\\" 'indent-region
 "v" 'check-parens
 "s" '(hydra-text-scale/body :which-key "scale-text")
 )

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

