(require 'package)
;; Add the Melpa repository to the list of package sources
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")) ;; installed by default
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/")) ;; installed by default from Emacs 28 onwards
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(setq use-package-always-ensure t)

(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents
     "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))

(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)

(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (evil-mode)
  :config
  ;; Delete without register
  (evil-define-operator evil-delete-without-register (beg end type yank-handler)
    (interactive "<R><y>")
    (evil-delete beg end type ?_ yank-handler))
  (define-key evil-normal-state-map (kbd "d") 'evil-delete-without-register)
  (define-key evil-visual-state-map (kbd "d") 'evil-delete-without-register)
  (define-key evil-normal-state-map (kbd "D") 'evil-delete)
  (define-key evil-visual-state-map (kbd "D") 'evil-delete)

  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd "SPC") nil)
    (define-key evil-motion-state-map (kbd "RET") nil)
    (define-key evil-motion-state-map (kbd "TAB") nil))

  (evil-set-undo-system 'undo-tree))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (setq evil-collection-mode-list '(dashboard dired ibuffer))
  (evil-collection-init))

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode)
  ;; I don't want random files in my directories for the undo-tree,
  ;; so unify them under this directory
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-tree"))))

(use-package general
  :config
  (general-evil-setup)

  ;; set up 'SPC' as the global leader key
  (general-create-definer iz/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC" ;; set leader
    :global-prefix "M-SPC") ;; access leader in insert mode

  (iz/leader-keys
    "b" '(:ignore t :wk "buffer")
    "bb" '(switch-to-buffer :wk "Switch buffer")
    "bk" '(kill-this-buffer :wk "Kill this buffer")
    "bn" '(next-buffer :wk "Next buffer")
    "bp" '(previous-buffer :wk "Previous buffer")
    "br" '(revert-buffer :wk "Reload buffer"))
  (iz/leader-keys
    "t"  '(:ignore t :wk "neotree")
    "tn" '(neotree-toggle :wk "Open neotree")))
;; zoom in and out
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)
;; man fuck minibuffers and their 3 esc quits
(global-set-key [escape] 'keyboard-escape-quit)

(use-package which-key
  :init
  (which-key-mode 1)
  :ensure t
  :config
  (setq which-key-side-window-location 'bottom
          which-key-sort-order #'which-key-key-order-alpha
          which-key-sort-uppercase-first nil
          which-key-add-column-padding 1
          which-key-max-display-columns nil
          which-key-min-display-lines 6
          which-key-side-window-slot -10
          which-key-side-window-max-height 0.25
          which-key-idle-delay 0.8
          which-key-max-description-length 25
          which-key-allow-imprecise-window-fit t
          which-key-separator " → " ))

(use-package org-roam
  :ensure t
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert))
  :custom
  (org-roam-directory (file-truename "~/Documents/org-roam"))
  :config
  (org-roam-setup)
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(use-package org-roam-ui
  :ensure t
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(setq org-agenda-files '("~/Documents/org-roam/agenda.org"))

(use-package org-superstar
  :hook
  (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-special-todo-items t)
  ;; disables leading bullets
  (setq org-superstar-leading-bullet ?\s)
  (setq org-indent-mode-turns-on-hiding-stars nil))

(use-package org-modern
  :ensure t
  :hook
  (org-mode . org-modern-mode)
  :config
  (setq org-auto-align-tags nil)
	(setq org-tags-column 0)
	(setq org-catch-invisible-edits 'show-and-error)
	(setq org-special-ctrl-a/e t)
	(setq org-insert-heading-respect-content t)

	;; org styling, hide markup etc
	(setq org-hide-emphasis-markers t)
	(setq org-pretty-entities t))

(use-package toc-org
  :hook
  (org-mode . toc-org-mode)
  :commands toc-org-enable)

(use-package org-auto-tangle
  :defer t
  :hook
  ((org-mode . org-auto-tangle-mode))
  :config
  (setq org-auto-tangle-default t))

(use-package auctex
  :ensure t
  :defer t)

(use-package cdlatex
  :ensure t)

(use-package org-fragtog
  :custom
  (org-startup-with-latex-preview t)
  :ensure t
  :hook
  ((org-mode . org-fragtog-mode)))

(use-package org-appear
  :hook
  (org-mode . org-appear-mode))

(defun diary-is-norm-scl-day ()
  ""
  (and (let ((dayname (calendar-day-of-week date)))
	           (memq dayname '(1 2 4 5)))
       (diary-block 9 06 2023 6 06 2024)))

(defun diary-is-wed-scl-day ()
  ""
  (and (let ((dayname (calendar-day-of-week date)))
	           (memq dayname '(3)))
       (diary-block 9 06 2023 6 06 2024)))

(defun diary-is-scl-day ()
  ""
  (and (unless (diary-block 9 29 2023 9 29 2023) t)
       (unless (diary-block 11 10 2023 11 10 2023) t)
       (unless (diary-block 11 20 2023 11 24 2023) t)
       (unless (diary-block 12 25 2023 12 29 2023) t)
       (unless (diary-block 1 1 2024 1 1 2024) t)
       (unless (diary-block 1 15 2024 1 15 2024) t)
       (unless (diary-block 1 26 2024 1 26 2024) t)
       (unless (diary-block 2 19 2024 2 19 2024) t)
       (unless (diary-block 3 25 2024 3 29 2024) t)
       (unless (diary-block 4 1 2024 4 1 2024) t)
       (unless (diary-block 5 24 2024 5 27 2024) t)))

(use-package org-alert
  :ensure t
  :config
  (setq org-alert-enable t)
  (setq alert-default-style 'libnotify)
  (setq org-alert-interval 150))

(require 'org-tempo)

(plist-put org-format-latex-options :scale 1.5)

(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.1))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.1))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.1))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.1))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.1))))
 '(org-level-6 ((t (:inherit outline-5 :height 1.1))))
 '(org-level-7 ((t (:inherit outline-5 :height 1.1)))))

(setq org-display-custom-times t)

(setq org-pretty-entities t)
(setq org-use-sub-superscripts "{}")
(setq org-hide-emphasis-markers t)
(setq org-startup-with-inline-images t)

(add-hook 'org-mode-hook 'org-indent-mode)
(setq org-return-follows-link t)
;; Stop src blocks from auto indenting
(setq org-edit-src-content-indentation 0)

(use-package dashboard
  :ensure t
  :init
  (setq initial-buffer-choice 'dashboard-open)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "Giygas cannot think rationally anymore, and he isn't even aware of what he is doing now.")
  (setq dashboard-startup-banner "~/.emacs.d/giegue.png")  ;; use custom image as banner
  (setq dashboard-center-content t) ;; set to 't' for centered content
  (setq dashboard-items '((recents . 5)
                          (projects . 3)
                          (agenda . 5)))
  :custom
  (dashboard-modify-heading-icons '((recents . "file-text")
				      (bookmarks . "book")))
  :config
  (dashboard-setup-startup-hook))

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :ensure t
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  (setq all-the-icons-dired-monochrome nil))

(use-package all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))

(use-package company
  :ensure t
  :defer 2
  :diminish
  :custom
  (company-begin-commands '(self-insert-command))
  (company-idle-delay .05)
  (company-minimum-prefix-length 2)
  (company-show-numbers t)
  (company-tooltip-align-annotations 't)
  :config
  (global-company-mode)
  (setq lsp-completion-provider :capf))

(use-package frame-local
  :ensure t)

(use-package company-box
  :ensure t
  :after company frame-local
  :hook (company-mode . company-box-mode))

(use-package counsel
  :ensure t
  :after ivy
  :diminish
  :config
    (counsel-mode)
    (setq ivy-initial-inputs-alist nil)) ;; removes starting ^ regex in M-x

(use-package ivy
  :ensure t
  :bind
  ;; ivy-resume resumes the last Ivy-based completion.
  (("C-c C-r" . ivy-resume)
   ("C-x B" . ivy-switch-buffer-other-window))
  :diminish
  :custom
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)
  :config
  (ivy-mode))

(use-package ivy-rich
  :after ivy
  :ensure t
  :init (ivy-rich-mode 1) ;; this gets us descriptions in M-x.
  :custom
  (ivy-virtual-abbreviate 'full
   ivy-rich-switch-buffer-align-virtual-buffer t
   ivy-rich-path-style 'abbrev)
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer))
(use-package prescient
  :ensure t)

(use-package ivy-prescient
  :after counsel
  :ensure t
  :config
  (ivy-prescient-mode))

(use-package company-prescient
  :after company
  :ensure t
  :config
  (company-prescient-mode))

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1))

(use-package drag-stuff
  :ensure t
  :config
  (drag-stuff-mode))

(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)))

(use-package rainbow-mode
  :diminish
  :hook org-mode prog-mode)

(use-package beacon
  :ensure t
  :config
  (beacon-mode))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package nyan-mode
  :ensure t
  :config
  (nyan-mode))

(use-package neotree
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-smart-open t
        neo-show-hidden-files t
        neo-window-width 30
        neo-window-fixed-size nil
        inhibit-compacting-font-caches t
        projectile-switch-project-action 'neotree-projectile-action)
        ;; truncate long file names in neotree
        (add-hook 'neo-after-create-hook
           #'(lambda (_)
               (with-current-buffer (get-buffer neo-buffer-name)
                 (setq truncate-lines t)
                 (setq word-wrap nil)
                 (make-local-variable 'auto-hscroll-mode)
                 (setq auto-hscroll-mode nil)))))

(use-package vterm
    :ensure t)

(use-package vterm-toggle
  :ensure t
  :after vterm
  :config
  ;; When running programs in Vterm and in 'normal' mode, make sure that ESC
  ;; kills the program as it would in most standard terminal programs.
  (evil-define-key 'normal vterm-mode-map (kbd "<escape>") 'vterm--self-insert)
  (setq vterm-toggle-fullscreen-p nil)
  (setq vterm-toggle-scope 'project)
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                     (let ((buffer (get-buffer buffer-or-name)))
                       (with-current-buffer buffer
                         (or (equal major-mode 'vterm-mode)
                             (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                  (display-buffer-reuse-window display-buffer-at-bottom)
                  ;;(display-buffer-reuse-window display-buffer-in-direction)
                  ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                  ;;(direction . bottom)
                  ;;(dedicated . t) ;dedicated is supported in emacs27
                  (reusable-frames . visible)
                  (window-height . 0.4))))

(use-package hl-todo
  :ensure t
  :hook ((org-mode . hl-todo-mode)
         (prog-mode . hl-todo-mode))
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(("TODO"       warning bold)
          ("FIXME"      error bold)
          ("HACK"       font-lock-constant-face bold)
          ("REVIEW"     font-lock-keyword-face bold)
          ("NOTE"       success bold)
          ("DEPRECATED" font-lock-doc-face bold))))

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(use-package flycheck
  :ensure t
  :defer t
  :diminish
  :config (global-flycheck-mode))
(use-package flycheck-rust
  :ensure t
  :config
  (with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))
(use-package flycheck-ocaml
  :ensure t
  :config
  (with-eval-after-load 'ocaml-mode
    (add-hook 'flycheck-mode-hook #'flycheck-ocaml-setup)))

(use-package rust-mode
  :ensure t)
(use-package yaml-mode
  :ensure t)
(use-package json-mode
  :ensure t)
(use-package cider
  :ensure t
  :hook ((clojure-mode . cider-jack-in)))
(use-package clojure-mode
  :ensure t)
(use-package markdown-mode
  :ensure t
  :hook ((markdown-mode . visual-line-mode)))

(use-package eglot
  :ensure t
  :config
  (add-to-list 'eglot-server-programs '((clojure-mode . ("clojure-lsp"))))
  (add-to-list 'eglot-server-programs '((rust-mode . ("rust-analyzer"))))
  :hook
  ((rust-mode . eglot)
   (clojure-mode . eglot)))

(setq treesit-font-lock-level 4)

;; Tell Emacs to prefer the treesitter mode
;; You'll want to run the command `M-x treesit-install-language-grammar' before editing.
(setq major-mode-remap-alist
      '((yaml-mode . yaml-ts-mode)
        (bash-mode . bash-ts-mode)
        (js-mode . js-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (rust-mode . rust-ts-mode)
        (go-mode . go-ts-mode)
        (json-mode . json-ts-mode)
        (css-mode . css-ts-mode)
        (python-mode . python-ts-mode)))

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
	      (ocaml "https://github.com/tree-sitter/tree-sitter-ocaml")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
        (rust "https://github.com/tree-sitter/tree-sitter-rust")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(set-frame-parameter (selected-frame) 'alpha '(95 . 85))
(add-to-list 'default-frame-alist '(alpha . (95 . 85)))
(defun toggle-transparency ()
   (interactive)
   (let ((alpha (frame-parameter nil 'alpha)))
     (set-frame-parameter
      nil 'alpha
      (if (eql (cond ((numberp alpha) alpha)
                     ((numberp (cdr alpha)) (cdr alpha))
                     ;; Also handle undocumented (<active> <inactive>) form.
                     ((numberp (cadr alpha)) (cadr alpha)))
               100)
          '(85 . 50) '(100 . 100)))))
 (global-set-key (kbd "C-c t") 'toggle-transparency)

(set-face-attribute 'default nil
  :font "Spleen"
  :height 140
  :weight 'regular)
(set-face-attribute 'variable-pitch nil
  :font "Liberation Serif"
  :height 140
  :weight 'regular)
(set-face-attribute 'fixed-pitch nil
  :font "Spleen"
  :height 140
  :weight 'regular)
;; This sets the default font on all graphical frames created after restarting Emacs.
;; Does the same thing as 'set-face-attribute default' above, but emacsclient fonts
;; are not right unless I also add this method of setting the default font.
(add-to-list 'default-frame-alist '(font . "Spleen:style=Medium:antialias=true:hinting=true"))

(setq-default line-spacing 0.17)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(use-package doom-themes
  :pin melpa
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-gruvbox t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config))

;; Changing the backup file path
(defun iz/backup-file-name (fpath)
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let* ((backupRootDir "~/.emacs.d/emacs-backup/")
         (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path
         (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") )))
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath))
(setq make-backup-file-name-function 'iz/backup-file-name)

;; save minibuffer history
(savehist-mode 1)

(setq-default left-margin-width 5 right-margin-width 5) ; Define new widths.
(set-window-buffer nil (current-buffer)) ; Use them now.

;; smooth scrolling
(setq scroll-step           1
      scroll-conservatively 10000)

;; disable the gtk stuff
(menu-bar-mode -1)
(tool-bar-mode -1)

(delete-selection-mode 1)    ;; You can select text and delete it by typing.
(electric-indent-mode 1)
(electric-pair-mode 1)       ;; Turns on automatic parens pairing

(global-auto-revert-mode t)  ;; Automatically show changes if the file has changed

;; i want line numbers when i program !!
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'visual-line-mode)
