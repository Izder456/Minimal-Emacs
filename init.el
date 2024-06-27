(require 'package)
;; Add the Melpa repository to the list of package sources
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")) ;; installed by default
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/")) ;; installed by default from Emacs 28 onwards
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/"))

(setq package-archive-priorities '(("jcs-elpa" . 0)
				   ("melpa" . 1)
				   ("nongnu" . 2)
				   ("gnu" . 3)))
(use-package use-package
  :custom
  (use-package-always-ensure t)
  (package-native-compile t)
  (warning-minimum-level :error))

(use-package evil
 :ensure t
 :init
 ;; Configure evil to not bind its own keybindings
 (setq evil-want-keybinding nil)
 ;; Set window splitting behavior
 (setq evil-vsplit-window-right t)
 (setq evil-split-window-below t)
 ;; Enable evil mode
 (evil-mode 1)
 :config
 ;; Define a custom operator to delete without affecting the register
 (evil-define-operator evil-delete-without-register (beg end type yank-handler)
    (interactive "<R><y>")
    (evil-delete beg end type ?_ yank-handler))
 ;; Bind the custom delete operator to 'd' in normal and visual states
 (define-key evil-normal-state-map (kbd "d") 'evil-delete-without-register)
 (define-key evil-visual-state-map (kbd "d") 'evil-delete-without-register)
 ;; Bind 'D' to the default evil delete in normal and visual states
 (define-key evil-normal-state-map (kbd "D") 'evil-delete)
 (define-key evil-visual-state-map (kbd "D") 'evil-delete)
 ;; Disable certain keys in motion state to prevent accidental key presses
 (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd "SPC") nil)
    (define-key evil-motion-state-map (kbd "RET") nil)
    (define-key evil-motion-state-map (kbd "TAB") nil))
 ;; Set the undo system to undo-tree for a more powerful undo experience
 (evil-set-undo-system 'undo-tree)
 :bind
 (:map evil-motion-state-map
       ("s" . 'evil-avy-goto-char-2))
 (:map evil-normal-state-map
       ("s" . nil)))

(use-package evil-collection
 :ensure t
 :after evil
 :config
 ;; Initialize evil-collection with a specific list of modes
 (setq evil-collection-mode-list '(dashboard dired ibuffer))
 (evil-collection-init))

;; Set the location of the custom file and load it if it exists
(setq-default custom-file
              (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
 (load custom-file))

;; Ensure customizations are loaded immediately
(setq custom-initialize-delay nil)

(setenv "PATH"
        (concat
	 (concat (getenv "HOME") ".local/bin") path-separator
	 (concat (getenv "HOME") ".gems/bin") path-separator
	 (concat (getenv "HOME") ".go/bin") path-separator
	 (concat (getenv "HOME") "go/bin") path-separator
	 (concat (getenv "HOME") ".cargo/bin") path-separator
	 "/usr/local/jdk-1.8.0/bin" path-separator
         (getenv "PATH")))

(use-package undo-tree
 :ensure t
 :config
 ;; Enable undo-tree globally
 (global-undo-tree-mode)
 ;; Set the directory for undo-tree history files
 ;; This helps keep the Emacs configuration directory clean
 (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-tree")))
 ;; Ensure the undo-tree directory exists
 (unless (file-exists-p "~/.emacs.d/undo-tree")
    (make-directory "~/.emacs.d/undo-tree" t)))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package general
 :config
 ;; Integrate general with evil-mode
 (general-evil-setup)

 ;; Set up 'C-x' as the global leader key
 (general-create-definer iz/leader-keys
   :states '(normal insert visual emacs command)
   :keymaps 'override
   :prefix "C-x" ;; set leader
   :global-prefix "C-x") ;; access leader in insert mode

 ;; Buffer commands
 (iz/leader-keys
   "k" '(kill-this-buffer :wk "Kill this buffer")
   "n" '(evil-next-buffer :wk "Cycle Windows")
   "S" '(evil-window-vnew :wk "Vertical buffer split")
   "s" '(evil-window-new :wk "Horizontal buffer split")
   "<up>" '(evil-window-up :wk "Switch to upper buffer")
   "<down>" '(evil-window-down :wk "Switch to lower buffer")
   "<left>" '(evil-window-left :wk "Switch to left buffer")
   "<right>" '(evil-window-right :wk "Switch to right buffer"))

 ;; Neotree commands
 (iz/leader-keys
   "t" '(neotree-toggle :wk "Open neotree"))

 ;; Tab-switching
 (global-set-key (kbd "C-<tab>") 'evil-window-mru)
 
 ;; Zoom in and out
 (global-set-key (kbd "C-+") 'text-scale-increase)
 (global-set-key (kbd "C--") 'text-scale-decrease)
 (global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
 (global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

 ;; Minibuffer quits
 (global-set-key [escape] 'keyboard-escape-quit))

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

(setq erc-prompt (lambda () (concat "<" (buffer-name) ">"))
      erc-server "irc.libera.chat"
      erc-nick "izder456"
      erc-user-full-name "izder456"
      erc-autojoin-channels-alist '(("irc.libera.chat" "#openbsd-gaming" "#openbsd" "#gaygeeks")
				    ("147.185.221.20" "#general"))
      erc-auto-query 'bury
      erc-fill-column 128
      erc-fill-function 'erc-fill-static
      erc-fill-static-center 20)

(use-package denote
  :pin gnu
  :ensure t
  :config
  (setq denote-directory (expand-file-name "~/Documents/notes/denote/"))
  (setq denote-known-keywords '())
  (setq denote-file-type nil)
  (add-hook 'dired-mode-hook #'denote-dired-mode))

(use-package org-superstar
  :hook
  (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-special-todo-items t)
  (setq org-superstar-headline-bullets-list '("ƛ"
					      "ꙮ"
					      "ꙭ"
					      "ꙫ"
					      "ꙩ"
					      "о"
					      "ы"
					      "ъ"
					      "ь"))
  ;; disables leading bullets
  (setq org-superstar-leading-bullet ?\s)
  (setq org-indent-mode-turns-on-hiding-stars nil))

(use-package toc-org
  :hook
  (org-mode . toc-org-mode)
  :commands toc-org-enable)

(use-package org-appear
  :hook
  (org-mode . org-appear-mode))

(use-package dashboard
  :ensure t
  :init
  (setq initial-buffer-choice 'dashboard-open)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "Cos I'm a lonely soul, and I got no friend on this road. And I'm a Mystery Jack, the lines on my hands tell me I'm on track.")
  (setq dashboard-startup-banner "~/.emacs.d/hantyumi.png")  ;; use custom image as banner
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

(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)
         (sly-mode . rainbow-delimiters-mode)
         (cider-mode . rainbow-delimiters-mode)
         (geiser-mode . rainbow-delimiters-mode)
         (geiser-repl-mode . rainbow-delimiters-mode)
         (inf-elixir-mode . rainbow-delimiters-mode)
         (hy-mode . rainbow-delimiters-mode)))

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

(use-package elfeed
  :ensure t
  :custom
  (elfeed-db-directory
   (expand-file-name "elfeed" user-emacs-directory)
   (elfeed-show-entry-switch 'display-buffer))
  :bind
  ("C-c w e" . elfeed)
  :config
  (setq elfeed-feeds
	'("https://xkcd.com/rss.xml"
	  "https://dataswamp.org/~solene/rss.xml"
	  "https://undeadly.org/cgi?action=rss")))

(use-package elfeed-dashboard
  :ensure t
  :config
  (setq elfeed-dashboard-file "~/.emacs.d/elfeed-dashboard.org")
  (advice-add 'elfeed-search-quit-window :after #'elfeed-dashboard-update-links))

(setq scheme-program-name "chicken-csi -:c")
(setq geiser-chicken-binary "chicken-csi")
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(use-package flycheck
  :ensure t
  :defer t
  :diminish
  :config (global-flycheck-mode))
(use-package flycheck-projectile
  :ensure t)
(use-package flycheck-rust
  :ensure t
  :config
  (with-eval-after-load 'rust-mode
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))
(use-package flycheck-elixir
  :ensure t
  :config
  (with-eval-after-load 'elixir-mode
    (add-hook 'elixir-mode-hook #'flycheck-elixir-setup)))
(use-package flycheck-clojure
  :ensure t)
(use-package flycheck-raku
  :ensure t)

;; Config modes
(use-package yaml-mode ;; Insecure Pythonic config format
  :ensure t
  :defer t)
(use-package json-mode ;; Annoying JS config format
  :ensure t
  :defer t)
(use-package toml-mode ;; Good format
  :ensure t
  :defer t)

;; Markdown
(use-package markdown-mode ;; For those not blessed by ORG-Mode
  :ensure t
  :defer t)

;; Programming Languages
(use-package rust-mode ;; C++ Replacer
  :ensure t
  :defer t)
(use-package d-mode ;; Real C++ Replacer
  :ensure t
  :defer t)
(use-package forth-mode ;; Hey, I'm weird too
  :ensure t
  :defer t)
(use-package raku-mode ;; Perl, but somehow worse
  :ensure t
  :defer t)
(use-package elixir-mode ;; Ruby, if it was functional
  :ensure t
  :defer t)
(use-package crystal-mode
  :ensure t
  :defer t)


;; Lisps
(use-package clojure-mode ;; Lisp on the JVM
  :ensure t
  :defer t)
(use-package hy-mode ;; Lisp on Python
  :ensure t
  :defer t)
(use-package fennel-mode ;; Lisp on Lua
  :ensure t
  :defer t)

;; REPLs
(use-package inf-elixir ;; Inferior Mode for Elixir
  :ensure t
  :defer t)
(use-package cider ;; REPL for Clojure 
  :ensure t
  :defer t)
(use-package geiser ;; REPL for scheme
  :ensure t
  :defer t)
(use-package geiser-chicken ;; Chicken for Geiser
  :ensure t
  :defer t
  :config
  (add-hook 'geiser-mode-hook 'geiser-chicken))
(use-package sly ;; REPL for CL
  :ensure t
  :defer t
  :config
  (setq inferior-lisp-program "sbcl"))

;; Misc Programming Stuffs
(use-package clhs ;; Common Lisp Hyperspec
  :ensure t
  :config
  (autoload 'clhc-doc "clhs" "Get doc on ANSI CL" t)
  (define-key help-map "\C-l" 'clhs-doc)
  (custom-set-variables
   '(tags-apropos-additonal-actions '(("Common Lisp" clhs-doc clhs-symbols)))))

(use-package auto-virtualenv ;; For Python/Hy
  :ensure t
  :init
  (use-package pyvenv
    :ensure t)
  :config
  (add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv)
  (add-hook 'projectile-after-switch-project-hook 'auto-virtualenv-set-virtualenv))

(use-package eglot
  :ensure t
  :config
  (add-to-list 'eglot-server-programs '((clojure-mode . ("clojure-lsp"))))
  (add-to-list 'eglot-server-programs '((rust-mode . ("rust-analyzer"))))
  (add-to-list 'eglot-server-programs '((c++-mode . ("clangd"))))
  (add-to-list 'eglot-server-programs '((c-mode . ("clangd"))))
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
	(perl "https://github.com/tree-sitter-perl/tree-sitter-perl")
	(css "https://github.com/tree-sitter/tree-sitter-css")
	(go "https://github.com/tree-sitter/tree-sitter-go")
	(rust "https://github.com/tree-sitter/tree-sitter-rust")
	(html "https://github.com/tree-sitter/tree-sitter-html")
	(ruby "https://github.com/tree-sitter/tree-sitter-ruby")
	(javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
	(json "https://github.com/tree-sitter/tree-sitter-json")
	(python "https://github.com/tree-sitter/tree-sitter-python")
	(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
	(java "https://github.com/tree-sitter/tree-sitter-java")
	(scala "https://github.com/tree-sitter/tree-sitter-scala")
	(c "https://github.com/tree-sitter/tree-sitter-c")
	(cpp "https://github.com/tree-sitter/tree-sitter-cpp")))

(set-frame-parameter (selected-frame) 'alpha '(100 . 95))
(add-to-list 'default-frame-alist '(alpha . (100 . 95)))
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

;; org

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

(setq org-display-custom-times t)

(setq org-pretty-entities t)
(setq org-use-sub-superscripts "{}")
(setq org-hide-emphasis-markers t)
(setq org-startup-with-inline-images t)

(add-hook 'org-mode-hook 'org-indent-mode)
(setq org-return-follows-link t)
;; Stop src blocks from auto indenting
(setq org-edit-src-content-indentation 0)

(defun load-my-fonts (frame)
 (select-frame frame)
 (set-face-attribute 'default nil
                      :font "Spleen"
                      :weight 'regular
                      :height 120)
 (set-face-attribute 'fixed-pitch nil
                      :font "Spleen"
                      :weight 'regular
                      :height 120)
 (set-face-attribute 'variable-pitch nil
                      :font "Freeserif"
                      :weight 'regular
                      :height 1.2))

(if (daemonp)
    (add-hook 'after-make-frame-functions #'load-my-fonts)
 (load-my-fonts (selected-frame)))

;; Make sure certain org faces use the fixed-pitch face when variable-pitch-mode is on
(with-eval-after-load 'org-faces
 (set-face-attribute 'org-block nil
                      :foreground nil
                      :inherit 'fixed-pitch)
 (set-face-attribute 'org-table nil
                      :inherit 'fixed-pitch)
 (set-face-attribute 'org-formula nil
                      :inherit 'fixed-pitch)
 (set-face-attribute 'org-code nil
                      :inherit '(shadow fixed-pitch))
 (set-face-attribute 'org-verbatim nil
                      :inherit '(shadow fixed-pitch))
 (set-face-attribute 'org-special-keyword nil
                      :inherit '(font-lock-comment-face fixed-pitch))
 (set-face-attribute 'org-meta-line nil
                      :inherit '(font-lock-comment-face fixed-pitch))
 (set-face-attribute 'org-checkbox nil
                      :inherit 'fixed-pitch))

;; Set org-mode to use Variable pitch
(add-hook 'org-mode-hook 'variable-pitch-mode)
(add-hook 'org-mode-hook 'visual-line-mode)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(use-package doom-themes
  :pin melpa
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold nil    ; if nil, bold is universally disabled
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

;; disable size hinting
(setq frame-resize-pixelwise t)

;; save minibuffer history
(savehist-mode 1)

(setq-default left-margin-width 5 right-margin-width 5) ; Define new widths.
(set-window-buffer nil (current-buffer)) ; Use them now.

;; smooth scrolling
(setq scroll-step           1
      scroll-conservatively 10000)

;; LaTeX
(setq org-latex-compiler "xelatex")
(setq org-latex-pdf-process '("xelatex %f"))

;; the gtk stuff
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(delete-selection-mode 1)    ;; You can select text and delete it by typing.
(electric-indent-mode 1)
(electric-pair-mode 1)       ;; Turns on automatic parens pairing

(global-auto-revert-mode t)  ;; Automatically show changes if the file has changed

;; i want line numbers when i program !!
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'visual-line-mode)
