(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))
;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :ensure use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :ensure t unless otherwise specified.
  (setq elpaca-use-package-by-default t))

;; Block until current queue processed.
(elpaca-wait)

;; Hardcore garbage collects
(use-package gcmh
  :ensure t
  :demand t
  :init
  (gcmh-mode 1)
  :config
  (setq gcmh-idle-delay 5
	gcmh-high-cons-threshold (* 64 1024 1024))) ; 64mb

;; Replace with faster alternatives
(use-package dash
  :ensure t
  :demand t
  :config
  (global-dash-fontify-mode))

(use-package evil
  :ensure t
  :defer t
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
  (evil-set-undo-system 'undo-fu))

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
	 "/usr/local/jdk-17/bin" path-separator
         (getenv "PATH")))

(use-package undo-fu
  :ensure t
  :defer t)

(use-package editorconfig
  :ensure t
  :defer t
  :config
  (editorconfig-mode 1))

(use-package general
  :ensure t
  :demand t
  :config
  ;; Integrate general with evil-mode
  (general-evil-setup)

  ;; Set up 'C-x' as the global leader key
  (general-create-definer iz/leader-keys
    :states '(normal insert visual emacs command)
    :keymaps 'override
    :prefix "C-x" ;; set leader
    :global-prefix "C-x") ;; access leader in insert mode

  ;; Define functions for resizing windows
  (defun partial-size-window ()
    "Set the two split windows to 70% and 30% vertically."
    (interactive)
    (let ((size (- (truncate (* .70 (frame-height))) (window-height))))
      (if (> size 0)
          (enlarge-window size))))

  (defun partial-size-window-h ()
    "Set the two split windows to 70% and 30% horizontally."
    (interactive)
    (let ((size (- (truncate (* .70 (frame-width))) (window-width))))
      (if (> size 0)
          (enlarge-window-horizontally size))))
  
  ;; Buffer commands
  (iz/leader-keys
    "k" '(kill-this-buffer :wk "Kill this buffer")
    "n" '(evil-next-buffer :wk "Cycle Windows")
    "S" '(evil-window-vnew :wk "Vertical buffer split")
    "s" '(evil-window-new :wk "Horizontal buffer split")
    "<up>" '(evil-window-up :wk "Switch to upper buffer")
    "<down>" '(evil-window-down :wk "Switch to lower buffer")
    "<left>" '(evil-window-left :wk "Switch to left buffer")
    "<right>" '(evil-window-right :wk "Switch to right buffer")
    "r" '(partial-size-window :wk "Partial size window vertical")
    "r" '(partial-size-window-h :wk "Partial size window horizontal"))

  ;; Neotree commands
  (iz/leader-keys
    "t" '(neotree-toggle :wk "Open neotree"))

  ;; Consult
  (iz/leader-keys
    "RET" '(consult-buffer :wk "Consult Buffer Swap")
    "b"   '(consult-buffer :wk "Consult Buffer Swap")
    "g"   '(consult-goto-line :wk "Consult Goto"))

  ;; Corfu
  (general-add-hook
   'corfu-mode-hook
   (lambda ()
     (general-define-key
      :keymaps 'corfu-map
      "tab" '(corfu-next :wk "corfu next")
      "backtab" '(corfu-previous :wk "corfu previous"))))

  ;; Vertico
  (general-add-hook
   'vertico-mode-hook
   (lambda ()
     (general-define-key
      :keymaps 'vertico-map
      "tab" '(vertico-next :wk "vertico next")
      "backtab" '(vertico-previous :wk "vertico previous"))))
  
  ;; Popper
  (iz/leader-keys
    "p" '(popper-toggle-type :wk "Popper Toggle")
    "C-p" '(popper-cycle :wk "Popper Cycle"))

  ;; Magit
  (iz/leader-keys
    "g" '(magit-status :wk "Magit Status")
    "M-g" '(magit-dispatch :wk "Magit Pull"))
  
  ;; Tab-switching
  (global-set-key (kbd "C-<tab>") 'evil-window-mru)

  ;; Zoom in and out
  (global-set-key (kbd "C-+") 'text-scale-increase)
  (global-set-key (kbd "C--") 'text-scale-decrease)
  (global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
  (global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

  ;; Minibuffer quits
  (global-set-key [escape] 'keyboard-escape-quit)

  ;; Enable winner-mode for undo/redo window configurations
  (winner-mode 1))

(defvar notify-program "notify-send")

(defun notify-send (title message)
  (start-process "notify" " notify"
                 notify-program "--expire-time=4000" title message))

(use-package erc
  :ensure t
  :defer t
  :init
  (defun erc-mention (match-type nickuserhost msg)
    (when (eq match-type 'current-nick)
      (notify-send "(IRC)"
  		   (format "PING! %s" msg))))
  (defun erc-clean-image-urls-in-buffer ()
    "Clean URLs by removing trackers after image extensions in the current buffer."
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\(<\\)?\\(https?://[^ >]+\\)\\.\\(jpg\\|jpeg\\|png\\|gif\\|webp\\|mov\\|mp4\\|mkv\\)\\([^ >]*\\)\\(>\\)?" nil t)
        (let ((prefix (match-string 1))
              (url (match-string 2))
              (extension (match-string 3))
              (suffix (match-string 5)))
          (replace-match (concat (or prefix "")
                                 url
                                 "."
                                 extension
                                 (or suffix "")
                                 "\n\n"))))))
  (add-hook 'erc-insert-post-hook 'erc-clean-image-urls-in-buffer)
  (add-hook 'erc-text-matched-hook 'erc-mention)
  ;; Ensure erc buffers replace the current window
  (add-to-list 'display-buffer-alist
               '("^\\*erc\\*" . (display-buffer-same-window)))
  :custom
  (erc-hide-list '("JOIN" "PART" "QUIT"))
  (erc-lurker-hide-list '("JOIN" "PART" "QUIT"))
  (erc-lurker-threshold-time 43200)
  (erc-track-exclude-types '("JOIN" "MODE" "NICK" "PART" "QUIT"
			     "324" "329" "332" "333" "353" "477"))
  (erc-server "irc.libera.chat")
  (erc-nick "izder456")
  (erc-user-full-name "izder456")
  (erc-autojoin-channels-alist '(("Libera.chat" "#openbsd-gaming" "#openbsd" "#gaygeeks" "#linux+bsd")
                                 ("irc.atl.chat" "#general" "#support")))
  (erc-auto-query 'bury)
  (erc-kill-buffer-on-part t)
  (erc-kill-queries-on-quit t)
  (erc-kill-server-buffer-on-quit t)
  (erc-interpret-mirc-color t)
  (erc-fill-function nil)
  (erc-fill-mode nil)
  :config
  (add-to-list 'erc-modules 'spelling)
  (add-to-list 'erc-modules 'match)
  (add-to-list 'erc-modules 'smiley)
  (add-to-list 'erc-modules 'image)
  (add-to-list 'erc-modules 'hl-nicks)
  (setq erc-modules (remove 'stamp erc-modules))
  (erc-services-mode 1)
  (erc-update-modules)
  (erc-fill-disable))

(use-package erc-hl-nicks
  :ensure t
  :defer t
  :after erc)
(use-package erc-image
  :ensure t
  :defer t
  :after erc)

(use-package jabber
  :ensure t
  :defer t
  :init
  (defun jabber-notify (from buf text proposed-alert)
    (when (or jabber-message-alert-same-buffer
	      (not (memq (selected-window) (get-buffer-window-list buf))))
      (if (jabber-muc-sender-p from)
	  (notify-send (format "(PM) %s"
			       (jabber-jid-displayname (jabber-jid-user from)))
		       (format "%s: %s" (jabber-jid-resource from) text))
	(notify-send (format "%s" (jabber-jid-displayname from))
		     test))))
  (add-hook 'jabber-alert-message-hooks 'jabber-notify)
  :custom
  (jabber-mode-line-mode 1))

(use-package denote
  :ensure t
  :defer t
  :config
  (setq denote-directory (expand-file-name "~/Documents/notes/denote/"))
  (setq denote-known-keywords '())
  (setq denote-file-type nil)
  (add-hook 'dired-mode-hook #'denote-dired-mode))

(use-package org-superstar
  :ensure t
  :defer t
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
  :ensure t
  :defer t
  :hook
  (org-mode . toc-org-mode)
  :commands toc-org-enable)

(use-package org-appear
  :ensure t
  :defer t
  :hook
  (org-mode . org-appear-mode))

(use-package dashboard
  :ensure t
  :defer t
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
  :defer t
  :if (display-graphic-p))

(use-package nerd-icons
  :ensure t
  :defer t)

(use-package emojify
  :ensure t
  :defer t
  :init
  (setq emojify-display-style "ascii")
  (global-emojify-mode 1))


(use-package all-the-icons-dired
  :ensure t
  :defer t
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  (setq all-the-icons-dired-monochrome nil))

(use-package frame-local
  :ensure t
  :demand t)

(use-package corfu 
  :ensure t
  :demand t
  :init (global-corfu-mode)
  :custom
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  (corfu-quit-at-boundary nil)
  (corfu-quit-no-match t)
  (corfu-auto t))

(use-package vertico
  :ensure t
  :demand t
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode 1))

(use-package marginalia
  :after vertico
  :ensure t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode 1))

(use-package orderless
  :ensure t
  :demand t
  :init
  (setq completion-styles '(orderless partial-completion basic)
        completion-category-defaults nil
        completion-category-overrides nil))

(use-package consult
  :ensure t
  :demand t
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init)

(use-package projectile
  :ensure t
  :defer t
  :config
  (projectile-mode +1))

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :hook
  '((prog-mode . rainbow-delimiters-mode)
    (sly-mode . rainbow-delimiters-mode)
    (ielm-mode . rainbow-delimiters-mode)
    (cider-mode . rainbow-delimiters-mode)
    (geiser-mode . rainbow-delimiters-mode)
    (geiser-repl-mode . rainbow-delimiters-mode)
    (inf-elixir-mode . rainbow-delimiters-mode)
    (hy-mode . rainbow-delimiters-mode)))

(use-package rainbow-mode
  :ensure t
  :defer t
  :diminish
  :hook
  '((prog-mode . rainbow-mode)
    (sly-mode . rainbow-mode)
    (ielm-mode . rainbow-mode)
    (cider-mode . rainbow-mode)
    (geiser-mode . rainbow-mode)
    (geiser-repl-mode . rainbow-mode)
    (inf-elixir-mode . rainbow-mode)
    (hy-mode . rainbow-mode)))

(use-package beacon
  :ensure t
  :defer t
  :config
  (beacon-mode))

(use-package doom-modeline
  :ensure t
  :defer t
  :init (doom-modeline-mode 1))

(use-package hl-todo
  :ensure t
  :defer t
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

(use-package flycheck
  :ensure t
  :defer t
  :diminish
  :config (global-flycheck-mode))
(use-package flycheck-projectile
  :ensure t
  :defer t)
(use-package flycheck-rust
  :ensure t
  :defer t
  :config
  (with-eval-after-load 'rust-mode
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))
(use-package flycheck-elixir
  :ensure t
  :defer t
  :config
  (with-eval-after-load 'elixir-mode
    (add-hook 'elixir-mode-hook #'flycheck-elixir-setup)))
(use-package flycheck-clojure
  :ensure t
  :defer t)
(use-package flycheck-raku
  :ensure t
  :defer t)

(use-package magit
  :ensure t
  :defer t)

(use-package magit-todos
  :ensure t
  :defer t
  :after magit
  :config
  (magit-todos-mode))

(use-package forge
  :ensure t
  :defer t
  :after magit)

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
  :defer t
  :config
  (autoload 'clhc-doc "clhs" "Get doc on ANSI CL" t)
  (define-key help-map "\C-l" 'clhs-doc)
  (custom-set-variables
   '(tags-apropos-additonal-actions '(("Common Lisp" clhs-doc clhs-symbols)))))

(use-package auto-virtualenv ;; For Python/Hy
  :ensure t
  :defer t
  :init
  (use-package pyvenv
    :ensure t)
  :config
  (add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv)
  (add-hook 'projectile-after-switch-project-hook 'auto-virtualenv-set-virtualenv))

;; i want line numbers when i program !!
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'visual-line-mode)

(use-package popper
  :ensure t
  :defer t
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode
          eshell-mode
          ielm-mode
          "\\*sly-.*\\*"
          cider-repl-mode
          geiser-repl-mode
          inf-elixir-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

(use-package eldoc
  :ensure nil
  :diminish eldoc-mode
  :config
  (global-eldoc-mode 1))

(use-package eglot
  :ensure nil
  :defer t
  :config
  (setq completion-category-overrides '((eglot (styles orderless))
                                        (eglot-capf (styles orderless))))
  (mapc (lambda (server-program) (add-to-list 'eglot-server-programs server-program))
        '((clojure-mode . ("clojure-lsp"))
          (rust-mode . ("rust-analyzer"))
          (c++-mode . ("clangd"))
          (c-mode . ("clangd"))))
  :hook
  '((prog-mode . eglot-ensure)
    (sly-mode . eglot-ensure)
    (ielm-mode . eglot-ensure)
    (cider-mode . eglot-ensure)
    (geiser-mode . eglot-ensure)
    (geiser-repl-mode . eglot-ensure)
    (inf-elixir-mode . eglot-ensure)
    (hy-mode . eglot-ensure)))

(use-package treesit-auto
  :ensure t
  :defer t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package indent-guide
  :ensure t
  :defer t
  :config
  (indent-guide-global-mode)
  (setq indent-guide-char ":")
  (setq indent-guide-delay 0.1)
  (set-face-background 'indent-guide-face "dimgray"))

(use-package highlight-indent-guides
  :ensure t
  :defer t
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-character ?\|)
  (setq highlight-indent-guides-responsive 'stack)
  (setq highlight-indent-guides-delay 0.1))

(use-package aggressive-indent
  :ensure t
  :defer t
  :config
  (global-aggressive-indent-mode 1)
  ;; Customize when not to indent
  (add-to-list
   'aggressive-indent-dont-indent-if
   '(and (derived-mode-p 'c++-mode)
         (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                             (thing-at-point 'line))))))

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

(use-package jinx
  :ensure t
  :demand t
  :hook (emacs-startup . global-jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))

(use-package org
  :ensure t
  :defer t
  :init
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

  (setq org-return-follows-link t)
  ;; Stop src blocks from auto indenting
  (setq org-edit-src-content-indentation 0)

  (setq org-display-custom-times t)

  (setq org-pretty-entities t)
  (setq org-use-sub-superscripts "{}")
  (setq org-hide-emphasis-markers t)
  (setq org-startup-with-inline-images t)

  (setq org-return-follows-link t)
  ;; Stop src blocks from auto indenting
  (setq org-edit-src-content-indentation 0)
  :hook
  '((org-mode-hook . org-indent-mode)))

(use-package mixed-pitch
  :ensure t
  :defer t
  :hook
  (text-mode . mixed-pitch-mode))

(use-package unicode-fonts
  :ensure t
  :demand t
  :init
  (unicode-fonts-setup)
  (set-charset-priority 'unicode)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (set-language-environment "UTF-8")
  (prefer-coding-system 'utf-8)
  (defun load-my-fonts (frame)
    (select-frame frame)
    (set-face-attribute 'default nil
		        :font "Spleen"
		        :weight 'regular
		        :height 120)
    (set-face-attribute 'bold nil
		        :font "Spleen"
		        :weight 'regular
		        :height 120)
    (set-face-attribute 'fixed-pitch nil
		        :font "Spleen"
		        :weight 'regular
		        :height 120)
    (set-face-attribute 'variable-pitch nil
		        :font "Spleen"
		        :weight 'regular
		        :height 120))
  (if (daemonp)
      (add-hook 'after-make-frame-functions #'load-my-fonts)
    (load-my-fonts (selected-frame)))
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
  :hook
  '((org-mode-hook . variable-pitch-mode)
    (org-mode-hook . visual-line-mode)))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(use-package doom-themes
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

(setq history-length 25)     ;; History Length
(savehist-mode 1)            ;; Save history
(save-place-mode 1)          ;; Save place in files
(delete-selection-mode 1)    ;; You can select text and delete it by typing.
(electric-indent-mode 1)     ;; Indents
(electric-pair-mode 1)       ;; Turns on automatic parens pairing
(global-auto-revert-mode 1)  ;; Automatically show changes if the file has changed
(prettify-symbols-mode 1)    ;; Combine symbols

(setq native-comp-async-report-warnings-errors 'silent
      byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local)
      idle-update-delay 1.0
      warning-minimum-level :error ;; suppress all warnings
      bidi-display-reordering 'left-to-right
      bidi-paragraph-direction 'left-to-right
      bidi-inhibit-bpa 1
      cursor-in-non-selected-windows nil
      highlight-nonselected-windows nil
      fast-but-imprecise-scrolling t
      inhibit-compacting-font-caches t
      custom-safe-themes t)

(setq-default
 indent-tabs-mode nil
 jit-lock-defer-time 0
 window-combination-resize t
 history-delete-duplicates t)

(setq display-time-24hr-format nil
      truncate-lines t
      tab-width 2
      fill-column 80
      line-move-visual t
      frame-resize-pixelwise t
      window-resize-pixelwise nil
      split-width-threshold 80
      create-lockfiles nil
      make-backup-files nil
      ;; But in case the user does enable it, some sensible defaults:
      version-control t     ; number each backup file
      backup-by-copying t   ; instead of renaming current file (clobbers links)
      delete-old-versions t ; clean up after itself
      kept-old-versions 5
      kept-new-versions 5
      backup-directory-alist (list (cons "." (concat user-emacs-directory "backup/")))
      display-time-default-load-average nil
      inhibit-startup-message t
      confirm-kill-processes nil
      load-prefer-newer t
      x-select-enable-clipboard t
      split-width-threshold 1)

(setq-default inhibit-startup-echo-area-message (user-login-name)
              display-line-numbers-width 3
              inhibit-major-mode 'fundamental-mode)
