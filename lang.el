;; ELisp

(autoload 'enable-paredit-mode "paredit" "turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)

;; Lisp

(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)

;; Clojure
(setq package-selected-packages '(clojure-mode clojure-mode cider lsp-treemacs))

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

(add-hook 'clojure-mode-hook 'lsp)
(add-hook 'clojure-mode-hook 'enable-paredit-mode)
(add-hook 'clojurescript-mode-hook 'lsp)
(add-hook 'clojurescript-mode-hook 'enable-paredit-mode)
(add-hook 'clojurec-mode-hook 'lsp)
(add-hook 'clojurec-mode-hook 'enable-paredit-mode)

;; Chicken
(setq package-selected-packages '(geiser))

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

(add-hook 'scheme-mode-hook 'setup-chicken-scheme)
(add-hook 'scheme-mode-hook 'enable-paredit-mode)

;; Rust
(setq package-selected-packages '(rust-mode rustic))

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))


;; Perl
(setq package-selected-packages '(cperl-mode))

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

;; Shell
(setq package-selected-packages '(shx shell-pop))

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

(require 'shx)
(require 'shell-pop)
(setq shell-pop-autocd-to-working-dir t)
(add-hook 'shell-mode-hook 'shx-mode)
(global-set-key (kbd "M-SPC") 'shell-pop)

;; C/++
(add-hook 'c-mode-hook #'lsp)
(add-hook 'c++-mode-hook #'lsp)