;; Clojure
(setq package-selected-packages '(clojure-mode cider lsp-treemacs))

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

(add-hook 'clojure-mode-hook 'lsp)
(add-hook 'clojurescript-mode-hook 'lsp)
(add-hook 'clojurec-mode-hook 'lsp)

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