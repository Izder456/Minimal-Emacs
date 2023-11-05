;; Treesitter
(setq treesit-font-lock-level 4)
(setq major-mode-remap-alist 
      '((yaml-mode . yaml-ts-mode)
        (bash-mode . bash-ts-mode)
        (js-mode . js-ts-mode)
        (rust-mode . rust-ts-mode)
        (json-mode . json-ts-mode)
        (css-mode . css-ts-mode)
        (python-mode . python-ts-mode)
        (clojure-mode . clojure-ts-mode)
        (lisp-mode . lisp-ts-mode)))

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (rust "https://github.com/camdencheektree-sitter-rust")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")
        (clojure "https://github.com/sogaiu/tree-sitter-clojure")
        (lisp "https://github.com/theHamsta/tree-sitter-commonlisp")))

;; Eglot
(add-to-list 'eglot-server-programs '((clojure-mode . ("clojure-lsp"))
                                      (c-mode . ("clangd"))
                                      (cpp-mode . ("clangd"))
                                      (ruby-mode . ("solargraph"))
                                      (rust-mode . ("rust-analyzer"))))

       
;; Lisp
(add-hook 'lisp-mode-hook 'enable-paredit-mode)

;; Clojure
(setq package-selected-packages '(clojure-ts-mode cider))
(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

;; Chicken
(setq package-selected-packages '(geiser geiser-chicken))

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

(add-hook 'scheme-mode-hook 'setup-chicken-scheme)

;; Rust
(setq package-selected-packages '(rust-mode))

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
