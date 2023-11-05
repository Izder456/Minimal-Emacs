;;
; User
;;

(setq user-mail-address "izder456@disroot.org"
      user-full-name "izder456")

;;
; No Warns
;;
(setq gs-cons-threshold 10000000)
(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)

;;
; Compile Peace
;;
(defun hide-compile-buffer-if-successful (buffer string)
  (setq compilation-total-time (time-subtract nil compilation-start-time))
  (setq time-str (concat " (Time: " (format-time-string "%s.%3N" compilation-total-time) "s)"))

  (if
    (with-current-buffer buffer
      (setq warnings (eval compilation-num-warnings-found))
      (setq warnings-str (concat " (Warnings: " (number-to-string warnings) ")"))
      (setq errors (eval compilation-num-errors-found))

      (if (eq errors 0) nil t))

    ;;If Errors then
    (message (concat "Compiled with Errors" warnings-str time-str))

    ;;If Compiled Successfully or with Warnings then
    (progn
      (bury-buffer buffer)
      (run-with-timer auto-hide-compile-buffer-delay nil 'delete-window (get-buffer-window buffer 'visible))
      (message (concat "Compiled Successfully" warnings-str time-str)))))

(make-variable-buffer-local 'compilation-start-time)

(defun compilation-started (proc)
  (setq compilation-start-time (current-time)))

(defcustom auto-hide-compile-buffer-delay 0
  "Time in seconds before auto hiding compile buffer."
  :group 'compilation
  :type 'number)

(add-hook 'compilation-start-hook 'compilation-started)
(add-hook 'compilation-finish-functions 'hide-compile-buffer-if-successful)

;;
; Fonts
;;

(defun load-my-fonts (frame)
  (select-frame frame)
  (set-face-attribute 'default nil :font "Spleen" :weight 'medium :height 120)
  (set-face-attribute 'fixed-pitch nil :font "Spleen" :weight 'medium :height 120)
  (set-face-attribute 'variable-pitch nil :font "Terminus" :weight 'medium :height 1.1)

  (set-face-attribute 'company-tooltip nil :font "Spleen" :weight 'medium :height 120)

  ;; Make the document title a bit bigger
  (set-face-attribute 'org-document-title nil :font "Liberation Serif" :weight 'bold :height 1.3)

  ;; Make sure certain org faces use the fixed-pitch face when variable-pitch-mode is on
  (set-face-attribute 'org-block nil :foreground nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil :inherit '(variable-pitch))
  (set-face-attribute 'org-formula nil :font "Liberation Serif" :weight 'regular :height 1.0)
  (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(if (daemonp)
    (add-hook 'after-make-frame-functions #'load-my-fonts)
  (load-my-fonts (selected-frame)))

;;
; Ligatures
;;

(dolist (char/ligature-re
         `((?-  ,(rx (or (or "-->" "-<<" "->>" "-|" "-~" "-<" "->") (+ "-"))))
           (?/  ,(rx (or (or "/==" "/=" "/>" "/**" "/*") (+ "/"))))
           (?*  ,(rx (or (or "*>" "*/") (+ "*"))))
           (?<  ,(rx (or (or "<<=" "<<-" "<|||" "<==>" "<!--" "<=>" "<||" "<|>" "<-<"
                             "<==" "<=<" "<-|" "<~>" "<=|" "<~~" "<$>" "<+>" "</>" "<*>"
                             "<->" "<=" "<|" "<:" "<>"  "<$" "<-" "<~" "<+" "</" "<*")
                         (+ "<"))))
           (?:  ,(rx (or (or ":?>" "::=" ":>" ":<" ":?" ":=") (+ ":"))))
           (?=  ,(rx (or (or "=>>" "==>" "=/=" "=!=" "=>" "=:=") (+ "="))))
           (?!  ,(rx (or (or "!==" "!=") (+ "!"))))
           (?>  ,(rx (or (or ">>-" ">>=" ">=>" ">]" ">:" ">-" ">=") (+ ">"))))
           (?&  ,(rx (+ "&")))
           (?|  ,(rx (or (or "|->" "|||>" "||>" "|=>" "||-" "||=" "|-" "|>" "|]" "|}" "|=")
                         (+ "|"))))
           (?.  ,(rx (or (or ".?" ".=" ".-" "..<") (+ "."))))
           (?+  ,(rx (or "+>" (+ "+"))))
           (?\[ ,(rx (or "[<" "[|")))
           (?\{ ,(rx "{|"))
           (?\? ,(rx (or (or "?." "?=" "?:") (+ "?"))))
           (?#  ,(rx (or (or "#_(" "#[" "#{" "#=" "#!" "#:" "#_" "#?" "#(") (+ "#"))))
           (?\; ,(rx (+ ";")))
           (?_  ,(rx (or "_|_" "__")))
           (?~  ,(rx (or "~~>" "~~" "~>" "~-" "~@")))
           (?$  ,(rx "$>"))
           (?^  ,(rx "^="))
           (?\] ,(rx "]#"))))
  (apply (lambda (char ligature-re)
           (set-char-table-range composition-function-table char
                                 `([,ligature-re 0 font-shape-gstring])))
         char/ligature-re))

;;
; Variables
;;

(setq ring-bell-function 'ignore)
(setq vc-follow-symlinks t)
(setq all-the-icons-scale-factor 1.2)
(setq confirm-kill-processes nil)
(setq kill-buffer-query-functions nil)
(treemacs-load-theme "all-the-icons")
(setq inferior-lisp-program "sbcl")
(setf nyan-animate-nyancat t)
(setf nyan-animation-frame-interval 0.05)
(setf nyan-wavy-trail t)
(setq-default cursor-type 'bar)

;; Encoding
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment 'utf-8)
(set-selection-coding-system 'utf-8)

;; Transparency
(set-frame-parameter nil 'alpha-background 98)
(add-to-list 'default-frame-alist '(alpha-background . 98))

;; Syntax Highlight
(if (fboundp 'global-font-lock-mode)
    (global-font-lock-mode 1))

;; Show-Parens
(show-paren-mode 1)

;; Delete without register
(evil-define-operator evil-delete-without-register (beg end type yank-handler)
  (interactive "<R><y>")
  (evil-delete beg end type ?_ yank-handler))
(define-key evil-normal-state-map (kbd "d") 'evil-delete-without-register)
(define-key evil-visual-state-map (kbd "d") 'evil-delete-without-register)
(define-key evil-normal-state-map (kbd "D") 'evil-delete)
(define-key evil-visual-state-map (kbd "D") 'evil-delete)

;; LSP
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-minimum-prefix-length 1)
      
;; Fix Unicode in vterm
(add-hook 'vterm-mode-hook
          (lambda ()
            (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix)))

;; Emms
(require 'emms-setup)
(emms-all)
(setq emms-player-list '(emms-player-mpv))

;; No Suggs
(setq suggest-key-bindings nil)

;; Hide emphasis markers on formatted text
(setq org-hide-emphasis-markers t)

;; Clear Minibuffer
(global-set-key [escape] 'keyboard-escape-quit)

;;
; Global Modes
;;

;; Set src block automatic indent to 0 instead of 2.
(require 'linum)
(setq org-edit-src-content-indentation 0) 
(add-hook 'prog-mode-hook 'rainbow-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(menu-bar-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode 1)
(delete-selection-mode 1)
(electric-pair-mode 1)
(electric-indent-mode -1)
(global-prettify-symbols-mode t)
(global-company-mode t)
(global-flycheck-mode t)
(which-key-mode t)
(desktop-save-mode -1)
(global-auto-revert-mode t)
(global-visual-line-mode t)
(elcord-mode)
(nyan-mode t)
