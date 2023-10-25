;; Base packages
(dolist (package '(compat
                   graphene
                   elcord
                   all-the-icons
                   paredit
                   evil
                   doom-modeline
                   dashboard
                   gruvbox-theme
                   default-text-scale))
  (unless (package-installed-p package)
    (package-install package)))

;; Enable Graphene
(require 'graphene)

;; Enable Dashboard
(require 'dashboard)
(dashboard-setup-startup-hook)
(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
(load "~/.emacs.d/splash.el")

;; Enable Doom-Modeline
(require 'doom-modeline)
(doom-modeline-mode 1)
(load "~/.emacs.d/modeline.el")

;; Enable Evil
(require 'evil)
(evil-mode 1)

;; Theme
(require 'gruvbox-theme)
(load-theme 'gruvbox t)

;; Fonts
;; scale entire UI, solves annoying mismatches
(global-set-key (kbd "C-=") 'default-text-scale-increase)
(global-set-key (kbd "C--") 'default-text-scale-decrease)
(global-set-key (kbd "C-0") 'default-text-scale-reset)
(global-set-key (kbd "<C-mouse-4>") 'default-text-scale-increase)
(global-set-key (kbd "<C-mouse-5>") 'default-text-scale-decrease)

;; Dired Packages
(dolist (package '(dired-hacks-utils
                   dired-ranger
                   all-the-icons-dired))
   (unless (package-installed-p package)
     (package-install package)))
;; Enable Dired     
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

;; Extra packages
(dolist (package '(dumb-jump
                   nyan-mode
                   ripgrep
                   sly
                   sly-quicklisp
                   vterm
                   which-key
                   tempel
                   tempel-collection
                   projectile
                   projectile-ripgrep
                   treemacs
                   treemacs-projectile
                   treemacs-all-the-icons
                   emms
                   ement
                   rainbow-mode
                   rainbow-delimiters
                   ligature
                   visual-fill-column
                   org-bullets
                   web-mode))
  (unless (package-installed-p package)
    (package-install package))
  (require package))

;;
; Dev
;;

;; Lsp
(dolist (package '(lsp-mode
                   lsp-treemacs
                   flycheck
                   company))
  (unless (package-installed-p package)
    (package-install package))
  (require package))


(with-eval-after-load 'lsp-mode
  (setq lsp-modeline-diagnostics-scope :workspace)
  (setq lsp-auto-configure t))
  
 

;; Company Completions
(defun company-complete-common-or-cycle ()
  "Company settings."
  (interactive)
  (when (company-manual-begin)
    (if (eq last-command 'company-complete-common-or-cycle)
        (let ((company-selection-wrap-around t))
          (call-interactively 'company-select-next))
      (call-interactively 'company-complete-common))))

(define-key company-active-map [tab] 'company-complete-common-or-cycle)
(define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
(setq company-insertion-on-trigger 'company-explicit-action-p)
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

(load "~/.emacs.d/lang.el")