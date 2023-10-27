;; Base packages
(dolist (package '(compat
                   graphene
                   vterm
                   vterm-toggle
                   elcord
                   all-the-icons
                   evil
                   doom-modeline
                   dashboard
                   beacon
                   gruvbox-theme
                   eshell-syntax-highlighting))
  (unless (package-installed-p package)
    (package-install package)))

;; Enable Graphene
(require 'graphene)

;; Eshell Syntax Highlight
(eshell-syntax-highlighting-global-mode +1)

;; VTerm
(setq vterm-max-scrollback 5000)
(setq vterm-toggle-fullscreen-p nil)
(setq vterm-toggle-scope 'project)

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
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

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
                   tempel
                   tempel-collection
                   projectile
                   projectile-ripgrep
                   persp-projectile
                   treemacs
                   treemacs-projectile
                   treemacs-all-the-icons
                   paredit
                   toc-org
                   org-bullets
                   emms
                   ement
                   rainbow-mode
                   rainbow-delimiters
                   ligature
                   visual-fill-column
                   which-key
                   web-mode))
  (unless (package-installed-p package)
    (package-install package))
  (require package))


;;
; Org-Mode                                      ;
;;

(require 'toc-org)
(add-hook 'org-mode-hook 'toc-org-enable)
(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;;
; Which-Key                                      ;
;;
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
      which-key-separator " → " )

;;
; Dev
;;

;; Lsp
(dolist (package '(lsp-mode
                   lsp-treemacs
                   flycheck
                   company
                   company-box
                   yasnippet
                   hydra
                   helm
                   helm-lsp
                   helm-xref))
  (unless (package-installed-p package)
    (package-install package))
  (require package))

(with-eval-after-load 'lsp-mode
  (setq lsp-modeline-diagnostics-scope :workspace)
  (setq lsp-auto-configure t))

;; Company settings
(add-hook 'company-mode-hook 'company-box-mode)
(global-company-mode t)

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

(load-file "~/.emacs.d/lang.el")
