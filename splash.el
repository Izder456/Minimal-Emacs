;;
; Dash Settings
;;

;; Set the title
(setq dashboard-banner-logo-title "Giegue!")
;; Set the banner
(setq dashboard-startup-banner "~/.emacs.d/giegue.png")

;; Content is not centered by default. To center, set
(setq dashboard-center-content t)

;; To disable shortcut "jump" indicators for each section, set
(setq dashboard-show-shortcuts nil)

;; Icons
(setq dashboard-icon-type 'all-the-icons)
(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)

;;
; Dash Items
;;

(setq dashboard-items '((recents  . 5)
                        (projects . 5)))

(dashboard-modify-heading-icons '((recents . "file-text")
                                  (projects . "book")))

;;
; Config
;;

;; Projects
(setq dashboard-projects-switch-function 'projectile-persp-switch-project)

;; show info about the packages loaded and the init time:
(setq dashboard-set-init-info t)

;; Footer
(setq dashboard-set-footer t)

;; show navigator below the banner:
(setq dashboard-set-navigator t)

(provide 'init-startup)