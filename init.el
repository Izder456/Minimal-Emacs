;; Require Emacs' package functionality
(require 'package)

;; Add the Melpa repository to the list of package sources
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")) ;; installed by default
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/")) ;; installed by default from Emacs 28 onwards
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; Initialise the package system.
(package-initialize)
(load-file "~/.emacs.d/packages.el")
(load-file "~/.emacs.d/config.el")