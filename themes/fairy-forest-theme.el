;;; fairy-forest-theme.el --- -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Author: twig <https://github.com/ImNotTwig>
;; Maintainer: Twig
;;
;;; Commentary:
;;
;;; Code:

(require 'doom-themes)

;;
;;; Variables

(defgroup fairy-forest-theme nil
  "Options for doom-themes."
  :group 'doom-themes)

(defcustom fairy-forest-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'fairy-forest-theme
  :type 'boolean)

(defcustom fairy-forest-padded-modeline nil
  "If non-nil, adds a 4px padding to the mode-line.  Can be an integer to determine the exact padding."
  :group 'fairy-forest-theme
  :type '(or integer boolean))


;;
;;; Theme definition

(def-doom-theme
 fairy-forest
 "A pastel on black theme, that has mystical forest vibes."
 ;; name        default   256       16
 ((bg         '("#0F1010" nil       nil))
  (bg-alt     '("#0F1010" nil       nil))
  (fg-alt     '("#808080" "#808080" "brightwhite"))
  (fg         '("#C9C7CD" "#C9C7CD" "white"))

  (base0      '("#1B1B1B" "#1B1B1B" "black"        ))
  (base1      '("#1B1B1B" "#1B1B1B" "brightblack"  ))
  (base2      '("#2A2A2A" "#2A2A2A" "brightblack"  ))
  (base3      '("#2A2A2A" "#2A2A2A" "brightblack"  ))
  (base4      '("#3E3E43" "#3E3E43" "brightblack"  ))
  (base5      '("#57575F" "#57575F" "brightblack"  ))
  (base6      '("#57575F" "#57575F" "brightblack"  ))
  (base7      '("#9998A8" "#9998A8" "brightblack"  ))
  (base8      '("#9998A8" "#9998A8" "white"        ))

  (grey       base4)
  (red        '("#F8C8DC" "#F8C8DC" "red"))
  (orange     '("#F5BBA1" "#F5BBA1" "brightred"))
  (green      '("#BEE7c5" "#BEE7c5" "green"))
  (yellow     '("#F5BBA1" "#F5BBA1" "yellow"))
  (blue       '("#8EC4E5" "#8EC4E5" "brightblue"))
  (dark-blue  '("#8EB6F5" "#8EB6F5" "blue"))
  (magenta    '("#F49AC2" "#F49AC2" "magenta"))
  (teal       '("#F49AC2" "#F49AC2" "magenta"))
  (violet     '("#C3B1E1" "#C3B1E1" "brightmagenta"))
  (cyan       '("#C1E7E3" "#C1E7E3" "brightcyan"))
  (dark-cyan  '("#C1E7E3" "#C1E7E3" "cyan"))
  ;; Additional custom colors
  (dark-green '("#BFD7b5" "#BFD7b5" "green"))
  (beige      '("#808080" "#808080" "yellow"))
  (err        '("#FCA5A5" "#FCA5A5" "red"))
  (str_color  '("#9998A8" "#9998A8" "white"))
  (white      '("#E1DBEB" "#E1DBEB" "white"))

  ;; face categories -- required for all themes
  (highlight      red)
  (vertical-bar   (doom-lighten bg 0.05))
  (selection      base5)
  (builtin        red)
  (comments       beige)
  (doc-comments   white)
  (constants      red)
  (functions      dark-green)
  (keywords       violet)
  (methods        yellow)
  (operators      dark-blue)
  (type           dark-cyan)
  (strings        str_color)
  (variables      fg)
  (numbers        yellow)
  (region         base0)
  (error          err)
  (warning        yellow)
  (success        dark-green)
  (vc-modified    yellow)
  (vc-added       green)
  (vc-deleted     red)

  ;; custom categories
  (hidden     `(,(car bg) "black" "black"))
  (-modeline-bright fairy-forest-brighter-modeline)
  (-modeline-pad
   (when fairy-forest-padded-modeline
     (if (integerp fairy-forest-padded-modeline) fairy-forest-padded-modeline 4)))

  (modeline-fg     'unspecified)
  (modeline-fg-alt base5)

  (modeline-bg
   (if -modeline-bright
       base2
     `(,(doom-darken (car bg) 0.15) ,@(cdr base0))))
  (modeline-bg-l
   (if -modeline-bright
       base2
     `(,(doom-darken (car bg) 0.1) ,@(cdr base0))))
  (modeline-bg-inactive   (doom-darken bg 0.1))
  (modeline-bg-inactive-l `(,(car bg) ,@(cdr base0))))

 ;; --- Extra Faces ------------------------
 (
  ((line-number-current-line &override) :foreground base4)
  ((line-number &override) :foreground base5)

  ((font-lock-comment-face &override) :foreground beige :slant 'italic)
  ((font-lock-comment-delimiter-face &override) :foreground beige :slant 'italic)

  ((font-lock-doc-face &override) :foreground white :slant 'italic :weight 'bold)
  ((font-lock-warning-face &override) :foreground err :slant 'italic :weight 'bold)
  ((font-lock-preprocessor-face &override) :foreground dark-green)
  ((font-lock-operator-face &override) :foreground dark-blue)

  ((font-lock-punctuation-face &override) :foreground violet)
  ((font-lock-delimiter-face &override) :foreground violet)
  ((font-lock-bracket-face &override) :foreground red)

  ((font-lock-string-face &override) :foreground str_color)
  ((font-lock-regexp-face &override) :foreground cyan)
  ((font-lock-escape-face &override) :foreground white)

  ((font-lock-number-face &override) :foreground yellow)

  ((font-lock-function-call-face &override) :foreground dark-green :slant 'italic)
  ((font-lock-function-name-face &override) :foreground dark-green :slant 'italic)

  ((font-lock-keyword-face &override) :foreground violet)

  ((font-lock-type-face &override) :foreground dark-cyan)
  ((font-lock-builtin-face &override) :foreground red)

  ((font-lock-property-use-face &override) :foreground yellow)
  ((font-lock-property-name-face &override) :foreground yellow :slant 'italic)

  ((font-lock-variable-use-face &override) :foreground fg)
  ((font-lock-variable-name-face &override) :foreground fg :slant 'italic)

  ((font-lock-constant-face &override) :foreground red :weight 'bold)
  ((font-lock-negation-char-face &override) :foreground magenta :slant 'italic :weight 'bold)

  ((dashboard-text-banner &override) :foreground red)
  ((dashboard-banner-logo-title &override) :foreground red)
  ((dashboard-heading &override) :foreground red)
  ((dashboard-items-face &override) :foreground violet)

;;; Doom Modeline
  (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
  (doom-modeline-buffer-path :foreground base7 :weight 'normal)
  (doom-modeline-buffer-file :foreground beige :weight 'normal)

  (mode-line
   :background modeline-bg :foreground modeline-fg
   :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
  (mode-line-inactive
   :background modeline-bg-inactive :foreground modeline-fg-alt
   :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
  (mode-line-emphasis
   :foreground (if -modeline-bright base7 highlight))
  (mode-line-buffer-id
   :foreground highlight)

;;; Ivy
  (ivy-subdir :foreground blue)
  (ivy-minibuffer-match-face-1 :foreground green :background bg-alt)
  (ivy-minibuffer-match-face-2 :foreground yellow :background bg-alt)
  (ivy-minibuffer-match-face-3 :foreground red :background bg-alt)
  (ivy-minibuffer-match-face-4 :foreground yellow :background bg-alt)

;;; Elscreen
  (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

;;; Solaire
  (solaire-mode-line-face
   :inherit 'mode-line
   :background modeline-bg-l
   :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
  (solaire-mode-line-inactive-face
   :inherit 'mode-line-inactive
   :background modeline-bg-inactive-l
   :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

;;; Telephone
  (telephone-line-accent-active
   :inherit 'mode-line
   :background (doom-lighten bg 0.2))
  (telephone-line-accent-inactive
   :inherit 'mode-line
   :background (doom-lighten bg 0.05))
  (telephone-line-evil-emacs
   :inherit 'mode-line
   :background dark-blue)

;;;; rainbow-delimiters
  (rainbow-delimiters-depth-1-face :foreground red)
  (rainbow-delimiters-depth-2-face :foreground yellow)
  (rainbow-delimiters-depth-3-face :foreground blue)
  (rainbow-delimiters-depth-4-face :foreground magenta)
  (rainbow-delimiters-depth-5-face :foreground green)
  (rainbow-delimiters-depth-6-face :foreground violet)
  (rainbow-delimiters-depth-7-face :foreground cyan)

;;; Magit
  (magit-section-heading :foreground blue)
  (magit-branch-remote   :foreground yellow)
  (magit-diff-our :foreground (doom-darken red 0.2) :background (doom-darken red 0.7))
  (magit-diff-our-highlight :foreground red :background (doom-darken red 0.5) :weight 'bold)
  (magit-diff-removed :foreground (doom-darken red 0.2) :background (doom-darken red 0.7))
  (magit-diff-removed-highlight :foreground red :background (doom-darken red 0.5) :weight 'bold)

  ;; --- Major-Mode Faces -------------------
;;; elisp
  (highlight-quoted-symbol :foreground yellow)

;;; js2-mode
  (js2-function-param :foreground yellow)
  (js2-object-property :foreground green)

;;; typescript-mode
  (typescript-this-face :foreground red)
  (typescript-access-modifier-face :foreground beige)

;;; rjsx-mode
  (rjsx-tag :foreground red)
  (rjsx-text :foreground violet)
  (rjsx-attr :foreground magenta :slant 'italic :weight 'medium)
  (rjsx-tag-bracket-face :foreground (doom-darken red 0.3))

;;; css-mode / scss-mode
  (css-property             :foreground blue)
  (css-selector             :foreground magenta)
  (css-pseudo-class         :foreground yellow)

;;; markdown-mode
  (markdown-markup-face :foreground violet)
  (markdown-header-face :inherit 'bold :foreground cyan)
  (markdown-blockquote-face :foreground violet :background (doom-lighten bg 0.04))
  (markdown-table-face :foreground violet :background (doom-lighten bg 0.04))
  ((markdown-code-face &override) :foreground cyan :background (doom-lighten bg 0.04))

;;; org-mode
  (org-hide :foreground hidden)
  (org-block :background (doom-darken base2 0.90))
  (org-block-begin-line :background (doom-darken base2 0.65) :foreground comments :extend t)
  (solaire-org-hide-face :foreground hidden)

;;; web-mode
  (web-mode-json-context-face :foreground beige)
  (web-mode-json-key-face :foreground magenta)
;;;; Block
  (web-mode-block-delimiter-face :foreground yellow)
;;;; Code
  (web-mode-constant-face :foreground constants)
  (web-mode-variable-name-face :foreground variables)
;;;; CSS
  (web-mode-css-pseudo-class-face :foreground yellow)
  (web-mode-css-property-name-face :foreground blue)
  (web-mode-css-selector-face :foreground magenta)
  (web-mode-css-function-face :foreground yellow)
;;;; HTML
  (web-mode-html-attr-engine-face :foreground yellow)
  (web-mode-html-attr-equal-face :foreground operators)
  (web-mode-html-attr-name-face :foreground magenta)
  (web-mode-html-tag-bracket-face :foreground (doom-darken red 0.3))
  (web-mode-html-tag-face :foreground red)))

;; --- extra variables ---------------------
;; ()

(provide 'fairy-forest)
;;; fairy-forest-theme.el ends here
