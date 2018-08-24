;; doom-watch-theme.el --- firewatch inspired doom-theme
;;; Commentary:
(require 'doom-themes)
;;; Code:
;;
(defgroup doom-watch-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-watch-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-watch-theme
  :type 'boolean)

(defcustom doom-watch-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-watch-theme
  :type 'boolean)

(defcustom doom-watch-comment-bg doom-watch-brighter-comments
  "If non-nil, comments will have a subtle, darker background.
Enhancing their legibility."
  :group 'doom-watch-theme
  :type 'boolean)

(defcustom doom-watch-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-watch-theme
  :type '(or integer boolean))


;;
(def-doom-theme doom-watch
  "A dark theme based off of xero's watch vim colorscheme"

  ((bg         '("#181B1F"))
   (bg-alt     '("#21242b"))
   (base0      '("#1b2229"))
   (base1      '("#1c1f24"))
   (base2      '("#202328"))
   (base3      '("#23272e"))
   (base4      '("#3f444a"))
   (base5      '("#5b6268"))
   (base6      '("#73797e"))
   (base7      '("#9ca0a4"))
   (base8      '("#dfdfdf"))
   (fg         '("#ABB2BF"))
   (fg-alt     '("#828997"))

   (grey       base5)
   (red        '("#FF324E"))
   (orange     '("#D19A66"))
   (green      '("#48b46c"))
   (green-br   '("#C8AE9D"))
   (teal       '("#44b9b1"))
   (yellow     '("#FFCF57"))
   (blue       '("#6B9FFF"))
   (magenta    '("#d62d93"))
   (dark-blue  '("#8999ab"))
   (violet     '("#9b7ed0"))
   (cyan       '("#56B6C2"))
   (dark-cyan  '("#6A8FBF"))
   
   ;; face categories
   (highlight      orange)
   (vertical-bar   base0)
   (selection      base5)
   (builtin        cyan)
   (comments       (if doom-watch-brighter-comments dark-cyan base5))
   (doc-comments   (if doom-watch-brighter-comments (doom-lighten dark-cyan 0.15) (doom-darken yellow 0.15)))
   (constants      base8)
   (functions      orange)
   (keywords       green)
   (methods        cyan)
   (operators      magenta)
   (type           cyan)
   (strings        green-br)
   (variables      violet)
   (numbers        magenta)
   (region         base2)
   (error          red)
   (warning        orange)
   (success        green)
   (vc-modified    yellow)
   (vc-added       green-br)
   (vc-deleted     red)
   
   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (hidden-alt `(,(car bg-alt) "black" "black"))
   (-modeline-pad
    (when doom-watch-padded-modeline
      (if (integerp doom-watch-padded-modeline) doom-watch-padded-modeline 4)))

   (modeline-fg     "#bbc2cf")
   (modeline-fg-alt (doom-blend blue grey (if doom-watch-brighter-modeline 0.4 0.08)))
   
   (modeline-bg
    (if doom-watch-brighter-modeline
        `("#383f58" ,@(cdr base1))
      `(,(car base3) ,@(cdr base0))))
   (modeline-bg-l
    (if doom-watch-brighter-modeline
        modeline-bg
      `(,(doom-darken (car bg) 0.15) ,@(cdr base1))))
   (modeline-bg-inactive   (doom-darken bg 0.20))
   (modeline-bg-inactive-l `(,(doom-darken (car bg-alt) 0.2) ,@(cdr base0))))
  
  ;; --- extra faces ------------------------
  ((elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")
   
   (cursor :background "#ff2d65")
   
   (font-lock-comment-face
    :foreground comments
    :background (if doom-watch-comment-bg (doom-darken bg-alt 0.095)))
   
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)
   
   (mode-line-buffer-id :foreground green :bold bold)
   
   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground blue :bold bold)
   
   (magit-diff-added             :foreground (doom-darken green 0.2)  :background (doom-blend green bg 0.1))
   (magit-diff-added-highlight   :foreground green                    :background (doom-blend green bg 0.2) :weight 'bold)
   
   (doom-modeline-bar :background (if doom-watch-brighter-modeline modeline-bg highlight))
   (doom-modeline-buffer-path :foreground (if doom-watch-brighter-modeline base8 blue) :bold bold)

   (mode-line
    :background base3 :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,base3)))
   
   (mode-line-inactive
    :background base1 :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,base1)))
   
   (mode-line-emphasis
    :foreground (if doom-watch-brighter-modeline base8 highlight))
   (fringe :background base1)
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;; markdown-mode
   (markdown-header-face :inherit 'bold :foreground red)
      ;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground dark-cyan)
   (rainbow-delimiters-depth-2-face :foreground teal)
   (rainbow-delimiters-depth-3-face :foreground dark-blue)
   (rainbow-delimiters-depth-4-face :foreground green)
   (rainbow-delimiters-depth-5-face :foreground violet)
   (rainbow-delimiters-depth-6-face :foreground green)
   (rainbow-delimiters-depth-7-face :foreground magenta)
   ;; org-mode
   (org-hide :foreground hidden)
   (solaire-org-hide-face :foreground hidden-alt))


  ;; --- extra variables --------------------
  ;; ()

  )

;;; doom-watch-theme.el ends here
