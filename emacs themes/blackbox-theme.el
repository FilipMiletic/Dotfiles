;;; blackbox-theme.el --- Black and LightGray theme with not so much syntax highlighting.

;; Author: Filip Miletic <miletic.phil@gmail.com>

;;; Commentary:

;; Black and LightGray theme with limited syntax highlighting focusing on:
;; 1. keywords -- a bit of standout (just white)
;; 2. strings -- clearly visible, to be sure quotes are closed (cyan3)
;; 3. comments -- should standout of other source code (green3).

;;; Code:

(deftheme blackbox "Have you set up optical filters for you space suit?")

(let
    ((blackbox-fg "#b2b2b2")
     (blackbox-bg "#101010")
     (blackbox-bg-modeline-active "#1f2f5f")
     (blackbox-bg-modeline-inactive "#20202f")
     (blackbox-box-modeline-active "#3f5f5f")
     (blackbox-box-modeline-inactive "#303030")
	 ;; keyword f5f8fa
     (blackbox-keyword "#ffffff")
     ;; str 63eb63, 00dbb1
	 (blackbox-str "#00ecec")
	 ;; comment  63eb63
     (blackbox-comment "#00fc3c")
     (blackbox-special "#ffa500")
     (blackbox-gray "#777777")
     (blackbox-fg-dim "#777777")
     (blackbox-bg-dim "#141416")
     (blackbox-fg-dim-2 "#506060")
     (blackbox-fg-todo "#bdabab")
     (blackbox-bg-todo "#ff0055")
     (blackbox-fg-done "#abbdab")
     (blackbox-bg-done "#777777")
     (blackbox-title "#f0f07f")
     (blackbox-heading "#ffffff")
     (blackbox-bg-hl-parens "#e242ac")
     (blackbox-bg-alt "#1E282D")
     (blackbox-fg-modeline-hl "#ffff00")
     (blackbox-bg-whitespace "#202020")
     (blackbox-fg-whitespace "#555555")
     (blackbox-fg-search "#fff68f")
     (blackbox-bg-search "#606020")
     (blackbox-fg-lazysearch "#40e0d0")
     (blackbox-bg-lazysearch "#206060")
     (blackbox-fg-search-fail "#da70d6")
     (blackbox-bg-search-fail "#603060")
     (blackbox-bg-highlight "#203040")
     (blackbox-fg-url "#90a0bd")
	 (blackbox-random "#9e33ab"))


  (custom-theme-set-faces
   'blackbox

   ;; standard faces
   `(default ((t (:background ,blackbox-bg :foreground ,blackbox-fg))))
   '(cursor ((nil (:background "#ff2d65"))))
   `(region ((t (:background "#839191" :foreground ,blackbox-bg))))
   `(highlight ((nil (:background ,blackbox-bg-highlight))))
   '(bold ((t (:weight bold))))
   '(minibuffer-prompt ((t (:foreground "#63EB63" :weight bold))))
   '(widget-field-face ((t (:background "#a0a0a0" :foreground "#000000"))))
   ;; `(header-line ((t (:foreground ,blackbox-keyword :background "#6b5e46" :box (:line-width 1 :color "#8d7a56" :style none)))))
   `(header-line ((t (:foreground ,blackbox-keyword :background "#404040"))))
   `(hl-line ((t (:background "#141416"))))
   '(highlight-numbers-number ((t (:foreground "#EC00BA"))))
   `(isearch ((t (:background ,blackbox-bg-search :foreground ,blackbox-fg-search :weight bold :underline (:color ,blackbox-fg-search)))))
   `(lazy-highlight ((t (:background ,blackbox-bg-lazysearch :foreground ,blackbox-fg-lazysearch :weight bold :underline (:color ,blackbox-fg-lazysearch)))))
   ;; match?
   `(isearch-fail ((t (:background ,blackbox-bg-search-fail :foreground ,blackbox-fg-search-fail :weight bold :underline (:color ,blackbox-fg-search-fail)))))
   

   ;; frame UI
   `(mode-line ((t (:background "#333333" :foreground "#ffffff"))))
   `(mode-line-inactive ((t (:background "#181818" :foreground ,blackbox-gray))))
   `(mode-line-buffer-id ((t (:foreground ,blackbox-comment))))
   `(mode-line-highlight ((nil (:foreground ,blackbox-fg-modeline-hl))))
   `(vertical-border ((nil (:foreground ,blackbox-box-modeline-inactive))))
   `(fringe ((nil (:background ,blackbox-bg-dim))))


   ;; syntax font-lock I DO care about
   ;; Classical colors all the way. 90s child after all...
   `(font-lock-string-face ((t (:foreground ,blackbox-str))))
   `(font-lock-comment-face ((t (:foreground ,blackbox-comment))))
   ;;`(font-lock-comment-face ((((class color) (background dark)) (:foreground "green"))))
   ;;`(font-lock-string-face ((((class color) (background dark)) (:foreground "cyan"))))

   `(font-lock-keyword-face ((t (:foreground ,blackbox-keyword))))
   ;; syntax font-lock I DON'T care about
   '(font-lock-builtin-face ((t nil)))
   `(font-lock-type-face ((t nil)))
   '(font-lock-function-name-face ((t nil)))
   '(font-lock-variable-name-face ((t nil)))
   '(font-lock-constant-face ((t nil)))
   
   ;; review this later.
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,blackbox-str :weight bold))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,blackbox-str :weight bold :slant italic))))


   ;; parenthesis and pairs
   `(show-paren-match ((t :background "#FFD603" :foreground "black")))
   `(sp-show-pair-match-face ((t (:background ,blackbox-bg-hl-parens))))


   ;; links
   `(link ((t (:foreground ,blackbox-fg-url :underline (:color ,blackbox-fg-url)))))
   `(link-visited ((t (:foreground ,blackbox-fg :underline (:color ,blackbox-fg)))))

   ;; dired
   '(dired-directory ((t (:inherit font-lock-keyword-face :weight bold))))

   ;; flycheck
   '(flycheck-warning ((t (:underline (:color "Wheat3" :style wave)))))
   '(flycheck-error ((t (:underline (:color "Coral" :style wave)))))
   '(flycheck-fringe-warning ((t (:foreground "Wheat3"))))
   '(flycheck-fringe-error ((t (:foreground "Coral"))))

   ;; which-key
   '(which-key-key-face ((t (:foreground "#ffa500"))))
   '(which-key-separator-face ((t (:foreground "#4d4d4d"))))

   ;; company
   '(company-tooltip ((t (:background "Gray20" :foreground "Gray80"))))
   '(company-tooltip-selection ((t (:background "LightSteelBlue4" :foreground "White"))))
   '(company-tooltip-annotation ((t (:foreground "Gray60"))))
   '(company-tooltip-annotation-selection ((t (:foreground "Gray80"))))
   '(company-tooltip-common ((t ())))
   '(company-scrollbar-bg ((t (:background "Gray40"))))
   '(company-scrollbar-fg ((t (:background "Gray80"))))
   '(company-preview ((t (:inherit company-tooltip-selection))))
   '(company-preview-common ((t (:inherit company-preview))))

   ;; erc
   '(erc-current-nick-face ((t (:foreground "#ffffff"))))
   '(erc-my-nick-face ((t (:foreground "#cc5555"))))
   '(erc-input-face ((t (:foreground "#8dbdbd"))))
   '(erc-timestamp-face ((t (:foreground "Wheat"))))
   '(erc-notice-face ((t (:foreground "#555555"))))
   '(erc-action-face ((nil (:slant italic))))
   '(erc-button ((t (:underlined on))))

   ;; circe
   `(circe-fool ((t (:foreground ,blackbox-str))))
   '(circe-highlight-nick-face ((t (:weight bold :foreground "#887FD5"))))
   '(circe-prompt-face ((t (:weight bold :foreground "#EB77EC"))))
   `(circe-server-face ((t (:foreground ,blackbox-fg-lazysearch ))))
   '(circe-my-message-face ((t (:weight bold))))

   ;; magit
   '(git-commit-summary ((t (:inherit font-lock-string-face))))

   ;; git gutter fringe
   `(git-gutter-fr:modified ((nil (:background ,blackbox-bg-dim :foreground "#ff55ff"))))
   `(git-gutter-fr:added ((nil (:background ,blackbox-bg-dim :foreground "#55ff55"))))
   `(git-gutter-fr:deleted ((nil (:background ,blackbox-bg-dim :foreground "#ff5555"))))


   ;; ivy
   `(ivy-current-match ((t (:background ,blackbox-bg-highlight))))
   `(ivy-minibuffer-match-face-1 ((t (:foreground ,blackbox-fg-dim))))
   `(ivy-minibuffer-match-face-2 ((t (:foreground ,blackbox-fg-search :weight bold :underline (:color ,blackbox-fg-search)))))
   `(ivy-minibuffer-match-face-3 ((t (:foreground ,blackbox-fg-lazysearch :weight bold :underline (:color ,blackbox-bg-lazysearch)))))
   `(ivy-minibuffer-match-face-4 ((t (:foreground ,blackbox-fg-search-fail :weight bold :underline (:color ,blackbox-bg-search-fail)))))
   `(ivy-modified-buffer ((nil (:foreground ,blackbox-keyword :slant italic))))
   '(ivy-remote ((t (:inherit font-lock-comment-face))))
   `(ivy-virtual ((t (:foreground ,blackbox-fg-dim))))

   ;; swiper
   `(swiper-match-face-1 ((t (:foreground ,blackbox-fg-dim))))
   `(swiper-match-face-2 ((t (:background ,blackbox-bg-search :foreground ,blackbox-fg-search :weight bold :underline (:color ,blackbox-fg-search)))))
   `(swiper-match-face-3 ((t (:background ,blackbox-bg-lazysearch :foreground ,blackbox-fg-lazysearch :weight bold :underline (:color ,blackbox-bg-lazysearch)))))
   `(swiper-match-face-4 ((t (:background ,blackbox-bg-search-fail :foreground ,blackbox-fg-search-fail :weight bold :underline (:color ,blackbox-bg-search-fail)))))


   ;; org
   `(org-document-title ((t (:foreground ,blackbox-keyword :weight bold :height 1.6))))
   
   `(org-level-1  ((t (:foreground "#887FD5" :weight bold :height 1.3))))
   `(org-level-2  ((t (:foreground "#EB77EC" :weight bold :height 1.2))))
   `(org-level-3  ((t (:foreground "#FFBB44" :weight bold :height 1.1))))
   `(org-level-4  ((t (:foreground "#EFEF66" :slant italic :height 1.1))))
   `(org-level-5  ((t (:foreground "#63EB63" :slant italic :height 1.1))))
   `(org-level-6  ((t (:foreground "#55F1E1" :slant italic :height 1.1))))
   `(org-level-7  ((t (:foreground "#70BFFF" :slant italic :height 1))))
   `(org-level-8  ((t (:foreground "#887FD5" :slant italic :height 1))))
   `(org-level-9  ((t (:foreground "#EB77EC" :slant italic :height 1))))
   `(org-level-10 ((t (:foreground "#FFBB44" :slant italic :height 1))))

   `(org-tag ((nil (:foreground ,blackbox-comment))))

   `(org-archived ((nil (:foreground ,blackbox-gray))))
   ;; todo: play with colors of the box
   `(org-todo ((nil (:background ,blackbox-bg-todo :foreground ,blackbox-fg-todo :weight bold :box (:line-width 1 :color ,blackbox-fg-todo)))))
   `(org-done ((nil (:background ,blackbox-bg-done :foreground ,blackbox-fg-done :weight bold :box (:line-width 1 :color ,blackbox-fg-done)))))

   '(org-table ((t (:inherit default))))

   `(org-date ((t (:foreground ,blackbox-comment :underline (:color ,blackbox-comment)))))

   `(org-verbatim ((nil (:background ,blackbox-bg-alt :foreground ,blackbox-fg))))

   `(org-special-keyword ((t (:foreground ,blackbox-gray :background ,blackbox-bg-modeline-inactive))))

   `(org-agenda-structure ((t (:foreground ,blackbox-fg :height 1.6 :weight bold))))
   `(org-agenda-date ((nil (:height 1.0))))
   `(org-agenda-date-today ((t (:height 1.5 :weight bold))))
   `(org-agenda-date-weekend ((t (:foreground ,blackbox-title :height 1.3))))

   '(org-scheduled ((t (:inherit :default))))
   `(org-scheduled-today ((t (:inherit :default :foreground ,blackbox-keyword))))
   `(org-scheduled-previously ((t (:inherit :default :foreground "#d05050"))))
   `(org-agenda-done ((t (:inherit :default :foreground ,blackbox-gray))))
   `(org-warning ((t (:foreground "#d0a000"))))

   `(org-agenda-clocking ((t (:background "#303030"))))

   '(org-meta-line ((t (:foreground "#707070" ))))
   '(org-document-info-keyword ((t (:inherit org-meta-line))))

   `(org-block-begin-line ((t :foreground ,blackbox-comment)))
   `(org-block-end-line ((t (:foreground ,blackbox-comment))))


   ;; calendar
   `(calendar-month-header ((t (:foreground ,blackbox-keyword :weight bold))))
   `(calendar-weekday-header ((t (:foreground ,blackbox-comment))))
   `(calendar-weekend-header ((t (:foreground ,blackbox-str :weight bold))))
   `(calendar-today ((t (:foreground ,blackbox-keyword :weight bold))))


   ;; emms
   '(emms-playlist-track-face ((t (:inherit default))))
   '(emms-playlist-selected-face ((t (:background "#20408b" :foreground "white" :weight bold))))


   ;; LaTeX
   '(font-latex-sectioning-1-face ((t (:inherit org-level-1))))
   '(font-latex-sectioning-2-face ((t (:inherit org-level-2))))
   '(font-latex-sectioning-3-face ((t (:inherit org-level-3))))
   '(font-latex-string-face ((t (:inherit font-lock-string-face))))
   '(font-latex-bold-face ((t (:inherit bold))))

   ;; CSS
   '(css-selector ((t (:inherit font-lock-keyword-face))))

   
   ;; XML
   `(nxml-element-local-name ((t (:foreground ,blackbox-fg-dim))))
   `(nxml-tag-delimiter ((t (:foreground ,blackbox-fg-dim))))
   `(nxml-namespace-attribute-xmlns ((t (:foreground ,blackbox-fg-dim))))
   `(nxml-attribute-local-name ((t (:foreground ,blackbox-fg-dim))))
   `(nxml-attribute-value ((t (:foreground ,blackbox-str))))
   `(nxml-cdata-section-CDATA ((t (:foreground ,blackbox-fg-dim))))
   `(nxml-cdata-section-content ((t (:foreground ,blackbox-fg))))
   `(nxml-cdata-section-delimiter ((t (:foreground ,blackbox-fg-dim-2))))


   ;; whitespace-mode
   `(whitespace-space ((t (:foreground ,blackbox-fg-whitespace))))
   `(whitespace-newline ((t (:foreground ,blackbox-fg-whitespace))))
   `(whitespace-indentation ((t (:foreground ,blackbox-fg-whitespace :background ,blackbox-bg-whitespace))))
   `(whitespace-line ((nil (:background ,blackbox-bg-whitespace))))


   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,blackbox-fg))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground "#887FD5"))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground "#EB77EC"))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground "#FFBB44"))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground "#EFEF66"))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground "#63EB63"))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground "#55F1E1"))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground "#70BFFF"))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground "#887FD5"))))


   ;; asciidoctor-mode
   `(asciidoctor-header-delimiter-face ((t (:foreground ,blackbox-fg-dim))))
   `(asciidoctor-header-face-1 ((t (:foreground ,blackbox-title :weight bold :height 1.3))))
   `(asciidoctor-header-face-2 ((t (:foreground ,blackbox-heading :weight bold :height 1.3))))
   `(asciidoctor-header-face-3 ((t (:foreground ,blackbox-heading :weight bold :height 1.2))))
   `(asciidoctor-header-face-4 ((t (:foreground ,blackbox-heading :weight bold :height 1.1))))
   `(asciidoctor-header-face-5 ((t (:foreground ,blackbox-heading :slant italic :height 1.1))))
   `(asciidoctor-header-face-6 ((t (:foreground ,blackbox-heading :slant italic :height 1.1))))
   `(asciidoctor-option-face ((t (:foreground ,blackbox-fg-dim))))
   `(asciidoctor-option-markup-face ((t (:foreground ,blackbox-fg-dim))))
   )
  )

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'blackbox)
;;; blackbox-theme.el ends here
