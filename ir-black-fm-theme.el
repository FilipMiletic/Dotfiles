;;; ir-black-dz.el --- Port of ir-black theme, with modifications by DZ

;; MODIFIED by David Zhou (C) 2016

;; Copyright (C) 2012  Jon-Michael Deldin

;; Author: Jon-Michael Deldin <dev@jmdeldin.com>
;; Keywords: faces
;; Compatibility: 24
;; Version: 1.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is an Emacs 26 port of Todd Werth's IR Black theme available at URL
;; `http://blog.toddwerth.com/entries/8'.
;;
;;; Credits:

;; Thanks to Bozhidar Batsov for pointers and the autoloader from his
;; solarized theme at URL
;; `https://github.com/bbatsov/solarized-emacs/blob/master/solarized-theme.el'.

;;; Code:

(deftheme ir-black-fm)


(let ((*background-color*   "#000000")
      (*brown*              "#E64")
      (*comments*           "#7C7C7C")
      (*constant*           "#99CC99")
      (*current-line*       "#151515")
      (*cursor-block*       "#FFFFFFF")
      (*cursor-underscore*  "#888888")
      (*keywords*           "#96CBFE")
      (*light-purple*       "#FFCCFF")
      (*line-number-fg*     "#666666")
      (*line-number-bg*     "#121212")
      (*method-declaration* "#FFD2A7")
      (*mode-line-bg*       "#202020")
      (*mode-line-fg*       "#CCCCCC")
      (*normal*             "#F0F0f0")
      (*number*             "#FF73FD")
      (*operators*          "#FFFFB6")
      (*cyan*               "#96CBFE")
      (*red*                "#FF6C60")
      (*yellow*             "#99cc99")
      (*red-light*          "#FFB6B0")
      (*regexp*                "#E9C")
      (*regexp-alternate*      "#FF0")
      (*regexp-alternate-2* "#B18A3D")
      (*search-selection*   "#2F2F00")
      (*string*             "#A8FF60")
      (*string-inner*       "#00A0A0")
      (*variable*           "#C6C5FE")
      (*visual-selection*   "#FF6C60")
      (*vertical-split*     "#404040")
      (*minibuffer-prompt*  "#888888"))
  (custom-theme-set-faces
   'ir-black-fm

   `(bold ((t (:bold t))))
   `(button ((t (:foreground, *keywords*))))
   `(default ((t (:background, *background-color* :foreground, *normal*))))
   `(escape-glyph ((t (:foreground, *string-inner*))))
   '(border-glyph ((t (nil))))
   `(header-line ((t (:background, *mode-line-bg* :foreground, *normal*)))) ;; info header
   `(highlight ((t (:background, *current-line*))))
   `(highlight-face ((t (:background, *current-line*))))
   `(hl-line ((t (:background, "#141416"))))
   `(info-xref ((t (:foreground, *keywords*))))
   `(region ((t (:background "#334141" :foreground "#cccccc"))))
   `(underline ((nil (:underline t))))

   ;; font-lock
   `(font-lock-builtin-face ((t (:foreground, *operators*))))
   `(font-lock-comment-delimiter-face ((t (:foreground, *comments*))))
   `(font-lock-comment-face ((t (:foreground, *comments*))))
   `(font-lock-constant-face ((t (:foreground, *constant*))))
   `(font-lock-doc-face ((t (:foreground, *string*))))
   `(font-lock-doc-string-face ((t (:foreground, *string*))))
   `(font-lock-function-name-face ((t (:foreground, *method-declaration*))))
   `(font-lock-keyword-face ((t (:foreground, *keywords*))))
   `(font-lock-negation-char-face ((t (:foreground, *red*))))
   `(font-lock-number-face ((t (:foreground, *number*))))
   `(font-lock-preprocessor-face ((t (:foreground, *keywords*))))
   `(font-lock-reference-face ((t (:foreground, *constant*))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground, *regexp*))))
   `(font-lock-regexp-grouping-construct ((t (:foreground, *regexp*))))
   `(font-lock-string-face ((t (:foreground, *string*))))
   `(font-lock-type-face ((t (:foreground, *operators*))))
   `(font-lock-variable-name-face ((t (:foreground, *variable*))))
   `(font-lock-warning-face ((t (:foreground, *red*))))
   `(font-lock-operator-face ((t (:foreground, "#eb77ec"))))


   ;; GUI
   `(fringe ((t (:background, *background-color*))))
   `(linum ((t (:background, *line-number-bg* :foreground, *line-number-fg*))))
   `(minibuffer-prompt ((t (:foreground, *minibuffer-prompt*))))
   `(cursor ((t (:background, "#FD4B70"))))
   `(text-cursor ((t (:background, *cursor-underscore*))))
   `(vertical-border ((t (:foreground, *vertical-split*)))) ;; between splits

      ;; which-key
   '(which-key-key-face ((t (:foreground "#ffa500"))))
   '(which-key-separator-face ((t (:foreground "#4d4d4d"))))

   ;; indentation
   `(highlight-indentation-face ((t (:background, "#181818"))))

   ;; ido
   `(ido-subdir ((t (:foreground, "#CF6A4C"))))
   `(ido-first-match ((t (:foreground, "#A8FF60"))))
   `(ido-only-match ((t (:foreground, "#A8FF60"))))

   ;; mumamo
   `(mumamo-background-chunk-submode ((t (:background, "#222222"))))

   ;; show-paren
   `(show-paren-mismatch ((t (:background, *red* :foreground, *normal* :weight bold))))
   `(show-paren-match ((t (:background, *keywords* :foreground, *normal* :weight bold))))

   ;; search
   `(isearch ((t (:background, *regexp-alternate* :foreground, *search-selection*))))
   `(isearch-fail ((t (:background, *red*))))
   `(lazy-highlight ((t (:background, *operators* :foreground, *search-selection*))))

   ;; magit
   `(magit-log-sha1 ((t (:foreground "#cf6a4c"))))
   `(magit-log-head-label-local ((t (:foreground "#3387cc"))))
   `(magit-log-head-label-remote ((t (:foreground "#65b042"))))
   `(magit-branch ((t (:foreground "#e9c062"))))
   `(magit-section-title ((t (:foreground "#9b859d"))))
   `(magit-item-highlight ((t (:background "#1f1f1f"))))
   `(magit-diff-add ((t (:foreground, *string*))))
   `(magit-diff-del ((t (:foreground, *red*))))

   ;; ivy mode / counsel
   `(ivy-current-match ((t (:background "#003366"))))
   `(ivy-minibuffer-match-face-1 ((t ())))
   `(ivy-minibuffer-match-face-2 ((t (:foreground "#FF6C60"))))
   `(ivy-minibuffer-match-face-3 ((t (:foreground "#FF6C60"))))
   `(ivy-minibuffer-match-face-4 ((t (:foreground "#FF6C60"))))

   ;; helm
   `(helm-source-header ((t (:foreground "#FF6C60"))))
   `(helm-selection ((t (:background "#003366"))))
   `(helm-match ((t (:foreground "#FF6C60"))))
   `(helm-match-item ((t (:foreground "#FF6C60"))))
   `(helm-grep-match ((t (:foreground "#FF6C60"))))
   `(helm-git-grep-match ((t (:foreground "#FF6C60"))))
   `(helm-grep-hit ((t (:foreground "#FF6C60"))))

   ;; org-mode
   `(org-date ((t (:foreground, *light-purple*))))
   `(org-level-1 ((t (:foreground, *string*))))
   `(org-special-keyword ((t (:foreground, *variable*))))
   `(org-link ((t (:foreground, *keywords*))))
   `(org-checkbox ((t (:foreground, *keywords* :background, *background-color* :bold t))))
   `(org-clock-overlay ((t (:foreground, *mode-line-bg* :background, *string*))))

   ;; flycheck
   `(flycheck-error
     ((t (:underline (:style wave :color, *red*) :inherit unspecified))))
   `(flycheck-warning
     ((t (:underline (:style wave :color, *red*) :inherit unspecified))))
   `(flycheck-info
     ((t (:underline (:style wave :color, *red*) :inherit unspecified))))

   
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

   
   ;; frame UI
   `(mode-line ((t (:background "#202020" :foreground "#ffffff"))))
   `(mode-line-inactive ((t (:background "#151515" :foreground "#666666"))))
   `(mode-line-buffer-id ((t (:foreground "#63eb63"))))
   `(vertical-border ((nil (:foreground "#060606"))))
   `(fringe ((nil (:background "#151515"))))

   
   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground "#55f1e1"))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground "#887FD5"))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground "#EB77EC"))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground "#FFBB44"))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground "#EFEF66"))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground "#63EB63"))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground "#55F1E1"))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground "#70BFFF"))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground "#887FD5"))))


   ;; LaTeX
   '(font-latex-sectioning-1-face ((t (:inherit org-level-1))))
   '(font-latex-sectioning-2-face ((t (:inherit org-level-2))))
   '(font-latex-sectioning-3-face ((t (:inherit org-level-3))))
   `(font-latex-string-face ((t (:inherit *string*))))
   '(font-latex-bold-face ((t (:inherit bold))))

   
   ;; starter kit
   `(esk-paren-face ((t (:foreground, *string-inner*))))
   ))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;; Local Variables:
;; no-byte-compile: t
;; End:

(provide-theme 'ir-black-fm)
;;; ir-black-fm-theme.el ends here
