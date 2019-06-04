;;; package --- Summary:
;;; Commentary:
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

;; *****************************************************************************
;;
;; naysayer :- Description
;;
;; *****************************************************************************
;;; Code:
(unless (>= emacs-major-version 24)
  (error "Requires Emacs 24 or later"))

(deftheme naysayer "Description")

(let ((background "#082628")
      (gutters    "#082628")
      (gutter-fg  "#082628")
      (gutters-active "#082628")
      (builtin      "#ffffff")
      (selection  "#0000ff")
      (text       "#d2b58d")
      (comments   "#67cd5d")
      (punctuation "#86E08F")
      (keywords "#ffffff")
      (variables "#cfdfff")
      (functions "#f0f0f0")
      (methods    "#d4d4d4")
      (strings    "#2ed0bf")
      (constants "#8fe1c8")
      (macros "#86E08F")
      (white     "#ffffff")
      (error "#ff0000")
      (warning "#ffaa00")
      (highlight-line "#0b3335")
      (line-fg "#126367"))

  (custom-theme-set-faces
   'naysayer

   ;; Default colors
   ;; *****************************************************************************

   `(default                          ((t (:foreground ,text :background ,background, :weight normal))))
   `(region                           ((t (:foreground nil :background ,selection))))
   `(cursor                           ((t (:background "brown1"                        ))))
   `(fringe                           ((t (:background ,background   :foreground ,white))))

   `(mode-line                        ((t (:foreground ,background :background "wheat"  ))))
   `(mode-line-inactive               ((t (:foreground "black" :background "grey"  ))))
   
   `(highlight ((t (:foreground nil :background ,selection))))

   ;; Font lock faces
   ;; *****************************************************************************

   `(font-lock-keyword-face           ((t (:foreground ,keywords, :weight normal))))
   `(font-lock-type-face              ((t (:foreground ,punctuation))))
   `(font-lock-constant-face          ((t (:foreground ,constants))))
   `(font-lock-variable-name-face     ((t (:foreground ,variables))))
   `(font-lock-builtin-face           ((t (:foreground ,builtin))))
   `(font-lock-string-face            ((t (:foreground ,strings))))
   `(font-lock-comment-face           ((t (:foreground "green3"))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,comments))))
   `(font-lock-function-name-face     ((t (:foreground ,functions))))
   `(font-lock-doc-string-face        ((t (:foreground ,strings))))
   `(font-lock-preprocessor-face      ((t (:foreground ,macros))))

   ;; Plugins
   ;; *****************************************************************************
   `(trailing-whitespace ((t (:foreground nil :background ,warning))))
   `(whitespace-trailing ((t (:background nil :foreground ,warning :inverse-video t))))

   `(linum ((t (:foreground ,line-fg :background ,background))))
   `(linum-relative-current-face ((t (:foreground ,line-fg :background ,background))))
   `(line-number ((t (:foreground ,line-fg :background ,background))))
   `(line-number-current-line ((t (:foreground ,white :background ,background))))

   ;; hl-line-mode
   `(hl-line ((t (:background ,highlight-line))))
   `(hl-line-face ((t (:background ,highlight-line))))

   ;; rainbow-delimiters

   ;; mode-line and powerline
   `(mode-line-buffer-id ((t (:foreground ,background :weight bold))))
   )
  )

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;; *****************************************************************************
;; Local Variables:
;; no-byte-compile: t
;; End:

(provide-theme 'naysayer)
;;; naysayer-theme.el ends here
