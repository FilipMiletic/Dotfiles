;;; dark-laptop-theme.el --- A port of the original dark-laptop theme
;;; by Laurent Michel to an Emacs 24 theme.

;; Copyright (C) 2011 Lane Liles

;; Author: Lane Liles <lane.liles@gmail.com>
;; Created: 2011-08-20
;; URL: http://github.com/lliles/dark-laptop
;; Version: 1.0

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

;;; Installation:
;;
;;   Copy this file to a folder in your Emacs config directory such as
;;  "~/.emacs.d/themes" and add it to the theme load path with:
;;
;;     (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
;;
;;   Then load the theme within Emacs with:
;;
;;     M-x load-theme dark-laptop

(deftheme dark-laptop
  "The dark-laptop theme.")

(let
    ( (dl-aquamarine           "#7fffd4")
      (dl-aquamarine-1         "#7fffd4")
      (dl-aquamarine-2         "#76eec6")       
       (dl-aquamarine-3         "#66cdaa")       
       (dl-aquamarine-4         "#458b74")       
       (dl-black                "#060606")
       (dl-blue                 "#0000ff")
       (dl-blue-3               "#0000cd")
       (dl-chocolate            "#d2691e")
       (dl-cyan                 "#00ffff")
       (dl-dark-gray            "#262626")
       (dl-dark-olive-green     "#556b2f")
       (dl-dark-slate-blue      "#483d8b")
       (dl-dark-turquoise       "#00ced1")       
       (dl-deep-sky-blue        "#00bfff")
       (dl-dim-gray             "#696969")
       (dl-forest-green         "#228b22")
       (dl-gold                 "#ffd700")
       (dl-green-3              "#00cd00")
       (dl-green-4              "#008b00")
       (dl-khaki                "#f0e68c")
       (dl-light-blue           "#add8e6")
       (dl-light-goldenrod      "#eedd82")
       (dl-light-gray           "#d3d3d3")
       (dl-light-pink           "#ffb6c1")
       (dl-light-salmon         "#ffa07a")
       (dl-light-sky-blue       "#87cefa")
       (dl-light-steel-blue     "#b0c4de")
       (dl-lime-green           "#32cd32")
       (dl-magenta              "#ff00ff")
       (dl-medium-purple        "#9370db")
       (dl-orange               "#ffa500")
       (dl-orange-red           "#ff4500")
       (dl-pale-green           "#cbe7d4")
       (dl-pale-turquoise       "#afeeee")       
       (dl-pink                 "#ffc0cb")
       (dl-purple               "#a020f0")
       (dl-red                  "#ff0000")
       (dl-sienna-1             "#ff8247")
       (dl-sky-blue             "#87ceeb")
       (dl-spring-green         "#00ff7f")
       (dl-turquoise            "#40e0d0")
       (dl-violet               "#ee82ee")
       (dl-white                "#ffffff")
       (dl-yellow               "#ffff00"))

  (custom-theme-set-faces
   'dark-laptop
  
    ;; basics
    '(bold ((t (:weight bold))))
    '(bold-italic ((t (:slant italic :weight bold))))
    `(default ((t (:foreground ,dl-white :background ,dl-black))))
    '(italic ((t (:slant italic))))
    '(underline ((t (:underline t))))
    `(mode-line ((t (:foreground "#dddddd" :background "#333333"))))
    `(mode-line-inactive ((t (:foreground "#666666" :background "#333333"))))
    `(mode-line-buffer-id ((t (:foreground "#12e64c" :background "#333333"))))
    `(highlight ((t (:background ,dl-dark-olive-green))))
    `(mouse ((t (:background ,dl-sienna-1))))
    `(border ((t (:background ,dl-black))))
    `(cursor ((t (:background "#FD4B70"))))
    `(region ((t (:background ,dl-blue))))
    `(primary-selection ((t (:background ,dl-blue))))
    `(secondary-selection ((t (:background ,dl-dark-slate-blue))))

    ;; customize
    `(custom-button ((t (nil))))
    `(custom-changed ((t (:foreground ,dl-white :background ,dl-blue))))
    `(custom-documentation ((t (nil))))
    `(custom-face-tag ((t (:underline t))))
    `(custom-group-tag ((t (:underline t :foreground ,dl-light-blue))))
    `(custom-group-tag-1 ((t (:underline t :foreground ,dl-pink))))
    `(custom-invalid ((t (:foreground ,dl-yellow :background ,dl-red))))
    `(custom-modified ((t (:foreground ,dl-white :background ,dl-blue))))
    `(custom-rogue ((t (:foreground ,dl-pink :background ,dl-black))))
    '(custom-saved ((t (:underline t))))
    `(custom-set ((t (:foreground ,dl-blue :background ,dl-white))))
    `(custom-state ((t (:foreground ,dl-lime-green))))
    '(custom-variable-button ((t (:weight bold :underline t))))
    `(custom-variable-tag ((t (:underline t :foreground ,dl-light-blue))))
    
    ;; font-locking
    `(font-lock-builtin-face ((t (:foreground ,dl-light-steel-blue))))
    `(font-lock-comment-face ((t (:foreground "#12e64c"))))
    `(font-lock-constant-face ((t (:foreground ,dl-aquamarine))))
    `(font-lock-doc-string-face ((t (:foreground ,dl-light-salmon))))
    `(font-lock-function-name-face ((t (:foreground ,dl-light-sky-blue))))
    `(font-lock-keyword-face ((t (:foreground ,dl-cyan))))
    `(font-lock-preprocessor-face ((t (:foreground ,dl-aquamarine))))
    `(font-lock-string-face ((t (:foreground ,dl-light-salmon))))
    `(font-lock-type-face ((t (:foreground ,dl-pale-green))))
    `(font-lock-variable-name-face ((t (:foreground ,dl-light-goldenrod))))
    `(font-lock-warning-face ((t (:weight bold :foreground ,dl-pink))))

    ;; gnus
    `(gnus-cite-face-1 ((t (:bold t :foreground ,dl-deep-sky-blue))))
    `(gnus-cite-face-10 ((t (:foreground ,dl-medium-purple))))
    `(gnus-cite-face-11 ((t (:foreground ,dl-turquoise))))
    `(gnus-cite-face-2 ((t (:bold t :foreground ,dl-cyan))))
    `(gnus-cite-face-3 ((t (:bold t :foreground ,dl-gold))))
    `(gnus-cite-face-4 ((t (:foreground ,dl-light-pink))))
    `(gnus-cite-face-5 ((t (:foreground ,dl-pale-green))))
    `(gnus-cite-face-6 ((t (:bold t :foreground ,dl-chocolate))))
    `(gnus-cite-face-7 ((t (:foreground ,dl-orange))))
    `(gnus-cite-face-8 ((t (:foreground ,dl-magenta))))
    `(gnus-cite-face-9 ((t (:foreground ,dl-violet))))
    `(gnus-emphasis-bold ((t (:bold t))))
    `(gnus-emphasis-bold-italic ((t (:italic t :bold t))))
    `(gnus-emphasis-highlight-words ((t (:background ,dl-black :foreground ,dl-yellow))))
    `(gnus-emphasis-italic ((t (:italic t))))
    `(gnus-emphasis-underline ((t (:underline t))))
    `(gnus-emphasis-underline-bold ((t (:underline t :bold t))))
    `(gnus-emphasis-underline-bold-italic ((t (:underline t :italic t :bold t))))
    `(gnus-emphasis-underline-italic ((t (:underline t :italic t))))
    `(gnus-group-mail-1-empty-face ((t (:foreground ,dl-aquamarine-1))))
    `(gnus-group-mail-1-face ((t (:bold t :foreground ,dl-aquamarine-1))))
    `(gnus-group-mail-2-empty-face ((t (:foreground ,dl-aquamarine-2))))
    `(gnus-group-mail-2-face ((t (:bold t :foreground ,dl-aquamarine-2))))
    `(gnus-group-mail-3-empty-face ((t (:foreground ,dl-aquamarine-3))))
    `(gnus-group-mail-3-face ((t (:bold t :foreground ,dl-aquamarine-3))))
    `(gnus-group-mail-low-empty-face ((t (:foreground ,dl-aquamarine-4))))
    `(gnus-group-mail-low-face ((t (:bold t :foreground ,dl-aquamarine-4))))
    `(gnus-group-news-1-empty-face ((t (:foreground ,dl-pale-turquoise))))
    `(gnus-group-news-1-face ((t (:bold t :foreground ,dl-pale-turquoise))))
    `(gnus-group-news-2-empty-face ((t (:foreground ,dl-turquoise))))
    `(gnus-group-news-2-face ((t (:bold t :foreground ,dl-turquoise))))
    `(gnus-group-news-3-empty-face ((t (nil))))
    `(gnus-group-news-3-face ((t (:bold t))))
    `(gnus-group-news-4-empty-face ((t (nil))))
    `(gnus-group-news-4-face ((t (:bold t))))
    `(gnus-group-news-5-empty-face ((t (nil))))
    `(gnus-group-news-5-face ((t (:bold t))))
    `(gnus-group-news-6-empty-face ((t (nil))))
    `(gnus-group-news-6-face ((t (:bold t))))
    `(gnus-group-news-low-empty-face ((t (:foreground ,dl-dark-turquoise))))
    `(gnus-group-news-low-face ((t (:bold t :foreground ,dl-dark-turquoise))))
    `(gnus-header-content-face ((t (:italic t :foreground ,dl-forest-green))))
    `(gnus-header-from-face ((t (:bold t :foreground ,dl-spring-green))))
    `(gnus-header-name-face ((t (:foreground ,dl-deep-sky-blue))))
    `(gnus-header-newsgroups-face ((t (:italic t :bold t :foreground ,dl-purple))))
    `(gnus-header-subject-face ((t (:bold t :foreground ,dl-orange))))
    `(gnus-signature-face ((t (:bold t :foreground ,dl-khaki))))
    `(gnus-summary-cancelled-face ((t (:background ,dl-black :foreground ,dl-yellow))))
    `(gnus-summary-high-ancient-face ((t (:bold t :foreground ,dl-sky-blue))))
    `(gnus-summary-high-read-face ((t (:bold t :foreground ,dl-pale-green))))
    `(gnus-summary-high-ticked-face ((t (:bold t :foreground ,dl-pink))))
    `(gnus-summary-high-unread-face ((t (:bold t))))
    `(gnus-summary-low-ancient-face ((t (:italic t :foreground ,dl-sky-blue))))
    `(gnus-summary-low-read-face ((t (:italic t :foreground ,dl-pale-green))))
    `(gnus-summary-low-ticked-face ((t (:italic t :foreground ,dl-pink))))
    `(gnus-summary-low-unread-face ((t (:italic t))))
    `(gnus-summary-normal-ancient-face ((t (:foreground ,dl-sky-blue))))
    `(gnus-summary-normal-read-face ((t (:foreground ,dl-pale-green))))
    `(gnus-summary-normal-ticked-face ((t (:foreground ,dl-pink))))
    `(gnus-summary-normal-unread-face ((t (nil))))
    `(gnus-summary-selected-face ((t (:underline t))))
     
     ;; highlight
    `(highlight-current-line ((t (:background ,dl-dark-olive-green))))

    ;; isearch
    `(isearch ((t (:background ,dl-blue))))

    ;; message
    `(message-cited-text-face ((t (:bold t :foreground ,dl-red))))
    `(message-header-cc-face ((t (:bold t :foreground ,dl-green-4))))
    `(message-header-name-face ((t (:bold t :foreground ,dl-orange))))
    `(message-header-newsgroups-face ((t (:bold t :foreground ,dl-violet))))
    `(message-header-other-face ((t (:bold t :foreground ,dl-chocolate))))
    `(message-header-subject-face ((t (:bold t :foreground ,dl-yellow))))
    `(message-header-to-face ((t (:bold t :foreground ,dl-cyan))))
    `(message-header-xheader-face ((t (:bold t :foreground ,dl-light-blue))))
    `(message-mml-face ((t (:bold t :background ,dl-green-3))))
    `(message-separator-face ((t (:foreground ,dl-blue-3))))

    ;; widget
    `(widget-button ((t (:bold t))))
    `(widget-button-pressed ((t (:foreground ,dl-red))))
    `(widget-documentation ((t (:foreground ,dl-lime-green))))
    `(widget-field ((t (:background ,dl-dim-gray))))
    `(widget-inactive ((t (:foreground ,dl-light-gray))))
    `(widget-single-line-field ((t (:background ,dl-dim-gray))))
    ))

(provide-theme 'dark-laptop)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; dark-laptop-theme.el ends here.
