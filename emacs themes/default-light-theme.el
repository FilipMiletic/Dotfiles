;;; default-light-theme.el --- Minimalistic version of default light emacs theme
;;; by Filip Miletic to on Emacs 26 theme.
;;; Commentary:
;;; Code:
(deftheme default-light
  "Stripped out colors of default light emacs theme.")

(let (
      (comment-green "#3f7f5f")
      (string-red "#5f005f")
      (builtin-blue "#00007f")
      (variable-black "#000000"))
    (custom-theme-set-faces
     'default-light
     
     '(font-lock-comment-face       ((t (:foreground "#3F7F5F"))))
     '(font-lock-string-face        ((t (:foreground "#5F005F"))))
     '(font-lock-keyword-face       ((t (:foreground "#00007F"))))
     '(font-lock-builtin-face       ((t (:foreground "#00007F"))))
     '(font-lock-function-name-face ((t (:foreground "#000000"))))
     '(font-lock-variable-name-face ((t (:foreground "#000000"))))
     '(font-lock-type-face          ((t (:foreground "#000000"))))
     ))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'default-light)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; default-light-theme.el ends here
