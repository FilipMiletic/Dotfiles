;;; package -- Summary:
;;  flycheck
;;  ido-ubiquitous
;;  flx-ido
;;  ido
;;  ido-vertical
;;  clojure-mode
;;  paredit
;;  company
;;  tramp
;;  evil
;;  evil-leader
;;; Commentary:
;;; Code:
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))

(package-initialize)
(elpy-enable)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(unless package-archive-contents
  (package-refresh-contents))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(menu-bar-mode t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode t)
(show-paren-mode 1)
(global-auto-revert-mode 1)
(global-prettify-symbols-mode 1)
(global-hl-line-mode 1)
(blink-cursor-mode 0)

(setq mac-option-modifier nil
      mac-command-modifier 'meta
      load-prefer-newer t
      gc-cons-threshold 100000000
      ring-bell-function 'ignore
      inhibit-splash-screen t
      initial-scratch-message nil
      inhibit-startup-message t
      require-final-newline t
      mouse-wheel-scroll-amount '(1 ((shift) .1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse 't
      scroll-step 1
      scroll-conservatively 10000
      scroll-margin 3
      fringes-outside-margins 1
      ns-pop-up-frames nil
      make-backup-files nil
      auto-save-default nil
      create-lockfiles nil
      sentence-end-double-space nil
      cursor-type 'box)

(setq-default indent-tabs-mode nil
              tab-width 4
              c-basic-offset 4
              fill-column 80)

(defalias 'yes-or-no-p 'y-or-n-p)

;; Don't close Emacs via GUI command
(defadvice handle-delete-frame (around my-handle-delete-frame-advice activate)
  "Hide Emacs instead of closing the last frame."
  (let ((frame (posn-window (event-start event)))
		  (numfrs (length(frame-list))))
	(if (> numfrs 1)
		  ad-do-it
	  (do-applescript "tell application \"System Events\" to tell process \"Emacs\" to set visible to false"))))

(set-frame-parameter (selected-frame) 'alpha '(98 . 100))
(add-to-list 'default-frame-alist '(alpha . (98 . 100)))

;; (defun toggle-transparency ()
;;    (interactive)
;;    (let ((alpha (frame-parameter nil 'alpha)))
;;      (set-frame-parameter
;;       nil 'alpha
;;       (if (eql (cond ((numberp alpha) alpha)
;;                      ((numberp (cdr alpha)) (cdr alpha))
;;                      ;; Also handle undocumented (<active> <inactive>) form.
;;                      ((numberp (cadr alpha)) (cadr alpha)))
;;                100)
;;           '(92 . 100) '(100 . 100)))))
;; (global-set-key (kbd "C-c t") 'toggle-transparency)

(setq custom-safe-themes t)
(use-package noctilux-theme
   :ensure t
   :config (load-theme 'noctilux t))

(use-package flycheck
  :ensure t
  :defer 2
  :config (global-flycheck-mode 1))

(use-package ido-ubiquitous
  :ensure t
  :config (ido-ubiquitous-mode t))

(use-package flx-ido
  :ensure t
  :config (setq ido-use-faces nil))

(use-package paredit
  :ensure t
  :diminish paredit-mode
  :config
  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           #'enable-paredit-mode)
  :bind (("C-c d" . paredit-forward-down))
  )

;; Ensure paredit is used EVERYWHERE!
(use-package paredit-everywhere
  :ensure t
  :diminish paredit-everywhere-mode
  :config
  (add-hook 'prog-mode-hook #'paredit-everywhere-mode))

(use-package ido
  :ensure t
  :config
  (progn
    (ido-mode t)
;;  (ido-everywhere t)
    (flx-ido-mode t)
    (setq ido-enable-flex-matching t)))

(use-package ido-vertical-mode
  :ensure t
  :config (ido-vertical-mode 1))

(use-package company
  :ensure t
  :diminish company-mode
  :config
  (setq company-auto-complete nil
        company-tooltip-flip-when-above t
        company-minimum-prefix-length 2
        company-tooltip-limit 10)
  (global-company-mode 1)
  (eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony)))

(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

(defvar irony-mode-map)
;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  "Needed hack for irony mode map key."
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(require 'tramp)
(setq tramp-default-method "ssh")

(use-package evil
  :init
  (progn
    (setq evil-default-cursor t)
    (use-package evil-leader
      :init (global-evil-leader-mode t)
      :config
      (progn
        (setq evil-leader/leader ",")
        (setq evil-leader/in-all-states t)
        (evil-leader/set-key
         "b" 'ido-switch-buffer
         "f" 'ido-find-file
         ;;"g" 'magit-status
         "k" 'kill-buffer
         "K" 'kill-this-buffer
         "o" 'occur
         "t" 'eshell
         "w" 'save-buffer)))
    (evil-mode 1))
  :config
  (progn
    ;; use ido to open files
    (define-key evil-ex-map "e " 'ido-find-file)
    (define-key evil-ex-map "b " 'ido-switch-buffer)))

(provide 'init)
;;; init.el ends here
