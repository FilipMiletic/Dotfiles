;;; package -- Summary:
;;  exec-path-from-shell
;;  flycheck
;;  magit
;;  ido-ubiquitous
;;  flx-ido
;;  ido
;;  ido-vertical
;;  clojure-mode
;;  paredit
;;  cider
;;  company
;;  tramp

;;; Commentary:
;; I will call him Hal-3000!

;;; Code:
;; Setup package system
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

;; Load paths
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(add-to-list 'load-path "~/.emacs.d/themes/doom-theme/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Modes
(menu-bar-mode t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode t)
(show-paren-mode 1)
(global-auto-revert-mode 1)
(global-prettify-symbols-mode 1)
(global-hl-line-mode 1)
(blink-cursor-mode 0)


;; Global configuration
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
      fringes-outside-margins t
      ns-pop-up-frames nil
      make-backup-files nil
      auto-save-default nil
      create-lockfiles nil
      sentence-end-double-space nil
      cursor-type 'box)

;; Indentation
(setq-default indent-tabs-mode nil
              tab-width 2
              c-basic-offset 4
              fill-column 80)

(defalias 'yes-or-no-p 'y-or-n-p)

(defun display-startup-echo-area-message ()
  "Just say hello Hal!"
  (message "Hello, Dave. You're looking well today."))

;; Don't close Emacs via GUI command
(defadvice handle-delete-frame (around my-handle-delete-frame-advice activate)
  "Hide Emacs instead of closing the last frame."
  (let ((frame (posn-window (event-start event)))
		(numfrs (length(frame-list))))
	(if (> numfrs 1)
		ad-do-it
	  (do-applescript "tell application \"System Events\" to tell process \"Emacs\" to set visible to false"))))

;; Theme
(setq custom-safe-themes t)
;;(load-theme 'monotropic t)
(setq darkokai-mode-line-padding 1)
(load-theme 'eby t)

;;(load-theme 'monotropic t)

(set-face-attribute 'default nil
                    :font "SF Mono"
                    :height 110
                    :weight 'regular)

(global-set-key [remap goto-line] 'goto-line-with-feedback)
(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for line number."
  (interactive)
  (with-no-warnings
    (unwind-protect
        (progn
          (linum-mode t)
          (goto-line (read-number "Goto line: ")))
      (linum-mode -1))))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package server
  :config
  (unless (server-running-p) (server-start)))

(use-package flycheck
  :ensure t
  :defer 2
  :config (global-flycheck-mode 1))

(defvar magit-auto-revert-buffers)
(defvar magit-push-always-verify)
(use-package magit
  :ensure t
  :commands (magit-status magit-checkout)
  :bind (("C-c g" . magit-status))
  :init
  (setq magit-auto-revert-buffers 'silent
        magit-push-always-verify nil
        git-commit-summary-max-length 70))

(use-package ido-ubiquitous
  :ensure t
  :config (ido-ubiquitous-mode t))

(use-package flx-ido
  :ensure t
  :config (setq ido-use-faces nil))

(use-package ido
  :ensure t
  :config
  (progn
    (ido-mode t)
    (ido-everywhere t)
    (flx-ido-mode t)
    (setq ido-enable-flex-matching t)))

(use-package company
  :ensure t
  :defer t
  :init (global-company-mode)
  :config
  (progn
    ;; Use Company for completion
    (bind-key [remap completion-at-point] #'company-complete company-mode-map)
    (setq company-tooltip-align-annotations t
          ;; Easy navigation to candidates with M-<n>
          company-show-numbers t))
  :diminish company-mode)

(use-package ido-vertical-mode
  :ensure t
  :config (ido-vertical-mode 1))

(use-package clojure-mode
  :ensure t
  :config (setq clojure-indent-style t))

(use-package smartparens
  :ensure t
  :init
  (add-hook 'clojure-mode-hook 'smartparens-mode)
  (add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
  (add-hook 'cider-repl-mode-hook 'smartparens-mode)
  (add-hook 'lisp-mode-hook 'smartparens-mode))

;; Irony
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(use-package cider
  :ensure t
  :init
  (add-hook 'cider-repl-mode-hook 'smartparens-strict-mode))

(require 'tramp)
(setq tramp-default-method "ssh")
(smartparens-global-mode t)


(provide 'init)
;;; init.el ends here
