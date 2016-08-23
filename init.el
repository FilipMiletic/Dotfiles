;;; package --- Summary
;; exec-path-from-shell
;; flycheck
;; theme: apropospriate-theme; firebelly;
;; nlinum
;; sml
;; ido: flx + ubiquitus + vertical
;; company
;; agressive indent

;;; Commentary:
;; My personal Emacs configuration.

;;; Code:
;; Setup package system
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))

(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(unless package-archive-contents
  (package-refresh-contents))

;; Global Configuration
(setq mac-option-modifier nil)
(setq mac-command-modifier 'meta)
(setq load-prefer-newer t)
(setq gc-cons-threshold 25000000)
(setq-default tab-width 4)
(setq ring-bell-function 'ignore)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)
(setq-default truncate-lines t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq mac-allow-anti-aliasing t)
(line-number-mode 1)
(column-number-mode 1)
(setq require-final-newline t)
(prefer-coding-system 'utf-8)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq scroll-conservatively 10000)
(setq scroll-margin 3)
(show-paren-mode 1)
(setq ns-pop-up-frames nil)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)
(setq sentence-end-double-space nil)
(add-to-list 'load-path "~/.emacs.d/themes/emacs-doom-theme/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-doom-theme/")

(defadvice handle-delete-frame (around my-handle-delete-frame-advice activate)
  "Hide Emacs instead of closing the last frame."
  (let ((frame (posn-window (event-start event)))
		(numfrs (length(frame-list))))
	(if (> numfrs 1)
		ad-do-it
	  (do-applescript "tell application \"System Events\" to tell process \"Emacs\" to set visible to false"))))

(defvar c-default-style)
(defvar c-basic-offset)
(setq c-default-style "k&r"
	  c-basic-offset 4
	  indent-tabs-mode t)

(global-prettify-symbols-mode t)
(global-auto-revert-mode t)
(global-hl-line-mode t)

;; Exec path from shell
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package flycheck
  :ensure t
  :defer 2
  :config
  (global-flycheck-mode 1))

(setq custom-safe-themes t)
(load-theme 'doom-dark)
;;(use-package spacemacs-theme
;;:ensure t
;;:init (load-theme 'spacemacs-dark ))

(use-package nlinum
  :ensure t
  :config
  (global-nlinum-mode 1))

(use-package smart-mode-line
  :ensure t
  :config
  (progn
    (setq sml/no-confirm-load-theme t)
    (setq sml/theme 'respectful)
    (add-hook 'after-init-hook #'sml/setup)))

(use-package ido
  :ensure t
  :config
  (progn
    (ido-mode t)
    (ido-everywhere t)
    (flx-ido-mode t)
    (setq ido-enable-flex-matching t)
    (setq ido-use-faces nil)))

(use-package ido-ubiquitous
  :ensure t
  :config
  (ido-ubiquitous-mode t))

(use-package flx-ido
  :ensure t
  :config
  (flx-ido-mode t)
  (setq ido-use-faces nil))

(use-package ido-vertical-mode
  :ensure t
  :config
  (progn
    (ido-vertical-mode 1)
    (setq ido-vertical-define-keys #'C-n-and-C-p-only)))

(use-package company
  :ensure t
  :diminish company-mode
  :config
  (setq company-auto-complete nil
        company-tooltip-flip-when-above t
        company-minimum-prefix-length 2
        company-tooltip-limit 10)
  (global-company-mode 1))

(use-package aggressive-indent
  :ensure t
  :config
  (global-aggressive-indent-mode t))

(provide 'init)

;;; init.el ends here
