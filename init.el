;;; package -- Summary:
;;  flycheck
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
;;; Code:
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

(setq custom-safe-themes t)
(use-package doom-themes
  :ensure t
  :config (load-theme 'doom-one t))


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
  (global-company-mode 1))

(require 'tramp)
(setq tramp-default-method "ssh")

(provide 'init)
;;; init.el ends here
