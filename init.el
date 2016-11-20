(require 'package)
(setq package-user-dir "~/.emacs.d/elpa/")
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(defvar mf-packages
  '(
    exec-path-from-shell
    flycheck
    company
    clojure-mode
    smartparens
    browse-kill-ring
    ido-ubiquitous
    ido-vertical-mode
    flx-ido
    noctilux-theme
    magit
    tramp
    undo-tree
    ))

(defun mf-install-packages ()
  "Install only the sweetest of packages."
  (interactive)
  (package-refresh-contents)
  (mapc #'(lambda (package)
            (unless (package-installed-p package)
              (package-install package)))
        mf-packages))

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
      cursor-type 'box
      ido-use-faces nil
      ido-enable-flex-matching 1
      tramp-default-method "ssh")

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
(load-theme 'noctilux t)


(ido-ubiquitous-mode 1)
(ido-mode 1)
(ido-everywhere 1)
(global-company-mode 1)
(smartparens-global-mode 1)

(add-hook 'emacs-lisp-mode-hook 'smartparens-mode)


