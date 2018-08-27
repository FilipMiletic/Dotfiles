;;; Package --- Summary
;;; Commentary:
(require 'package)
;;; Code:
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic settings
(defalias 'display-startup-echo-area-message #'ignore)
(defalias 'yes-or-no-p 'y-or-n-p)

;; MacOS dock behaviour
(defadvice handle-delete-frame (around my-handle-delete-frame-advice activate)
  "Hide Emacs instead of closing the last frame."
  (let ((frame (posn-window (event-start event)))
		(numfrs (length(frame-list))))
	(if (> numfrs 1)
		ad-do-it
	  (do-applescript "tell application \"System Events\" to tell /
                       process \"Emacs\" to set visible to false"))))

(setq-default indent-tabs-mode t
              indent-line-function 4
              tab-width 4
              c-basic-offset 4
              fill-column 80
			  cursor-type 'box
              cursor-in-non-selected-windows 'hollow
			  word-wrap t
			  require-final-newline t)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/custom")

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
(pixel-scroll-mode  1)
(menu-bar-mode      1)
(scroll-bar-mode   -1)
(show-paren-mode    1)
(line-number-mode   1)
(column-number-mode 1)
(blink-cursor-mode  0)
(transient-mark-mode 1)
(global-hl-line-mode 0)

(setq mac-option-modifier nil
	  mac-command-modifier 'meta
	  load-prefer-newer t
	  ring-bell-function 'ignore
	  inhibit-splash-screen t
	  initial-scratch-message nil
	  inhibit-startup-message t
	  mouse-wheel-scroll-amount '(1 ((shift) .1))
	  mouse-wheel-progressive-speed nil
	  scroll-step 1
	  scroll-conservatively 100000
	  scroll-margin 8
	  fringes-outside-margins 1
	  ns-pop-up-frames nil
	  make-backup-files nil
	  auto-save-default nil
	  create-lockfiles nil
	  frame-resize-pixelwise t
	  make-backup-files nil
	  eshell-cmpl-ignore-case t
	  create-lockfiles nil
	  frame-title-format '("%b")
	  gc-cons-threshold 100000000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom functions
(defun open-previous-line (arg)
  "Open a new line above current one ARG."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (when newline-and-indent
	(indent-according-to-mode)))

(global-set-key (kbd "M-o") 'open-previous-line)
(global-set-key (kbd "C-x C-u") 'undo-tree-mode)
(defvar newline-and-indent t)

(defun indent-buffer ()
  "Indent current buffer according to major mode."
  (interactive)
  (indent-region (point-min) (point-max)))

(setq initial-frame-alist
      '((width . 135)
        (height . 65)))
(setq org-hide-emphasis-markers t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Built in packages
(use-package eshell
  :defer t
  :config
  (add-hook 'eshell-mode-hook
			(lambda ()
			  (load "~/.emacs.d/custom/eshell-customizations.el"))))

(use-package org
  :mode (("\\.org\\'" . org-mode))
  :defer t
  :config
  (setq org-agenda-files (quote ("~/Documents/Notes/plan.org"))
		org-startup-indented t
		org-todo-keywords
		'(("TODO" . (:foreground red :weight bold))
		  ("WAIT" . (:foreground cyan :weight bold))
		  ("DONE" . (:foreground green :weight normal))))
  (setq-default org-catch-invisible-edits 'smart))

(add-hook 'org-mode-hook #'(lambda ()
							 (visual-line-mode)))

(add-to-list 'default-frame-alist '(ns-appearance . dark))
(set-face-attribute 'default nil :family "Iosevka" :height 120 :weight 'regular)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Packages
(use-package doom-themes
  :ensure t
  :config
  (setq doom-neotree-enable-file-icons t
		doom-themes-padded-modeline t)
  (load-theme 'doom-watch t)
  (setq doom-themes-enable-bold t
		doom-themes-padded-modeline t
		doom-neotree-file-icons 'simple)
  (doom-themes-neotree-config)
  (doom-themes-org-config))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package paredit
  :ensure t
  :diminish paredit-mode
  :config
  (dolist (m '(clojure-mode-hook
			   cider-repl-mode-hook
			   emacs-lisp-mode-hook
			   racket-mode-hook
			   racket-repl-mode-hook
			   scheme-mode-hook
			   eval-expression-minibuffer-setup-hook))
	(add-hook m #'paredit-mode)))

(use-package highlight-numbers
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))

(use-package rainbow-delimiters
  :ensure t
  :config
  (dolist (m '(clojure-mode-hook
			   cider-repl-mode-hook
			   emacs-lisp-mode-hook
			   racket-mode-hook
			   racket-repl-mode-hook
			   scheme-mode-hook
			   lisp-mode-hook
			   eval-expression-minibuffer-setup-hook))
	(add-hook m #'rainbow-delimiters-mode)))

(use-package exec-path-from-shell
  :ensure t
  :init (exec-path-from-shell-initialize)
  :config
  (setq exec-path-from-shell-check-startup-files nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enhancers
(use-package which-key
  :defer 0.2
  :diminish
  :config (which-key-mode))

(use-package company
  :ensure t
  :diminish company-mode
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-auto-complete nil
        company-idle-delay .1
        company-echo-delay 0
        company-tooltip-flip-when-above t
        company-minimum-prefix-length 2
        company-tooltip-limit 12))

(use-package ivy
  :ensure t
  :init (add-hook 'after-init-hook #'ivy-mode)
  :config
  (setq ivy-height 10
		ivy-wrap t
		ivy-fixed-height-minibuffer t
		ivy-use-virtual-buffers t
		enable-recursive-minibuffers t
		ivy-format-function 'ivy-format-function-arrow
        ivy-re-builders-alist '((counsel-M-x . ivy--regex-fuzzy)
								(t . ivy--regex-plus)))
  (define-key ivy-minibuffer-map (kbd "C-j") #'ivy-immediate-done)
  (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done))

(use-package ivy-rich
  :after ivy
  :init (ivy-rich-mode 1)
  :config (setq ivy-virtual-abbreviate 'full
				ivy-rich-path-style 'abbrev))

(use-package swiper
  :ensure t
  :bind (("C-s"     . swiper)
         ("C-r"     . swiper)
         ("C-c u"   . swiper-all)
         ("C-c C-r" . ivy-resume)
         ("C-c C-o" . ivy-occur))
  :config (setq swiper-include-line-number-in-search t))

(use-package smex
  :ensure t
  :config (smex-initialize))

(use-package counsel
  :ensure t
  :bind (("M-x"     . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-h f"   . counsel-describe-function)
         ("C-h v"   . counsel-describe-variable)
         ("C-c C-s" . counsel-rg)
         ("M-y"     . counsel-yank-pop)))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IRC, RSS, git, navigation and snippets
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
		 ("C-c g" . magit-file-log)))

(use-package neotree
  :ensure t
  :bind (("C-c f t" . neotree-toggle))
  :config
  (setq neo-window-width 40
		neo-theme 'icons
		neo-create-file-auto-open t
		neo-banner-message nil
		neo-show-updir-line t
		neo-dont-be-alone t
		neo-persist-show nil
		neo-show-hidden-files t))

(use-package projectile
  :ensure t
  :config
  (setq projectile-completion-system 'ivy
		projectile-enable-caching t
		projectile-find-dir-includes-top-level t))

;; Install Yasnippet-snippets package too
(use-package yasnippet
  :disabled t
  :ensure t
  :diminish yas-minor-mode
  :bind (("C-c C-c" . yas-insert-snippet))
  :config (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode))

(use-package ace-window
  :ensure t
  :commands ace-window
  :init (bind-key "C-x o" 'ace-window)
  :config (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; RSS reader
(use-package elfeed
  :ensure t
  :defer t
  :bind ("C-x w" . elfeed)
  :init (setf url-queue-timeout 30)
  :config
  (push "-k" elfeed-curl-extra-arguments)
  (setq-default elfeed-search-filter "@1-week-ago +unread")
  (setq elfeed-feeds
		'(("https://nullprogram.com/feed/" systems emacs)
		  ("https://utcc.utoronto.ca/~cks/space/blog/?atom" unix)
		  ("https://eli.thegreenplace.net/feeds/all.atom.xml")
		  ("https://joelonsoftware.com/feed/")
		  ("http://bit-player.org/feed")
		  ("http://feeds.feedburner.com/HighScalability")
		  ("https://blog.codinghorror.com/rss/")
		  ("https://martinfowler.com/feed.atom" agile)
		  ("https://www.tedunangst.com/flak/rss")
		  ("https://muratbuffalo.blogspot.com/feeds/posts/default" distributed)
		  ("http://blog.cognitect.com/blog?format=rss" clojure)
		  ("http://www.righto.com/feeds/posts/default" hardware)
		  ("http://lambda-the-ultimate.org/rss.xml" functional)
		  ("http://willcrichton.net/notes/"))))
(add-hook 'elfeed-show-mode-hook
		  (lambda () (set-face-attribute 'variable-pitch (selected-frame) :font
										 (font-spec :family "Menlo" :size 12))))

;; IRC setup
(setq my-credentials-file "~/.emacs.d/.private.el")
(defun my-nickserv-password (server)
  "Here I specify file which has to store credentials for SERVER."
  (with-temp-buffer
	(insert-file-contents-literally my-credentials-file)
	(plist-get (read (buffer-string)) :nickserv-password)))

(use-package circe
  :defer t
  :config
  (progn
	(setq circe-network-options
		  '(("Freenode"
			 :nick "phlm"
			 :nickserv-password my-nickserv-password
			 :channels ("#clojure" "#haskell" "#illumos" :after-auth "#emacs" "#freebsd" "#unleashed"))
			("OFTC"
			 :nick "phlm"
			 :channels ("#kernelnewbies"))
			("Mozilla"
			 :host "irc.mozilla.org"
			 :port (6697)
			 :nick "phlm"
			 :nickserv-password my-nickserv-password
			 :channels ("#rust" "#rust-beginners"))))
	(enable-circe-color-nicks)
	(setq circe-reduce-lurker-spam t)
	(setq circe-format-server-topic "*** Topic change by {userhost}: {topic-diff}")
	(setq circe-format-say "{nick:-12s} {body}")
	(setq lui-time-stamp-position 'right-margin
          lui-fill-type nil)
	(add-hook 'lui-mode-hook 'my-lui-setup)
	(defun my-lui-setup ()
	  (setq
	   fringes-outside-margins t
	   right-margin-width 8
	   word-wrap t
	   wrap-prefix "     "))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Languages
;; C/C++
(use-package ggtags
  :ensure t
  :config
  (add-hook 'c-mode-common-hook
			(lambda ()
			  (when (derived-mode-p 'c-mode 'c++-mode)
				(ggtags-mode 1)))))

(use-package counsel-gtags
  :defer t)

(use-package counsel-projectile
  :defer t)

(use-package xcscope
  :init (cscope-setup))


;; Racket/Scheme
(use-package geiser
  :ensure t
  :defer
  :bind (:map scheme-mode-map ("C-c C-c" . geiser-eval-last-sexp))
  :config
  (progn
	(setq geiser-active-implementations '(racket))
	(setq geiser-default-implementation 'racket)))

;; Clojure
(use-package cider
  :ensure t
  :defer t
  :commands (cider cider-connect cider-jack-in)
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode))
  :config
  (setq cider-auto-select-error-buffer t
		cider-repl-pop-to-buffer-on-connect nil
		cider-repl-use-clojure-font-lock t
		cider-repl-wrap-gistory t
		cider-repl-history-size 1000
		cider-repl-use-pretty-printing t
		cider-show-error-buffer t
		cider-inject-dependencies-at-jack-in t
		nrepl-hide-special-buffers t
		nrepl-popup-stacktraces nil)
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-mode-hook #'company-mode)
  (add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'company-mode))


;; Rust
(use-package rust-mode
  :defer t
  :config (add-hook 'rust-mode-hook
					(lambda ()
					  (local-set-key (kbd "C-c <tab>") #'rust-format-buffer))))

(use-package cargo
  :defer t
  :config (add-hook 'rust-mode-hook 'cargo-minor-mode))

(use-package racer
  :defer t
  :config
  (progn
	(setq racer-cmd "~/.cargo/bin/racer")
	(setq racer-rust-src-path "/Users/phil/Developer/other/rust/src")
	(add-hook 'rust-mode-hook #'racer-mode)
	(add-hook 'racer-mode-hook #'company-mode)))


;; Local Variables:
;; byte-compile-warnings: (not free-vars noruntime)
;; fill-column: 80
;; End:

(provide 'init)
;;; init ends here
