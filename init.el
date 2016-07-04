;;; Package -- Summary
;;; Commentary:
;; My personal Emacs setup.

(setq user-full-name "Filip Miletic"
	  user-mail-address "filip.miletic@me.com")

;;; Code:
;; Don't load outdated byte code
(setq load-prefer-newer t)

;; Bootstrap 'use-package'
(eval-when-compile
  (require 'package))
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
			 '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'diminish)
(require 'bind-key)

;; Customization
(defconst custom-file (locate-user-emacs-file "customize.el")
  "File used to store settings from Customization UI.")

(set-face-attribute 'mode-line nil :box nil)

(setq temporary-file-directory (expand-file-name "~/.emacs.d/tmp"))
(setq backup-directory-alist
	  `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
	  `((".*" ,temporary-file-directory t)))

(defun my-minibuffer-setup-hook ()
  "Defining setup hook for GC threshold."
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  "Defining exit hook for GC threshold."
  (defvar gc-cons-threshold)
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hoop #'my-minibuffer-exit-hook)

;;; UI

;; Get rid of toolbar, menubar, scrollbars.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (and (eq system-type 'darwin) (fboundp 'menu-bar-mode))
  (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(set-default 'truncate-lines t)

(setq-default tab-width 4)
(setq-default indent-tabs-mode t)

(delete-selection-mode 1)
(transient-mark-mode 1)
(blink-cursor-mode 1)
(defvar linum-format)
(setq ring-bell-function #'ignore
	  inhibit-startup-screen t
	  echo-keystrokes 0.1
	  linum-format " %d")
(setq initial-scratch-message "")
(fset 'yes-or-no-p #'y-or-n-p)
;; Opt out startup message in echo area

(global-linum-mode)

(set-face-attribute 'default nil
					:family "Menlo" :height 120)

;; utf-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; themes and look
(use-package darkokai-theme
  :ensure t
  :config
  (setq darkokai-mode-line-padding 1)
  (load-theme 'darkokai t))

;; System setup
(defvar delete-old-version)
(setq delete-old-version t)
(setq make-backup-files nil)
(setq create-lockfiles nil)

(defun copy-from-osx ()
  "Defining shell command to str."
  (defvar shell-command-to-string)
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  "Var process connectio type for paste-to-osx."
  (defvar process-connection-type)
  (let ((process-connection-type nil))
	(let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
	  (process-send-string proc text)
	  (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)
(fset 'display-startup-echo-area-message #'ignore)

;; Functin that can evaluate region or buffer
(defun eval-region-or-buffer ()
  "Evaluate the selection, or, if empty, the whole buffer."
  (interactive)
  (let ((debug-on-error))
	(cond
	 (mark-active
	  (call-interactively 'eval-region)
	  (setq deactivate-mark t))
	 (t
	  (eval-buffer)))))

;; Allow this emacs process to be a server for client processes.
(server-start)

(defadvice handle-delete-frame (around my-handle-delete-frame-advice activate)
  "Hide Emacs instead of closing the last frame."
  (let ((frame (posn-window (event-start event)))
		(numfrs (length(frame-list))))
	(if (> numfrs 1)
		ad-do-it
	  (do-applescript "tell application \"System Events\" to tell process \"Emacs\" to set visible to false"))))

(use-package powerline
  :ensure t
  :config
  (setq powerline-default-separator 'bar)
  :init (powerline-default-theme))

(use-package page-break-lines
  :ensure t
  :init (global-page-break-lines-mode)
  :diminish page-break-lines-mode)

(use-package cus-edit
  :defer t
  :config
  (setq custom-file custom-file
		custom-buffer-done-kill nil
		custom-buffer-verbose-help nil
		custom-unlispify-tag-names nil
		custom-unlispify-menu-entries nil)
  :init (load custom-file 'no-error 'no-message))

;; Better work with parenthesis
(use-package smartparens
  :ensure t
  :init
  (smartparens-global-mode)
  (show-smartparens-global-mode)
  ;; (dolist (hook '(inferior-emacs-lisp-mode-hook
  ;;				  emacs-lisp-mode-hook))
  ;;	(add-hook hook #'smartparens-strict-mode))
  :config
  (require 'smartparens-config)
  (setq sp-autoskip-closing-pair 'always)
  :bind
  (:map smartparens-mode-map
		("C-c s u" . sp-unwrap-sexp)
		("C-c s w" . sp-rewrap-sexp))
  :diminish (smartparens-mode))

;; Highlight current line
(use-package hl-line
  :init (global-hl-line-mode 1))

;; C code style
(defvar c-default-style)
(defvar  c-basic-offset)
(setq c-default-style "k&r"
	  c-basic-offset 4
	  indent-tabs-mode t)

;; Prettify
(global-prettify-symbols-mode t)

;; Smooth Scroll
(use-package smooth-scrolling
  :ensure t
  :config (setq smooth-scroll-margin 2)
  :init (smooth-scrolling-mode 1))
(setq mouse-wheel-scroll-amount '(1 ((shift) .1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 1)
(setq scroll-step 1)

;; If a file is changed outside emacs, load those changes in buffers
(global-auto-revert-mode t)

;; Graphical (auto-)completion
(use-package company
  :ensure t
  :init (global-company-mode)
  :config
  (progn
	(delete 'company-dabbrev company-backends)
	(setq company-tooltip-align-annotations t
		  company-tooltip-minimum-width 27
		  company-idle-delay 0.3
		  company-tooltip-limit 10
		  company-minimum-prefix-length 2
		  company-tooltip-flip-when-above t))
  :bind (:map company-active-map
			  ("M-k" . company-select-next)
			  ("M-i" . company-select-previous)
			  ("TAB" . company-complete-selection))
  :diminish company-mode)

(use-package helm-core
  :ensure t)
(use-package helm
  :ensure t
  :bind (("M-a" . helm-M-x)
		 ("C-x C-f" . helm-find-files)
		 ("C-x f" . helm-recentf)
		 ("C-SPC" . helm-dabbrev)
		 ("M-y" . helm-show-kill-ring)
		 ("C-x b" . helm-buffers-list))
  :bind (:map helm-map
			  ("M-i" . helm-previous-line)
			  ("M-k" . helm-next-line)
			  ("M-I" . helm-previous-page)
			  ("M-K" . helm-next-page)
			  ("M-h" . helm-beginning-of-buffer)
			  ("M-H" . helm-end-of-buffer)
			  ("<tab>" . helm-execute-persistent-action)
			  ("C-i" . helm-execute-persistent-action))
  :config (progn
			(defvar helm-buffers-fuzzy-matching)
			(setq helm-buffers-fuzzy-matching t)
			(helm-mode 1)
			(defvar helm-ff-search-library-in-sexp)
			(defvar helm-ff-file-name-history-use-recentf)
			(defvar helm-ag-fuzzy-match)
			(setq helm-split-window-in-side-p t
				  helm-buffers-fuzzy-matching t
				  helm-move-to-line-cycle-in-source t
				  helm-ff-search-library-in-sexp t
				  helm-ff-file-name-history-use-recentf t
				  helm-ag-fuzzy-match t)

			(substitute-key-definition 'find-tag 'helm-etags-select global-map)
			(defvar projectile-completion-system)
			(setq projectile-completion-system 'helm))
  (add-to-list 'display-buffer-alist
			   `(,(rx bos "*helm" (* not-newline) "*" eos)
				 (display-buffer-reuse-window display-buffer-in-side-window)
				 (reusable-frames . visible)
				 (side . bottom)
				 (window-height . 0.4)))
  :diminish (helm-mode))

(use-package helm-descbinds
  :ensure t
  :bind ("C-h b" . helm-descbinds))
(use-package helm-files
  :bind (:map helm-find-files-map
			  ("M-i" . nil)
			  ("M-k" . nil)
			  ("M-I" . nil)
			  ("M-K" . nil)
			  ("M-h" . nil)
			  ("M-H" . nil)
			  ("M-v" . yank)))

(use-package helm-flycheck
  :ensure t
  :defer t
  :after flycheck)

(use-package multiple-cursors
  :ensure t)

(use-package projectile
  :ensure t
  :bind (("C-x p" . projectile-presp-switch-project))
  :config
  (setq projectile-completion-system 'helm)
  (projectile-global-mode)
  (helm-projectile-on)
  (setq projectile-enable-caching nil)
  :diminish (projectile-mode))

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))

(use-package elixir-mode
  :ensure t
  :commands elixir-mode
  :config (add-hook 'elixir-mode-hook 'alchemist-mode))

(use-package alchemist
  :ensure t
  :commands (alchemist-mode))

(use-package erlang
  :ensure t
  :bind (:map erlang-mode-map ("M-," . alchemist-goto-jump-bacl))
  :config
  (setq erlang-indent-level 2))

(use-package ns-win
  :defer t
  :if (eq system-type 'darwin)
  :config
  (setq ns-pop-up-frames nil
		mac-option-modifier 'meta
		mac-command-modifier 'meta
		mac-right-command-modifier 'left
		mac-right-option-modifier 'none
		mac-function-modifier 'hyper))

(use-package exec-path-from-shell
  :ensure t
  :config
	  (exec-path-from-shell-initialize))

(use-package flycheck
  :ensure t
  :defer 5
  :config
  (global-flycheck-mode 1)
  :diminish (flycheck-mode))

(use-package magit
  :ensure t
  :defer 2
  :bind (("C-x g" . magit-status))
  :config
  (progn
	(delete 'Git vc-handled-backends)))

(use-package whitespace-cleanup-mode
  :ensure t
  :bind (("C-c t c" . whitespace-cleanup-mode)
		 ("C-c x w" . whitespace-cleanup))
  :init (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
		  (add-hook hook #'whitespace-cleanup-mode))
  :diminish (whitespace-cleanup-mode))

;; Org-mode settings
(defvar org-log-done)
(defvar org-todo-keywords)
(defvar org-todo-keywords-face)
(setq org-log-done t
	  org-todo-keywords '((sequence "TODO" "INPROGRESS" "DONE"))
	  org-todo-keywords-face '(("INPROGRESS" . ("foreground " "blue" : weight bold))))

(add-hook 'org-mode-hook
		  (lambda ()
			(flyspell-mode)))

;; disable flyspell welcome message
(defvar flyspell-issue-welcome-flag)
(setq flyspell-issue-welcome-flag nil)

;; change between normal and relative
;; line numbering by C-c ln
(use-package linum-relative
  :ensure t
  :bind ("C-c l n" . linum-relative-toggle))

;; ssh-config
(use-package ssh-config-mode
  :mode ((".ssh/config\\'" . ssh-config-mode)
		 ("sshd?_confi\\'" . ssh-config-mode)
		 ("known_hosts\\'" . ssh-known-hosts-mode)
		 ("authorized_keys2?\\'" . ssh-autorized-keys-mode)))

;; markdown
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
		 ("\\.md\\'" . markdown-mode)
		 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; toml
(use-package toml-mode
  :mode "\\.toml$")

;; yaml
(use-package yaml-mode
  :mode "\\.ya?ml\'")

;;; provide init package
(provide 'init)
;;; init.el ends here
