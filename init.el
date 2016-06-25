;;; Package -- Summary
;;; Commentary:
;; My personal Emacs setup.

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
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
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
(setq ring-bell-function #'ignore
	  inhibit-startup-screen t
	  echo-keystrokes 0.1
	  linum-format " %d")
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
(setq delete-old-version t)
(setq make-backup-files nil)
(setq create-lockfiles nil)

(defun copy-from-osx ())
(shell-command-to-string "pbpaste")

(defun paste-to-osx (text &optional push)
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
										;(server-start)

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
										; (dolist (hook '(inferior-emacs-lisp-mode-hook
										;				  emacs-lisp-mode-hook))
										;	(add-hook hook #'smartparens-strict-mode))
  :config
  (require 'smartparens-config)
  (setq sp-autoskip-closing-pair 'always)
  :bind
  (:map smartparens-mode-map
		("C-c s u" . sp-unwrap-sexp)
		("C-c s w" . sp-rewrap-sexp))
  :diminish (smartparens-mode))

;; Interactivley Do Things for buffers and mode line navigation
(use-package ido
  :config
  (setq ido-enable-flex-matching t)
  (ido-everywhere t)
  (ido-mode 1))

;; Highlight current line
(use-package hl-line
  :init (global-hl-line-mode 1))

;; C code style
(setq c-default-style "k&r"
	  c-basic-offset 4
	  indent-tabs-mode t)

;; Prettify
(global-prettify-symbols-mode 1)

;; Smooth Scroll
(use-package smooth-scrolling
  :ensure t
  :config (setq smooth-scroll-margin 2)
  :init (smooth-scrolling-mode 1))
(setq mouse-wheel-scroll-amount '(1 ((shift) .1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

;; If a file is changed outside emacs, load those changes in buffers
(global-auto-revert-mode t)

(use-package company                    ; Graphical (auto-)completion
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
			(setq helm-buffers-fuzzy-matchin t)
			(helm-mode 1)
			(setq helm-split-window-in-side-p t
				  helm-buffers-fuzzy-matching t
				  helm-move-to-line-cycle-in-source t
				  helm-ff-search-library-in-sexp t
				  helm-ff-file-name-history-use-recentf t
				  helm-ag-fuzzy-match t)

			(substitute-key-definition 'find-tag 'helm-etags-select global-map)
			(setq projectile-completition-system 'helm))
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
  :ensure t
  :bind (("C-c o <SPC>" . mc/vertical-align-with-space)
		 ("C-c o a"     . mc/vertical-align)
		 ("C-c o e"     . mc/mark-more-like-this-extended)
		 ("C-c o h"     . mc/mark-all-like-this-dwim)
		 ("C-c o l"     . mc/edit-lines)
		 ("C-c o n"     . mc/mark-next-like-this)
		 ("C-c o p"     . mc/mark-previous-like-this)
		 ("C-c o r"     . vr/mc-mark)
		 ("C-c o C-a"   . mc/edit-beginnings-of-lines)
		 ("C-c o C-e"   . mc/edit-ends-of-lines)
		 ("C-c o C-s"   . mc/mark-all-in-region))
  :config
  (setq mc/mode-line
		;; Simplify the MC mode line indicator
		'(:propertize (:eval (concat " " (number-to-string (mc/num-cursors))))
					  face font-lock-warning-face)))

(use-package projectile
  :ensure t
  :bind (("C-x p" . projectile-presp-switch-project))
  :config
  (setq projectile-completition-system 'helm)
  (projectile-global-mode)
  (helm-projectile-on)
  (setq projectile-enable-caching nil)
  :diminish (projectile-mode))

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))

(use-package elixir-mode
  :load-path  "~/.emacs.d/emacs-elixir/"
  :config (progn
			(defun my-elixir-do-end-close-action (id action context)
			  (when (eq action 'insert)
				(newline-and-indent)
				(forward-line -1)
				(indent-according-to-mode)))

			(sp-with-modes '(elixir-mode)
			  (sp-local-pair "do" "end"
							 :when '(("SPC" "RET"))
							 :post-handlers '(:add my-elixir-do-end-close-action)
							 :actions '(insert)))))

(use-package alchemist
  :defer 1
  :load-path "~/.emacs.d/alchemist.el/"
  :bind (:map alchemist-iex-mode-map
			  ("C-d" . windmove-right)
			  :map alchemist-mode-map
			  ("M-w" . alchemist-goto-list-symbol-definitions))
  :config (progn
 ;			(setq alchemist-goto-elixir-source-dir "~/Developer/elixir/")
 ;			(setq alchemist-goto-erlang-source-dir "~/Developer/otp/")
			(defun phil-alchemist-mode-hook ()
			  (tester-init-test-run #'alchemist-mix-test-file "_test.exs$")
			  (tester-init-test-suite-run #'alchemist-mix-test))
			(add-hook 'alchemist-mode-hook 'phil-alchemist-mode-hook)

			;; Display alchemist buffer on top
			(add-to-list 'display-buffer-alist
						 `(,(rx bos (or  "*alchemist test report*"
										 "*alchemist mix*"
										 "*alchemist help*"
										 "*alchemist elixir*"
										 "*alchemist elixirc*"))
						   (display-buffer-reuse-window
							display-buffer-in-side-window)
						   (reusable-frames . visible)
						   (side . right)
						   (window-width . 0.5)))))

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

(use-package  exec-path-from-shell
  :ensure t
  :if (and (eq system-type 'darwin) (display-graphic-p))
  :config
  (progn
	(when (string-match-p "/zsh%" (getenv "SHELL"))
	  ;; (setq exec-path-from-shell-arguments '("-l")))

	  (exec-path-from-shell-initialize))))

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

;; ssh-config
(use-package ssh-config-mode
  :mode ((".ssh/config\\'" . ssh-config-mode)
		 ("sshd?_confi\\'" . ssh-config-mode)
		 ("known_hosts\\'" . ssh-known-hosts-mode)
		 ("authorized_keys2?\\'" . ssh-autorized-keys-mode)))

;; json
(use-package json-mode
  :mode "\\.json$"
  :config (setq js-indent-level 4))

;; markdown
(use-package markdown-mode
  :mode ("\\.markdown\\'" "\\.mk?d\\'" "\\.text\\'"))

;; toml
(use-package toml-mode
  :mode "\\.toml$")

;; yaml
(use-package yaml-mode
  :mode "\\.ya?ml\'")

;;; provide init package
(provide 'init)
;;; init.el ends here
