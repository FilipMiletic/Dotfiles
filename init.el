;;; Package --- Summary
;;; Commentary:
(require 'package)
;;; Code:
(package-initialize)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(when (not package-archive-contents)
  (package-refresh-contents))

(require 'tls)
(push "/usr/local/etc/openssl/cert.pem" gnutls-trustfiles)
(with-eval-after-load 'tls
    (push "/usr/local/etc/openssl/cert.pem" gnutls-trustfiles))
(setq tls-checktrust 'ask)

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; -----------------------------------------------------------------------------
;; Basic settings
;; -----------------------------------------------------------------------------
(defalias 'display-startup-echo-area-message #'ignore)
(defalias 'yes-or-no-p 'y-or-n-p)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; MacOS dock behaviour
(defadvice handle-delete-frame (around my-handle-delete-frame-advice activate)
  "Hide Emacs instead of closing the last frame."
  (let ((frame (posn-window (event-start event)))
        (numfrs (length(frame-list))))
    (if (> numfrs 1)
        ad-do-it
      (do-applescript "tell application \"System Events\" to tell process \"Emacs\" to set visible to false"))))
(defvar site-lisp-path "~/.emacs.d/")
(defvar custom-conf-lisp-path (concat site-lisp-path "custom/"))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path custom-conf-lisp-path)
(add-to-list 'load-path (concat custom-conf-lisp-path "langs/"))
;;(setq use-package-always-ensure t)
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
;; -----------------------------------------------------------------------------
(transient-mark-mode 1)
(tool-bar-mode       0)
(scroll-bar-mode     0)
(show-paren-mode     1)
(line-number-mode    1)
(column-number-mode  1)
(blink-cursor-mode   1)

(setq-default indent-tabs-mode nil
              indent-line-function 2
              tab-width 2
              fill-column 80
              cursor-type 'box
              cursor-in-non-selected-windows 'hollow
              require-final-newline t
              default-directory "/Users/phlm/")

(setq mouse-autoselect-window nil
      focus-follows-mouse nil
      mac-option-modifier nil
      mac-command-modifier 'meta
      dired-use-ls-dired nil
      load-prefer-newer t
      delete-old-versions t
      ring-bell-function 'ignore
      inhibit-splash-screen t
      initial-scratch-message nil
      inhibit-startup-message t
      mouse-wheel-scroll-amount '(1 ((shift) .1))
      mouse-wheel-progressive-speed nil
      scroll-step 1
      scroll-conservatively 100000
      scroll-margin 10
      fringes-outside-margins 1
      ns-pop-up-frames nil
      make-backup-files nil
      auto-save-default nil
      create-lockfiles nil
      frame-resize-pixelwise t
      make-backup-files nil
      eshell-cmpl-ignore-case t
      create-lockfiles nil
      frame-title-format (list "emacs - %b  %f")
      gc-cons-threshold (* 50 1000 1000)
      ns-use-mwheel-momentum t
      ns-use-mwheel-acceleration t
      ns-use-thin-smoothing t
      ns-antialias-text t
      shell-file-name "/bin/bash"
      blink-cursor-blinks 7)

;; -----------------------------------------------------------------------------
;; Custom functions and customizations of builtin packages
;; -----------------------------------------------------------------------------
(add-hook 'hi-lock-mode-hook
          (lambda nil
            (highlight-regexp "FIXME" (quote hi-red-b))
            (highlight-regexp "TODO" (quote hi-red-b))))

(defun open-previous-line (arg)
"Open a new line above current one ARG."
(interactive "p")
(beginning-of-line)
(open-line arg)
(when newline-and-indent
  (indent-according-to-mode)))

(global-set-key (kbd "C-x C-b") 'ivy-switch-buffer)
(global-set-key (kbd "M-o") 'open-previous-line)
(global-set-key (kbd "C-x C-u") 'undo-tree-mode)
(defvar newline-and-indent t)

(defun indent-buffer ()
  "Indent current buffer according to major mode."
  (interactive)
  (indent-region (point-min) (point-max)))

(setq initial-frame-alist
      '((width . 120)
        (height . 53)))

(defun eshell/clear ()
  "Clear eshell buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun fm/finder ()
  "Open current file in Finder."
  (interactive)
  (let ((file (buffer-file-name)))
    (if file
        (shell-command
         (format "%s %s" (executable-find "open") (file-name-directory file)))
      (error "Buffer is not attached to any file!"))))

(defun fm/rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it has open with NEW-NAME."
  (interactive "New name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

(defun fm/pop-shell (arg)
  "Pop shell in a side window.  Pass ARG to shell."
  (interactive "P")
  (select-window
   (display-buffer-in-side-window
    (save-window-excursion
      (let ((prefix-arg arg))
        (call-interactively #'eshell))
      (current-buffer))
    '((side . right)))))

(add-to-list 'default-frame-alist '(ns-appearance . dark))

(setq-default line-spacing 1)
(set-face-font 'default "Monaco-11")

;; Like is this really necessary or is it just placebo?
;; -- what the fuck is wrong with me?!
(defun my-scroll-hook(_)
  "Increase gc-threshold before scroll and set it back after."
  (setq gc-cons-threshold most-positive-fixnum)
  (run-with-idle-timer 3 nil (lambda () (setq gc-cons-threshold (* 8 1024 1024)))))

(advice-add 'scroll-up-line :before 'my-scroll-hook)
(advice-add 'scroll-down-line :before 'my-scroll-hook)

;; -----------------------------------------------------------------------------
;; -- Packages
;; got so used to my own theme that I just can't use others, Emacs highlights so
;; much shit that it's unbearable; people coded without syntax for half a century
(defadvice load-theme (before clear-previous-themes activate)
  "Clear existing theme settings instead of layering them."
  (mapc #'disable-theme custom-enabled-themes))

;; jai and organic-green as contrasting themes, ir-black-24 as default one
(load-theme 'ir-black-24 t)


;; -----------------------------------------------------------------------------
;; Making Emacs comfy desu.
;; -----------------------------------------------------------------------------
(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package paredit
  :diminish paredit-mode
  :config
  (dolist (m '(clojure-mode-hook
               cider-repl-mode-hook
               clojure-mode-hook
               emacs-lisp-mode-hook
               racket-mode-hook
               racket-repl-mode-hook
               scheme-mode-hook
               slime-mode-hook
               slime-repl-mode-hook
               eval-expression-minibuffer-setup-hook))
    (add-hook m #'paredit-mode)))

(use-package highlight-numbers
  :config
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))

(use-package rainbow-delimiters
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
  :init (exec-path-from-shell-initialize)
  :config
  (setq exec-path-from-shell-check-startup-files nil))

(use-package which-key
  :defer 1
  :diminish which-key-mode
  :config (which-key-mode))

(use-package hl-todo
  :ensure t
  :config (dolist (m '(c-mode-hook
                       c++-mode-hook
                       scheme-mode-hook
                       clojure-mode-hook
                       lisp-mode-hook))
            (add-hook m #'hl-todo-mode)))

(use-package company
  :defer 1
  :diminish company-mode
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  (add-hook 'after-init-hook (lambda ()
                               (company-mode -1)))
  :config
  (setq company-auto-complete nil
        company-idle-delay .3
        company-show-numbers t
        company-echo-delay 0
        company-tooltip-flip-when-above t
        company-minimum-prefix-length 2
        company-tooltip-limit 12))


(use-package ivy
  :init (add-hook 'after-init-hook #'ivy-mode)
  :diminish (ivy-mode)
  :config
  (setq ivy-height 15
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
  :bind (("C-s"         . swiper)
         ("C-r"         . swiper)
         ("C-c u"   . swiper-all)
         ("C-c C-r" . ivy-resume)
         ("C-c C-o" . ivy-occur))
  :config (setq swiper-include-line-number-in-search t))

(use-package smex
  :config (smex-initialize))

(use-package counsel
  :bind (("M-x"     . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-h f"   . counsel-describe-function)
         ("C-h v"   . counsel-describe-variable)
         ("C-c C-s" . counsel-rg)
         ("M-y"     . counsel-yank-pop)))

(use-package flycheck
  :init (global-flycheck-mode))


;; -----------------------------------------------------------------------------
;; Git, project organisation and snippets
;; -----------------------------------------------------------------------------
(use-package magit
  :defer t
  :bind (("C-x g" . magit-status)
         ("C-c g" . magit-file-log)))

(defun fm/projectile-proj-find (dir)
  "Function that will tell project.el to find a project DIR via Projectile."
  (let ((root (projectile-project-root dir)))
    (and root (cons 'transient root))))

(use-package projectile
  :config
  (setq projectile-completion-system 'ivy
        projectile-enable-caching t
        projectile-find-dir-includes-top-level t))

(use-package yasnippet
  :disabled t
  :defer t
  :diminish yas-minor-mode
  :bind (("C-c C-c" . yas-insert-snippet))
  :config (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode))

(use-package ace-window
  :commands ace-window
  :init (bind-key "C-x o" 'ace-window)
  :config (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))


;; -----------------------------------------------------------------------------
;; Languages: C, C++, Rust, Clojure, Latex(basiclatex)
;; -----------------------------------------------------------------------------
;; C/C++ -- just add tag system. No completion, it causes more harm than good in
;; cpp, especialy in larger codebases.

(c-add-style "llvm.org"
             '("gnu"
               (c++-indent-level . 2)
               (c-basic-offset . 2)
               (indent-tabs-mode . nil)
               (c-offsets-alist . ((arglist-intro . ++)
                                   (innamespace . 0)
                                   (member-init-intro . ++)))))

(add-hook 'c++-mode-hook (function (lambda ()
                                     (progn
                                       (c-set-style "llvm.org")))))

(use-package ggtags
  :defer t
  :config
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode)
                (ggtags-mode 1)))))

(use-package counsel-gtags
  :defer t)
(use-package counsel-projectile
  :defer t)
(setq clang-format-style-option "llvm")
(use-package clang-format
  :commands (clang-format-region)
  :config (setq clang-format-style-option "llvm"))

;; Using clangd and eglot(LSP for Emacs) for completion
(defvar fm-clangd (executable-find "clangd")
  "Clangd executable path.")
(setq fm-clangd "/usr/local/opt/llvm/bin/clangd")

(use-package eglot
    :ensure t
    :config
    (require 'eglot))

(defun fm/cpp-eglot-enable ()
  "Enable variables and hooks for eglot cpp when digging into C++ project."
  (interactive)

  (setq company-backends
        (cons 'company-capf
              (remove 'company-capf company-backends)))
  (projectile-mode t)
  (with-eval-after-load 'project
    (add-to-list 'project-find-functions
                 'fm/projectile-proj-find))
  (add-to-list 'eglot-server-programs `((c++-mode) ,fm-clangd))
  (add-hook 'c++-mode-hook 'eglot-ensure))

(defun fm/cpp-eglot-disable ()
  "Disable hook for eglot."
  (interactive)
  (remove-hook 'c++-mode-hook 'eglot-ensure))


;; Clojure  --------------------------------------------------------------------
;; cider is the main environment, with clj-refactor for auto import
;; and refactoring, Emacs is the way to go when I work /w some Lisp or Clojure
(use-package cider
  :defer t
  :commands (cider cider-connect cider-jack-in)
  :mode (("\\.clj\\'"  . clojure-mode)
         ("\\.edn\\'"  . clojure-mode)
         ("\\.cljx\\'" . clojure-mode)
         ("\\.cljc\\'" . clojure-mode))
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
        nrepl-popup-stacktraces nil
        cljr-inject-dependencies-at-jack-in nil)
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-mode-hook #'company-mode)
  (add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'company-mode))

(use-package clj-refactor
  :defer t)


;; Rust  -----------------------------------------------------------------------
;; rustfmt working C-c C-f; racer working for completion and navigation (rls?);
(use-package rust-mode
  :defer t
  :config (add-hook 'rust-mode-hook
                    (lambda ()
                      (local-set-key (kbd "C-c <tab>") #'rust-format-buffer))))
(setq company-tooltip-align-annotations t)

(use-package cargo
  :defer t
  :config (add-hook 'rust-mode-hook 'cargo-minor-mode))

(use-package racer
  :config
  (progn
    (setq racer-cmd "~/.cargo/bin/racer")
    (setq racer-rust-src-path "/Users/phlm/.rustup/toolchains/nightly-x86_64-apple-darwin/lib/rustlib/src/rust/src")
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'company-mode)))


;; CUDA; OpenCL; GLSL
(use-package opencl-mode
  :defer t)
(use-package cuda-mode
  :defer t)
(use-package glsl-mode
  :defer t)
;; -----------------------------------------------------------------------------
;; Local Variables:
;; byte-compile-warnings: (not free-vars noruntime)
;; fill-column: 80
;; End:

(provide 'init)
;;; init ends here
(put 'dired-find-alternate-file 'disabled nil)
