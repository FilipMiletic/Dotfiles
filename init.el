;;; Package --- Summary
;;; Commentary:
(require 'package)
;;; Code:
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; -----------------------------------------------------------------------------
;; Basic settings
;; -----------------------------------------------------------------------------
(defalias 'display-startup-echo-area-message #'ignore)
(defalias 'yes-or-no-p 'y-or-n-p)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook
          '(lambda() (set-fill-column 80)))

;; MacOS dock behaviour
(defadvice handle-delete-frame (around my-handle-delete-frame-advice activate)
  "Hide Emacs instead of closing the last frame."
  (let ((frame (posn-window (event-start event)))
        (numfrs (length(frame-list))))
    (if (> numfrs 1)
        ad-do-it
      (do-applescript "tell application \"System Events\" to tell process \"Emacs\" to set visible to false"))))

(setq-default indent-tabs-mode nil
              indent-line-function 4
              tab-width 4
              fill-column 80
              cursor-type 'box
              cursor-in-non-selected-windows 'hollow
              require-final-newline t)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/custom")
(setq use-package-always-ensure t)
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
(transient-mark-mode t)
(pixel-scroll-mode   1)
(tool-bar-mode       0)
(unless (display-graphic-p)
  (menu-bar-mode -1))
(scroll-bar-mode     0)
(show-paren-mode     1)
(line-number-mode    1)
(column-number-mode  1)
(blink-cursor-mode   1)

(setq c-default-style "gnu")
(setq-default c-basic-offset 8)
(setq mac-option-modifier nil
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
      frame-title-format (list "emacs - %b  %f")
      gc-cons-threshold (* 50 1000 1000)
      ns-use-mwheel-momentum t
      ns-use-mwheel-acceleration t
      ns-use-thin-smoothing nil
      ns-antialias-text t
      shell-file-name "/bin/bash"
      blink-cursor-blinks 7)

;; -----------------------------------------------------------------------------
;; Custom functions and customisations of builtin packages
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
(setq org-hide-emphasis-markers t)

(setq eshell-scroll-to-bottom-on-output nil)
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
    '((side . bottom)))))

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
  (setq org-agenda-files (quote ("~/Documents/org/plan.org"))
        org-todo-keywords
        '((sequence "TODO" "HOLD" "DONE"))))

(add-hook 'org-mode-hook #'(lambda ()
                             (visual-line-mode)))

(add-to-list 'default-frame-alist '(ns-appearance . dark))
(set-face-attribute 'default nil :font "Noto Sans Mono-12:weight=Medium")
(add-hook 'org-mode-hook '(lambda () (setq fill-column 80)))
(add-hook 'org-mode-hook 'auto-fill-mode)
(setq org-html-validation-link nil)

;; -----------------------------------------------------------------------------
;; -- Packages
;; -----------------------------------------------------------------------------
;;
;; Light is Acme inspired, dark theme is my minimalistic 8bit retro theme with
;; rainbow parens cause Lispsss (blackbox)))).
;; Use Acme it in case of bright environments, else use blackbox.
;;
;; (use-package acme-theme
;;   :init (setq acme-theme-gray-rainbow-delimiters nil
;;               acme-theme-more-syntax-hl t))

(load-theme 'blackbox t)

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package shackle
  :config
  (progn
    (setq shackle-default-alignment 'below)
    (setq shackle-default-size 0.4)
    (setq shackle-select-reused-windows nil)
    (setq shackle-rules
          '((compilation-mode :noselect t)
            ("*undo-tree*"    :noselect t :size 0.25 :align right)
            ("*eshell*" :select t :other t)
            ("*Shell Command Output*" :noselect t)
            (occur-mode :noselect t :align t)
            ("*Help*" :select t :inhibit-window-quit t :other t)
            ("*Messages*" :noselect t :inhibit-window-quit t :other t)
            (magit-status-mode :select t :inhibit-window-quit t :same t)
            (magit-log-mode :select t :inhibit-window-quit t :same t)))
    (shackle-mode 1)))

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
  :diminish
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
  :config
  (setq company-auto-complete nil
        company-idle-delay .3
        company-echo-delay 0
        company-tooltip-flip-when-above t
        company-minimum-prefix-length 2
        company-tooltip-limit 12))

(use-package ivy
  :init (add-hook 'after-init-hook #'ivy-mode)
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
;; IRC, RSS, git, navigation and snippets, and shackle for minibuffers
;; -----------------------------------------------------------------------------
;; Git and project organisation
(use-package magit
  :defer t
  :bind (("C-x g" . magit-status)
         ("C-c g" . magit-file-log)))

(use-package projectile
  :config
  (setq projectile-completion-system 'ivy
        projectile-enable-caching t
        projectile-find-dir-includes-top-level t))

;; Snippets
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

;; RSS
(use-package elfeed
  :defer t
  :bind ("C-c w" . elfeed)
  :init (setf url-queue-timeout 30)
  :config
  (push "-k" elfeed-curl-extra-arguments)
  (setq-default elfeed-search-filter "@1-week-ago +unread")
  (setq elfeed-feeds
        '(("https://nullprogram.com/feed/"                       systems emacs)
          ("https://eli.thegreenplace.net/feeds/all.atom.xml"                 )
          ("http://bit-player.org/feed"                                       )
          ("https://blog.codinghorror.com/rss/"                               )
          ("https://www.tedunangst.com/flak/rss"                              )
          ("http://www.righto.com/feeds/posts/default"                hardware)
          ("http://tonsky.me/blog/atom.xml"                            clojure)
          ("https://furbo.org/feed/"                                          )
          ("https://ferd.ca/feed.rss"))))

;; IRC
(setq my-credentials-file "~/.emacs.d/.private.el")
(defun my-nickserv-password (server)
  "IRC SERVER authentication."
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
             :channels (:after-auth "#lisp" "#emacs" "#freebsd" "#haskell"
                                    "#clojure" "#illumos" "##c++"))
            ("Rizon"
             :host "irc.rizon.net"
             :nick "phlm"
             :port 9999
             :use-tls t
             :nickserv-password my-nickserv-password
             :channels (:after-auth "#dailyprog"))))
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

;; -----------------------------------------------------------------------------
;; Languages: C, C++, Rust, Go, Racket/Scheme, Clojure, Latex;
;; -----------------------------------------------------------------------------
;; C/C++
;; Use llvm style for C++
;; For C use default GNU 8 spaces tab
(c-add-style "llvm.org"
             '("gnu"
               (fill-column . 80)
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

(setq ccls-executable "/usr/local/Cellar/ccls/0.20180924/bin/ccls")

(use-package ccls
  :commands (lsp)
  :config (setq company-transformers nil
                company-lsp-async t
                company-lsp-cache-candidates nil))
(add-hook 'c++-mode-hook  (lambda () (require 'ccls) (lsp)))
(add-hook 'c-mode-hook    (lambda () (require 'ccls) (lsp)))
(add-hook 'cuda-mode-hook (lambda () (require 'ccls) (lsp)))

(use-package counsel-gtags
  :defer t)

(use-package counsel-projectile
  :defer t)

(setq clang-format-style-option "llvm")
(use-package clang-format
  :commands (clang-format-region)
  :config (setq clang-format-style-option "llvm"))

(use-package xcscope
  :init (cscope-setup))

(use-package lsp-mode
  :commands lsp
  :config (require 'lsp-clients))

(use-package company-lsp
  :after (ccls company lsp-mode)
  :config (push 'company-lsp company-backends)
  (setq company-transformers nil company-lsp-async t
        company-lsp-cache-candidates nil))

;; Racket/Scheme
(use-package geiser
  :defer t
  :bind (:map scheme-mode-map ("C-c C-c" . geiser-eval-last-sexp))
  :config
  (progn
    (setq geiser-active-implementations '(racket))
    (setq geiser-default-implementation 'racket)))

;; Clojure
(use-package cider
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

(use-package clj-refactor
  :defer t)

;; Rust
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
    (setq racer-rust-src-path "/Users/phil/Developer/other/rust/src")
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'company-mode)))

(use-package realgud
  :defer t)

;; Go
(use-package go-mode
:ensure t
:init (add-hook 'go-mode-hook
                  (lambda ()
                    (setq gofmt-command "goimports")
                    (add-hook 'before-save-hook 'gofmt-before-save)
                    (setq truncate-lines t)
                    (setq indent-tabs-mode t)
                    (setq tab-width 4))))

(use-package company-go
  :ensure t)

(add-hook 'go-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-go))
                          (company-mode)))

(use-package go-eldoc
:ensure t
:init (add-hook 'go-mode-hook 'go-eldoc-setup))

;; LaTeX -- C-c C-c to compile LaTeX to pdf, and C-c C-c to open it in Skim
;;          C-c C-l to view compilation output
(setq TeX-PDF-mode t)
(use-package auctex-latexmk
  :defer t)

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (push
             '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
               :help "Run latexmk on file")
             TeX-command-list)))
(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))

(setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list
      '(("PDF Viewer"
         "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))

(custom-set-variables
 '(TeX-source-correlate-method 'synctex)
 '(TeX-source-correlate-mode t)
 '(TeX-source-correlate-start-server t))

;; -----------------------------------------------------------------------------
;; Local Variables:
;; byte-compile-warnings: (not free-vars noruntime)
;; fill-column: 80
;; End:

(provide 'init)
;;; init ends here
