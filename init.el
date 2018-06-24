;;; package --- Summary
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

(defalias 'display-startup-echo-area-message #'ignore)
(defalias 'yes-or-no-p 'y-or-n-p)
(defadvice handle-delete-frame (around my-handle-delete-frame-advice activate)
  "Hide Emacs instead of closing the last frame."
  (let ((frame (posn-window (event-start event)))
		  (numfrs (length(frame-list))))
	(if (> numfrs 1)
		  ad-do-it
	  (do-applescript "tell application \"System Events\" to tell process \"Emacs\" to set visible to false"))))

(setq-default indent-tabs-mode t
              indent-line-function 4
              tab-width 4
              c-basic-offset 4
              fill-column 80
              cursor-in-non-selected-windows nil
			  word-wrap t
			  line-spacing 1)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/custom")


(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
(pixel-scroll-mode  1)
(menu-bar-mode      1)
;;(tool-bar-mode     0)
(scroll-bar-mode   -1)
(show-paren-mode    1)
(line-number-mode   1)
(column-number-mode 1)
(blink-cursor-mode  0)
(transient-mark-mode 1)
(global-hl-line-mode 1)

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
	  cursor-type 'box
	  frame-resize-pixelwise t
	  make-backup-files nil
	  eshell-cmpl-ignore-case t
	  create-lockfiles nil
	  frame-title-format '("%b")
	  gc-cons-threshold 100000000
	  )

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

;; Some org mode stuff
(setq org-log-done 'time)
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-default-notes-file "~/Documents/Notes/notes.org")
(setq org-directory "~/Documents/Notes")
(setq org-agenda-files "~/Documents/Notes/organizer.org")
(setq initial-frame-alist
      '((width . 135)
        (height . 65)))
(setq org-hide-emphasis-markers t)

(use-package eshell
  :defer t
  :config
  (add-hook 'eshell-mode-hook
			(lambda ()
			  (load "~/.emacs.d/custom/eshell-customizations.el"))))

(use-package org-bullets
  :defer t
  :init (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(setq org-todo-keywords
	  (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
			  (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))))

(setq org-todo-keyword-faces
	  (quote (("TODO"      :foreground "red"     :weight bold)
			  ("NEXT"      :foreground "blue"    :weight bold)
			  ("DONE"      :foreground "green"   :weight bold)
			  ("WAITING"   :foreground "orange"  :weight bold)
			  ("HOLD"      :foreground "magenta" :weight bold)
			  ("CANCELLED" :foreground "green"   :weight bold))))

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

(setq org-capture-templates
      (quote (("t" "todo"    entry (file "~/Documents/Notes/organizer.org")
               "* TODO %?\n%U\n%a\n"   :clock-in t)
              ("n" "note"    entry (file "~/Documents/Notes/notes.org")
               "* %? :NOTE:\n%U\n%a\n" :clock-in t)
              ("j" "journal" entry (file+datetree "~/Documents/Notes/journal.org")
               "* %?\n%U\n"            :clock-in t))))

(add-hook 'org-mode-hook #'(lambda ()
							 (visual-line-mode)))

;; --------------------------------- VISUALS --------------------------------------
;; Fonts: Hack 11 /w extra spacing 1 or 12 /w extra spacing 2, Iosevka /w 0 spacing
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(set-frame-font "Hack 11")
(setq doom-themes-padded-modeline t)
(use-package doom-themes
  :ensure t
  :config (load-theme 'doom-vibrant t))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package paredit
  :ensure t
  :diminish paredit-mode
  :config
  (add-hook 'emacs-lisp-mode-hook                  #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook                        #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook                        #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook            #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook                      #'enable-paredit-mode)
  (add-hook 'clojure-mode-hook                     #'enable-paredit-mode))

(use-package highlight-numbers
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))

(use-package highlight-quoted
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-quoted-mode))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook                  'rainbow-delimiters-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook 'rainbow-delimiters-mode)
  (add-hook 'ielm-mode-hook                        'rainbow-delimiters-mode)
  (add-hook 'lisp-mode-hook                        'rainbow-delimiters-mode)
  (add-hook 'lisp-interaction-mode-hook            'rainbow-delimiters-mode)
  (add-hook 'scheme-mode-hook                      'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook                     'rainbow-delimiters-mode))

(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))

;; use C-M-i for ivy fuzzy regex completion
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
  :init (add-hook 'after-init-hook #'ivy-mode)
  :config
  (setq ivy-height 12
		ivy-wrap t
		ivy-fixed-height-minibuffer t
		projectile-completion-system 'ivy
		smex-completion-method 'ivy))

(use-package swiper
  :bind (("C-s"     . swiper)
         ("C-r"     . swiper)
         ("C-c u"   . swiper-all)
         ("C-c C-r" . ivy-resume)
         ("C-c C-o" . ivy-occur)
         ("C-c C-b" . ivy-switch-buffer)
         ("C-c C-k" . kill-buffer))
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
  :defer t)

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
		 ("C-c g" . magit-file-log)))

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

(setq scheme-program-name "/usr/local/bin/mit-scheme")

;; Added support for Ctags, and Cquery which currently blows my mind
(setq path-to-ctags "/usr/local/bin/ctags")
(defun create-tags (dir-name)
  "Create tags file in directory DIR-NAME."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -f TAGS -e -R %s" path-to-ctags (directory-file-name dir-name ))))

(use-package projectile
  :defer t
  :config
  (setq projectile-completion-system 'ivy
		projectile-enable-caching t))

(use-package ggtags
  :defer t)

(use-package counsel-gtags
  :defer t)

(use-package counsel-projectile
  :defer t)

(use-package lsp-mode
  :defer t)

(setq cquery-executable "/usr/local/bin/cquery")
(setq cquery-extra-init-params '(:index (:comments 2) :cacheFormat "msgpack" :completion (:detailedLabel t)))

(defun cquery//enable ()
  "Enable cquery for all C/C++ modes."
  (condition-case nil
	  (lsp-cquery-enable)
	(user-error nill)))

(use-package cquery
  :commands (lsp-cquery-enable)
  :hook (c-mode . #'cquery//enable)
  :config
  (add-hook 'c-mode-hook #'cquery//enable)
  (add-hook 'c++-mode-hook #'cquery//enable))

(use-package company-lsp
  :after (cquery company lsp-mode)
  :config (push 'company-lsp company-backends)
  (setq company-transformers nil company-lsp-async t company-lsp-cache-candidates nil))

(use-package ivy-xref
  :after cquery
  :config (set-variable 'xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package ace-window
  :commands ace-window
  :init (bind-key "C-x o" 'ace-window)
  :config (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package tex
  :defer t
  :ensure auctex
  :config (setq TeX-auto-save nil))

(use-package elfeed
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
		("http://lambda-the-ultimate.org/rss.xml" functional))))
(add-hook 'elfeed-show-mode-hook
		  (lambda () (set-face-attribute 'variable-pitch (selected-frame) :font
									  (font-spec :family "Helvetica Neue" :size 14))))

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
			 :channels ("#clojure" "#haskell" "#illumos" :after-auth "#emacs" "#freebsd"))
			("OFTC"
			 :nick "phlm"
			 :channels ("#kernelnewbies"))))
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

;; Local Variables:
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
(provide 'init)
;;; init ends here
