;;; package --- Summary
;;; Commentary:
(require 'package)
;;; Code:
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa"     . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu"       . "https://elpa.gnu.org/packages/"))
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
              cursor-in-non-selected-windows nil)
(setq-default word-wrap t)
(add-to-list 'load-path "~/.emacs.d/custom")
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
(pixel-scroll-mode 1)
(menu-bar-mode      1)
(tool-bar-mode     -1)
(scroll-bar-mode   -1)
(show-paren-mode    1)
(line-number-mode   1)
(column-number-mode 1)
(blink-cursor-mode  0)
(global-hl-line-mode -1)
(global-auto-revert-mode 1)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load "~/.emacs.d/custom/eshell-customizations.el")
(setq-default line-spacing 1)
(set-frame-font "Hack 11")
;; (set-frame-font "Fira Code Retina 11")

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
	  create-lockfiles nil)
(setq gc-cons-threshold most-positive-fixnum)

;; Some custom functions
(defun open-previous-line (arg)
  "Open a new line above current one ARG."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (when newline-and-indent
	(indent-according-to-mode)))
(global-set-key (kbd "M-o") 'open-previous-line)
(defvar newline-and-indent t)
;;------------------------------- ORG MODE SETTINGS ----------------------------
(setq org-log-done 'time)
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-default-notes-file "~/Documents/Notes/notes.org")
(setq org-directory "~/Documents/Notes")
(setq org-agenda-files "~/Documents/Notes/organizer.org")
(setq initial-frame-alist
      '((width . 120)
        (height . 65)))
(setq org-hide-emphasis-markers t)
(use-package org-bullets
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

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
               "* TODO %?\n%U\n%a\n"   :clock-in t :clock-resume t)
              ("n" "note"    entry (file "~/Documents/Notes/notes.org")
               "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
              ("j" "journal" entry (file+datetree "~/Documents/Notes/journal.org")
               "* %?\n%U\n"            :clock-in t :clock-resume t))))

(let* ((variable-tuple (cond ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
                             ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
                             (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
       (headline           `(:inherit default :weight bold)))

  (custom-theme-set-faces 'user
                          `(org-level-8 ((t (,@headline ,@variable-tuple))))
                          `(org-level-7 ((t (,@headline ,@variable-tuple))))
                          `(org-level-6 ((t (,@headline ,@variable-tuple))))
                          `(org-level-5 ((t (,@headline ,@variable-tuple))))
                          `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
                          `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
                          `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
                          `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
                          `(org-document-title ((t (,@headline ,@variable-tuple :height 1.5 :underline nil))))))
;; ---------------------------------------------------------------------------------------------------------------
(setq doom-vibrant-comment-bg t)
(setq doom-vibrant-padded-modeline t)

(use-package doom-themes
  :ensure t
  :config (load-theme 'doom-one t))

(use-package find-file-in-project
  :ensure t
  :bind ("C-c f" . ffip))

;; Get used to changes and learn how to use it, instead of paredit
;; (use-package parinfer
;;   :ensure t
;;   :bind
;;   (("C-," . parinfer-toggle-mode))
;;   :init
;;   (progn
;;     (setq parinfer-extensions
;;           '(defaults       ; should be included.
;;             pretty-parens  ; different paren styles for different modes.
;;             paredit        ; Introduce some paredit commands.
;;             smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
;;             smart-yank))   ; Yank behavior depend on mode.
;;     (add-hook 'clojure-mode-hook #'parinfer-mode)
;;     (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
;;     (add-hook 'common-lisp-mode-hook #'parinfer-mode)
;;     (add-hook 'scheme-mode-hook #'parinfer-mode)
;;     (add-hook 'lisp-mode-hook #'parinfer-mode)))

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

;; Use C-M-i for ivy fuzzy regex completion
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

(use-package swiper
  :demand
  :bind (("C-s"     . swiper)
         ("C-r"     . swiper)
         ("C-c u"   . swiper-all)
         ("C-c C-r" . ivy-resume)
         ("C-c C-o" . ivy-occur)
         ("C-c C-b" . ivy-switch-buffer)
         ("C-c C-k" . kill-buffer))
  :config
  (progn (ivy-mode 1)
         (setq ivy-height 10)
         (setq enable-recursive-minibuffers t)
         (setq swiper-include-line-number-in-search t)
         (setq ivy-re-builders-alist
               '((counsel-M-x . ivy--regex-fuzzy)
                 (t . ivy--regex-plus)))

         (define-key ivy-minibuffer-map (kbd "TAB") 'ivy-partial-or-done)))

(use-package smex
  :ensure t
  :config (smex-initialize))

(use-package counsel
  :ensure t
  :bind (("M-x"     . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
		 ("C-c p"   . counsel-file-jump)
         ("C-h f"   . counsel-describe-function)
         ("C-h v"   . counsel-describe-variable)
         ("C-c s s" . counsel-rg)
         ("M-y"     . counsel-yank-pop))
  :config
  (progn (defun counsel-major-mode-commands (&optional initial-input)
           "Ivy version of `smex-major-mode-commands'.
Optional INITIAL-INPUT is the initial input in the minibuffer."
           (interactive)
           (unless initial-input
             (setq initial-input (cdr (assoc this-command
                                             ivy-initial-inputs-alist))))
           (let* ((cands obarray)
                  (pred 'commandp)
                  (sort t))
             (when (require 'smex nil 'noerror)
               (unless smex-initialized-p
                 (smex-initialize))
               (smex-detect-new-commands)
               (smex-update)
               (setq cands (delete-dups (append (smex-extract-commands-from-keymap (current-local-map))
                                                (smex-extract-commands-from-features major-mode))))
               (setq cands (smex-sort-according-to-cache cands))
               (setq cands (mapcar #'symbol-name cands))
               (setq pred nil)
               (setq sort nil))
              (ivy-read (counsel--M-x-prompt) cands
                        :predicate pred
                        :require-match t
                        :history 'extended-command-history
                        :action
                        (lambda (cmd)
                          (when (featurep 'smex)
                            (smex-rank (intern cmd)))
                          (let ((prefix-arg current-prefix-arg)
                                (this-command (intern cmd)))
                            (command-execute (intern cmd) 'record)))
                        :sort sort
                        :keymap counsel-describe-map
                        :initial-input initial-input
                        :caller 'counsel-major-mode-commands)))
		 
         (defun counsel-describe-package ()
           "Forward to `describe-package'."
           (interactive)
           (ivy-read "Describe package: "
                     (let ((packages (append (mapcar 'car package-alist)
                                             (mapcar 'car package-archive-contents)
                                             (mapcar 'car package--builtins))))
                       (delq nil
                             (mapcar (lambda (elt)
                                       (symbol-name elt))
                                     packages)))
                     :action (lambda (x)
                               (describe-package (intern x)))
                     :caller 'counsel-describe-package))))

(use-package flycheck
  :ensure t
  :init
  (progn
	(add-hook 'prog-mode-hook (flycheck-mode 1))))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
		 ("C-c g" . magit-file-log)))

(use-package cider
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

(use-package ggtags
  :ensure t)

(use-package counsel-gtags
  :ensure t)

(use-package lsp-mode
  :ensure t)

(setq cquery-executable "/usr/local/bin/cquery")
(setq cquery-extra-init-params '(:index (:comments 2) :cacheFormat "msgpack" :completion (:detailedLabel t)))

(use-package cquery
  :ensure t
  :after lsp-mode)

(use-package company-lsp
  :ensure t
  :after (cquery company)
  :config (push 'company-lsp company-backends)
  (setq company-transformers nil company-lsp-async t company-lsp-cache-candidates nil))

(use-package lsp-ui
  :ensure t
  :config (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package ivy-xref
  :ensure t
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
		("https://www.joelonsoftware.com/feed/")
		("https://eli.thegreenplace.net/feeds/all.atom.xml")
		("https://blog.acolyer.org/feed/" papers)
		("http://bit-player.org/feed")
		("http://feeds.feedburner.com/HighScalability")
		("https://blog.codinghorror.com/rss/")
		("https://martinfowler.com/feed.atom" agile)
		("https://steve-yegge.blogspot.rs/")
		("http://blog.cognitect.com/blog?format=rss" clojure)
		("http://www.righto.com/feeds/posts/default" hardware))))

(defun indent-buffer ()
  "Indent current buffer according to major mode."
  (interactive)
  (indent-region (point-min) (point-max)))

(use-package rust-mode
  :ensure t
  :config (add-hook 'rust-mode-hook
					(lambda ()
					  (local-set-key (kbd "C-c <tab>") #'rust-format-buffer))))

(use-package cargo
  :ensure t
  :config (add-hook 'rust-mode-hook 'cargo-minor-mode))

(use-package racer
  :ensure t
  :config
  (progn
	(setq racer-cmd "~/.cargo/bin/racer")
	(setq racer-rust-src-path "/Users/phil/Developer/other/rust/src")
	(add-hook 'rust-mode-hook #'racer-mode)
	(add-hook 'racer-mode-hook #'company-mode)))
;; Local Variables:
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
(provide 'init)
;;; init ends here
