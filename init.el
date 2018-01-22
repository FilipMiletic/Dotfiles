;;; package --- Summary
;;; Commentary:
(require 'package)
;;; Code:
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa"     . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu"       . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
;;(require 'diminish)
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

(add-to-list 'load-path "~/.emacs.d/custom")
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
(hl-line-mode       1)
(menu-bar-mode      1)
(tool-bar-mode     -1)
(scroll-bar-mode   -1)
(show-paren-mode    1)
(line-number-mode   1)
(column-number-mode 1)
(blink-cursor-mode  0)
(global-auto-revert-mode 1)

(load "~/.emacs.d/custom/eshell-customizations.el")
(set-frame-font "Fira Code Retina 11")

(setq mac-option-modifier nil
      mac-command-modifier 'meta
      load-prefer-newer t
      gc-cons-threshold 200000000
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
(setq org-log-done 'time)
(setq initial-frame-alist
      '((width . 120)
        (height . 65)))

(use-package kaolin-themes
  :ensure t
  :config (load-theme 'kaolin-dark t))

(use-package org-bullets
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

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
  (add-hook 'clojure-mode-hook                     #'enable-paredit-mode)
  (add-hook 'slime-repl-mode-hook                  #'enable-paredit-mode))

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
  (add-hook 'clojure-mode-hook                     'rainbow-delimiters-mode)
  (add-hook 'slime-repl-mode-hook                  'rainbow-delimiters-mode))

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
         ("C-c k"   . counsel-rg)
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
                        :caller 'counsel-major-mode-commands)
			 ))
		 
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

;; TODO: Configure flycheck to work with cquery
(use-package flycheck-irony
  :ensure t
  :init (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;; (defun counsel-find-function (str)
;;   (if (< (length str) 3)
;;       (counsel-more-chars 3)
;;     (let ((cmd
;;             (format
;;               "find %s ! -readable -prune -o -iname \"%s*\" -print"
;;               ; FIX: configure it and use `mdfind' instead
;;				 ; NOTE: some versions of `find' may require parentheses,
;;               ; like this: \( ! -readable -prune \)
;;               default-directory
;;               (counsel-unquote-regex-parens
;;               (ivy--regex str)))))
;;       (message "%s" cmd)
;;       (counsel--async-command cmd))
;;     '("" "working...")))

;; ;;;###autoload
;; (defun counsel-find (&optional initial-input)
;;   "Use `mdfind', `counsel' and `ivy' to present all paths
;;    in a directory tree that match the `REGEX' input"
;;   (interactive)
;;   (ivy-read "Find: " #'counsel-find-function
;;             :initial-input initial-input
;;             :dynamic-collection t
;;             :history 'counsel-find-history
;;             :action (lambda (file)
;;                       (with-ivy-window
;;                         (when file
;;                           (find-file file))))
;;             :unwind #'counsel-delete-process
;;             :caller 'counsel-find))
;; FIX: (counsel-set-async-exit-code 'counsel-find 1 "Nothing found")

(use-package magit
  :ensure t
  :commands magit-get-top-dir
  :bind (("C-c g" . magit-status)
		 ("C-c C-g" . magit-file-log)
		 ("C-c f" . magit-grep))
  :init
  (progn
	(delete 'Git vc-handled-backends)

	(defadvice magit-status (around magit-fullscreen activate)
	  (window-configuration-to-register :magit-fullscreen)
	  ad-do-it
	  (delete-other-windows))
	
	(defadvice git-commit-commit (after delete-window activate)
	  (delete-window))

	(defadvice git-commit-abort  (after delete-window activate)
	  (delete-window))

	(defun magit-commit-mode-init ()
	  (when (looking-at "\n")
		(open-line 1)))

	(add-hook 'git-commit-mode-hook 'magit-commit-mode-init))
  :config
  (progn
	(defadvice magit-quit-window (around magit-restore-screen activate)
	  (let ((current-mode major-mode))
		ad-do-it
		(when (eq 'magit-status-mode current-mode)
		  (jump-to-register :magit-fullscreen))))

	(define-key magit-mode-map "c" 'magit-maybe-commit)

	(setq
	 magit-diff-refine-hunk t
	 magit-rewrite-incluseive 'ask
	 magit-set-upstream-on-push 'askifnotset)))

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


;; TODO: Replace Irony with cquery (/w ivy and company support of course)
(use-package irony
  :ensure t
  :config
  (progn
	(use-package company-irony
	  :ensure t
	  :config
	  (add-to-list 'company-backends 'company-irony))
	(add-hook 'c++-mode-hook 'irony-mode)
	(add-hook 'c-mode-hook 'irony-mode)
	(add-hook 'irony-mode-hook 'my-irony-mode-hook)
    (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)))

;; In order to start rtags server do following in terminal:
;; rdm
;; cd /path/to/project/root/
;; cmake . -DCMAKE_EXPORT_COMPILE_COMMANDS=1
;; rc -J .

;; TODO: Replacing rtags also with cquery
(use-package rtags
  :ensure t
  :config
  (progn
	(use-package company-rtags
	  :ensure t)
	(use-package flycheck-rtags
	  :ensure t)
	(setq rtags-completions-enabled t)
	(add-to-list 'company-backends 'company-rtags)
	(defun my-flycheck-rtags-setup ()
	  (flycheck-select-checker 'rtags)
	  ;; rtags create more accurate overlays.
	  (setq-local flycheck-highlighting-mode nil)
	  (setq-local flycheck-check-syntax-automatically nil))
	;; keybindings
	(define-key c-mode-base-map (kbd "M-.")
	  (function rtags-find-symbol-at-point))
	(define-key c-mode-base-map (kbd "M-,")
	  (function rtags-find-references-at-point))
	(rtags-enable-standard-keybindings)
	(setq rtags-autostart-diagnostics t)
	(rtags-diagnostics)
	(add-hook 'c-mode-common-hook #'my-flycheck-rtags-setup)
	(add-hook 'c++-mode-hook      #'my-flycheck-rtags-setup)))
(defun my-flycheck-rtags-setup ()
  "Tell flycheck to use rtags for checking."
  (flycheck-select-checker 'rtags)
  (setq-local flycheck-highlighting-mode nil)
  (setq-local flycheck-check-syntax-automatically nil))
(global-set-key (kbd "C-x p") 'rtags-peek-definition)

(use-package fic-mode
  :diminish fic-mode
  :config (add-hook 'prog-mode-hook 'fic-mode))

(use-package ace-window
  :commands ace-window
  :init (bind-key "C-x o" 'ace-window)
  :config (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package tex
  :defer t
  :ensure auctex
  :config (setq TeX-auto-save t))

(use-package elfeed
  :defer t
  :bind ("C-x w" . elfeed)
  :init (setf url-queue-timeout 30)
  :config
  (push "-k" elfeed-curl-extra-arguments)
  (setq-default elfeed-search-filter "@1-week-ago +unread")
  (add-hook 'elfeed-new-entry-hook
          (elfeed-make-tagger :before "2 weeks ago"
                              :remove 'unread)))

(setq elfeed-feeds
      '(("https://nullprogram.com/feed/" systems emacs general) 
        ("https://planet.emacsen.org/atom.xml" emacs)
		("https://utcc.utoronto.ca/~cks/space/blog/" unix general)
		("https://www.joelonsoftware.com/feed/")
		("http://feeds.feedburner.com/HighScalability")
		("https://blog.codinghorror.com/rss/")))

;; Local Variables:
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
(provide 'init)
;;; init ends here
