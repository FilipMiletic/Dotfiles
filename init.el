;;; package -- Summary
;;; Commentary:
;;; Code:
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


(unless package-archive-contents
  (package-refresh-contents))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/custom"))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
(menu-bar-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(line-number-mode 1)
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
      scroll-step 1
      scroll-conservatively 100000
      scroll-margin 8
      fringes-outside-margins 1
      ns-pop-up-frames nil
      make-backup-files nil
      auto-save-default nil
      create-lockfiles nil
      sentence-end-double-space nil
      cursor-type 'box
      mac-allow-anti-aliasing t
      frame-resize-pixelwise t)
;; initial window
(setq initial-frame-alist
      '(
        (width . 115)
        (height . 65)
        ))

(setq-default indent-tabs-mode t
              indent-line-function 4
              tab-width 4
              c-basic-offset 4
              fill-column 80
              left-fringe-width 8)

(defalias 'yes-or-no-p 'y-or-n-p)

;; Don't close Emacs via GUI command
(defadvice handle-delete-frame (around my-handle-delete-frame-advice activate)
  "Hide Emacs instead of closing the last frame."
  (let ((frame (posn-window (event-start event)))
		  (numfrs (length(frame-list))))
	(if (> numfrs 1)
		  ad-do-it
	  (do-applescript "tell application \"System Events\" to tell process \"Emacs\" to set visible to false"))))

(set-frame-font "Fira Code 11")

;; Theme changer for changing theme depending on time
;; (setq calendar-location-name "Belgrade")
;; (setq calendar-latitude 44.78)
;; (setq calendar-longitude 20.44)
;; (require 'theme-changer)
;; (change-theme 'tao-yang 'tao-yin)

(use-package kaolin-theme
  :ensure t
  :config (load-theme 'kaolin t))

(use-package flx-ido
  :ensure t
  :config (setq ido-use-faces nil))

(use-package paredit
  :ensure t
  :diminish paredit-mode
  :config
  (add-hook 'emacs-lisp-mode-hook                  #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook                        #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook                        #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook            #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook                      #'enable-paredit-mode))

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
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package paredit-everywhere
  :ensure t
  :diminish paredit-everywhere-mode
  :config
  (add-hook 'prog-mode-hook #'paredit-everywhere-mode))

(use-package avy
  :ensure t
  :bind ("C-x ;" . avy-goto-char))

(use-package ace-window
  :ensure t
  :bind ("M-p" . ace-window))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package ibuffer
  :config (setq ibuffer-expert t)
  :bind ("C-x C-b" . ibuffer))

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
         (setq ivy-height 6)
         (setq enable-recursive-minibuffers t)
         (setq swiper-include-line-number-in-search t)
         (setq ivy-re-builders-alist
               '((counsel-M-x . ivy--regex-fuzzy)
                 (t . ivy--regex-plus)))

         (define-key ivy-minibuffer-map (kbd "TAB") 'ivy-partial-or-done)))

(use-package counsel
  :after (ivy)
  :demand
  :bind (("M-x"     . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-h f"   . counsel-describe-function)
         ("C-h v"   . counsel-describe-variable)
         ("C-c f r" . counsel-recentf)
         ("C-c g"   . counsel-git)
         ("C-c /"   . counsel-git-grep)
         ;; ("C-c k"   . counsel-ag)
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

(use-package magit
  :ensure t
  :config
  (progn
    (setq magit-push-always-verify nil)
    (setq magit-diff-refine-hunk t))
  :bind
  ("C-c g" . magit-status)
  ("C-c C-a" . magit-commit-amend))

(use-package ido
  :ensure t
  :config
  (progn
    (ido-mode t)
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
        company-idle-delay .1
        company-echo-delay 0
        company-tooltip-flip-when-above t
        company-minimum-prefix-length 2
        company-tooltip-limit 12)
  (global-company-mode 1))

(use-package markdown-mode
  :mode ("\\.\\(m\\(ark\\)?down\\|md\\)$" . markdown-mode)
  :config)

(use-package diminish
  :ensure t)

;; Enable mouse support
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (global-set-key [mouse-4] '(lambda ()
                              (interactive)
                              (scroll-down 1)))
  (global-set-key [mouse-5] '(lambda ()
                              (interactive)
                              (scroll-up 1)))
  (defun track-mouse (e)))

(provide 'init)
;; init.el ends here
