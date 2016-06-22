(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes nil)
 '(custom-safe-themes
   (quote
	("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(fringe-mode 0 nil (fringe))
 '(linum-format " %d ")
 '(ns-use-native-fullscreen t)
 '(package-selected-packages
   (quote
	(smooth-scrolling magit evil-leader alchemist zerodark-theme spaceline ## atom-one-dark-theme)))
 '(show-paren-delay 0.0)
 '(sml/theme (quote respectful))
 '(spacemacs-theme-comment-bg nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(setq user-full-name "Filip Miletic")
(setq user-mail-adress "filip.miletic@me.com")

;; disable splash screen
(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'text-mode)

;; disable cocoa gui 
(scroll-bar-mode -1)
(tool-bar-mode -1)
(global-hl-line-mode 1)
;; (menu-bar-mode -1)

;; selection mode modified
(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

;; enable line numbers
(global-linum-mode t)
;; alias for yes and no, so I don't have to type whole answer
(defalias 'yes-or-no-p 'y-or-n-p)

;; disable backups and autosaves
(setq backup-inhibited t)
(setq auto-save-default nil)
(setq make-backup-files nil)

;; show matching brackets
(show-paren-mode t)

;; melpa and marmalade package configs
(load "package")
(package-initialize)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(setq package-archive-enable-alist '(("melpa" deft magit)))

;; tab width
(setq-default tab-width 4)
(setq-default indent-tabs-mode t)

;; defualt font Menlo Park 12
(set-default-font "Menlo 12")

;; theme setting
(load-theme 'molokai t)
;(load-theme 'zerodark t)

;; enabling EVIL mode
(require 'evil)
(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key
  "e" 'find-file
  "b" 'switch-to-buffer
  "k" 'kill-buffer)
(evil-mode t)


;; smart mode line setup
(require 'spaceline-config)
(spaceline-spacemacs-theme)

;; disable 3D emacs drawing of objects
(set-face-attribute 'mode-line nil :box nil)

;; c style
(setq c-default-style "k&r"
	  c-basic-offset 4
	  indent-tabs-mode t)

;; silence bell
(setq ring-bell-function 'ignore)

;; pretify simbols
(global-prettify-symbols-mode)

;; smooth scrolling
(setq scroll-margin 0)
(setq scroll-conservatively 10000)
(setq scroll-preserve-screen-position t)


;; if a file is changed outside of Emacs, load those changes
(global-auto-revert-mode t)

;; code colapsing
(add-hook 'prog-mode-hook (lambda () (hs-minor-mode t)))

;; disable minibuffer GC
(defun my-minibuffer-setup-hook()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;; emacs startup time reducer
(defun loadup-gen ()
  (interactive)
  (defun get-loads-from-*Messages* ()
	(save-excursion
	  (let ((retval ()))
		(set-buffer "*Messages*")
		(beginning-of-buffer)
		(while (search-forward-regexp "^Loading " nil t)
		  (let ((start (point)))
			(search-forward "...")
			(backward-char 3)
			(setq retval (cons (buffer-substring-no-properties start (point)) retval))))
		retval)))
  (map 'list
	   (lambda (file) (princ (format "(load \"%s\")\n" file)))
	   (get-loads-from-*Messages*)))

;;; esc quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

