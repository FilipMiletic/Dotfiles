(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes )
 '(fringe-mode 2 nil (fringe))
 '(linum-format " %d ")
 '(ns-use-native-fullscreen t)
 '(package-selected-packages
   (quote
	(molokai-theme color-theme-sanityinc-tomorrow atom-one-dark-theme)))
 '(show-paren-delay 0.0)
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
(load-theme 'spolsky t)

;; configure and enable default Spaceline theme
(load "spaceline-config")
(require 'spaceline-config)
(spaceline-spacemacs-theme)

;; enabling EVIL mode
(require 'evil)
(evil-mode t)
;;(goto-last-change t)
;;(goto-last-change-reverse t)


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

(defun go-mode-setup()
  (go-eldoc-setup)
  (setq compile-command "go build -v && go test -v && go vet")
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save) )
(add-hook 'go-mode-hook 'go-mode-setup)

(require 'auto-complete-config)
(require 'auto-complete-clang)
(require 'go-autocomplete)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
