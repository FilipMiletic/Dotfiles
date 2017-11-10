(require 'package)
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
(require 'diminish)
(require 'bind-key)


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
              left-fringe-width 8)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
(hl-line-mode       1)
(menu-bar-mode     -1)
(tool-bar-mode     -1)
(scroll-bar-mode   -1)
(show-paren-mode    1)
(line-number-mode   1)
(column-number-mode 1)
(blink-cursor-mode  0)
(global-auto-revert-mode 1)
(set-frame-font "Source Code Pro Medium 12")
(setq mac-option-modifier nil
      mac-command-modifier 'meta
      load-prefer-newer t
      gc-cons-threshold 100000000
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
      mac-allow-anti-aliasing t
      frame-resize-pixelwise t)

(setq make-backup-files nil)
(setq initial-frame-alist
      '((width . 120)
        (height . 68)))
(defalias 'yes-or-no-p 'y-or-n-p)

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t))

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
         ("C-c k"   . counsel-ag)
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

	(defun magit-maybe-commit (&optional show-options)
	  "Runs magit-commit unless prefix is passed"
	  (interactive "P")
	  (if show-options
		  (magit-key-mode-popup-comitting)
		(magit-commit)))
	
	(define-key magit-mode-map "c" 'magit-maybe-commit)

	(use-package rebase-mode)

	(setq
	 magit-diff-refine-hunk t
	 magit-rewrite-incluseive 'ask
	 magit-set-upstream-on-push 'askifnotset
	 )))

(use-package evil
  :init
  (progn
    ;; if we don't have this evil overwrites the cursor color
    (setq evil-default-cursor t)

    ;; leader shortcuts

    ;; This has to be before we invoke evil-mode due to:
    ;; https://github.com/cofi/evil-leader/issues/10
    (use-package evil-leader
      :init (global-evil-leader-mode)
      :config
      (progn
        ;; (setq evil-leader/in-all-states t)
		(setq evil-leader/leader ",")
        ;; keyboard shortcuts
        (evil-leader/set-key
          "a" 'ag-project
          "A" 'ag
          "b" 'ivy-switch-buffer
          "f" 'counsel-find-file
          "g" 'magit-status
          "k" 'kill-buffer
          "K" 'kill-this-buffer
          "o" 'occur
          "p" 'magit-find-file-completing-read
          "t" 'bw-open-term
          "T" 'eshell
          "w" 'save-buffer
          "x" 'smex
          )))

    ;; boot evil by default
    (evil-mode 1))
  :config
  (progn
    ;; use ido to open files
    (define-key evil-ex-map "e " 'counsel-find-file)
    (define-key evil-ex-map "b " 'ivy-switch-buffer)

    ;; jj escapes to normal mode
    (define-key evil-insert-state-map (kbd "j") 'bw-evil-escape-if-next-char-is-j)
    (setq
     ;; h/l wrap around to next lines
     evil-cross-lines t

)

    ;; esc should always quit: http://stackoverflow.com/a/10166400/61435
    (define-key evil-normal-state-map [escape] 'keyboard-quit)
    (define-key evil-visual-state-map [escape] 'keyboard-quit)
    (define-key minibuffer-local-map [escape] 'abort-recursive-edit)
    (define-key minibuffer-local-ns-map [escape] 'abort-recursive-edit)
    (define-key minibuffer-local-completion-map [escape] 'abort-recursive-edit)
    (define-key minibuffer-local-must-match-map [escape] 'abort-recursive-edit)
    (define-key minibuffer-local-isearch-map [escape] 'abort-recursive-edit)

    ;; modes to map to different default states
    (dolist (mode-map '((comint-mode . emacs)
                        (term-mode . emacs)
                        (eshell-mode . emacs)
                        (help-mode . emacs)
                        (fundamental-mode . emacs)))
      (evil-set-initial-state `,(car mode-map) `,(cdr mode-map)))))


(require 'tramp)
