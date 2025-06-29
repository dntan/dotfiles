;; Dannys windows basics config

;; Set default zoom to 128

(set-face-attribute 'default nil :font "DejaVuSansM Nerd Font Mono" :height 130)

;; Save find history
(savehist-mode +1)
(setq savehist-additional-variables '(kill-ring search ring regexp-search-ring))


;; Set custom text to the scratch buffer
(setq initial-scratch-message
      ";; Hello Danny! what are you focusing on today?")

;; I can inhibit the start up screen if I really want
(setq inhibit-startup-screen t)

;; Set the default dir
(setq default-directory "C:/User/Danny")

;;Package dir
(add-to-list 'load-path' "C:/User/Danny/.emacs.d/packages")


;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			   ("org" . "https://orgmode.org/elpa/")
			   ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)


;; minimal basics emacs, all optional
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(column-number-mode +1)
(global-goto-address-mode +1)
(global-visual-line-mode +1) ;; This is usually what I want to happen! -------------------------->
(delete-selection-mode +1) ;;I want to highlight then typing should replace that text.
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

(recentf-mode +1) ;; look up recently opened files.
(global-set-key "\C-x\ \C-r" 'recentf-open-files) ;;C-x C-r, q to quit.
(global-hl-line-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)


;; UTF-8
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)

;; Backups
;; URL: https://sachachua.com/dotemacs/index.html
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;; Quickly access dot emacs d
(global-set-key (kbd "C-c e")
    (lambda()
      (interactive)
      (find-file "~/.emacs.d")))

;; Global keys, Trying out emacs elements suggestions see how it goes.

(setq recenter-positions '(top middle bottom))
;;(global-set-key (kbd "C-1") 'kill-this-buffer) ;;This did not seem to work.
(global-set-key (kbd "C-<down>") (kbd "C-u 1 C-v"))
(global-set-key (kbd "C-<up>") (kbd "C-u 1 M-v"))
(global-set-key [C-tab] 'other-window)
(global-set-key (kbd "C-c c") 'calendar)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-/") #'hippie-expand)
(global-set-key (kbd "C-x C-j") 'dired-jump)
(global-set-key (kbd "C-c r") 'remember)

;; dired
(setq dired-listing-switches "-alt --dired --group-directories-first -h -G")

(add-hook 'dired-mode-hook 'dired-hide-details-mode)

(add-hook 'dired-mode-hook (lambda () (dired-omit-mode)))

;; I-search
(setq case-fold-search t) ;; C-s for I search in document. case-insensitive.

(setq sentence-end-double-space nil)

;; org-mode

(require 'org-tempo)
(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; Collapse headers

(setq org-ellipsis " ▾")

;; Try to use which-key?
(use-package which-key
:init (which-key-mode)
:diminish which-key-mode
:config
(setq which-key-idle-delay 0.3))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098"
     default))
 '(package-selected-packages '(doom-themes magit org-bullets sicp)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; Loading in a dark theme.
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-dracula t))

;;(load-theme 'tango-dark)

;;What about transparency

;; (add-to-list 'default-frame-alist '(alpha-background . 90)) ;; doesn't work on windows.

;; requirements for programming in orgmode


(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))   ;; Python support using python-shell-interpreter

;; More org-babel stuff, making it better to work with python:
(setq org-babel-default-header-args:python '((:results . "output")))

(global-set-key (kbd "C-c p s") #'run-python)


;;  ignore the prompt when eval some source code blocks

(defun my-org-confirm-babel-evaluate (lang body)
  (not (string= lang "python")))  ;don't ask for ditaa
(setq org-confirm-babel-evaluate #'my-org-confirm-babel-evaluate)


