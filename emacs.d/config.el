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

;;This is Danny's emacs config file as init.el

           (setq inhibit-startup-message t)

           (scroll-bar-mode -1)        ; Disable visible scrollbar
           (tool-bar-mode -1)          ; Disable the toolbar
           (tooltip-mode -1)           ; Disable tooltips
           (set-fringe-mode 10)        ; Give some breathing room

           (menu-bar-mode -1)            ; Disable the menu bar

           (savehist-mode +1)
           (setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))

           ;; Set up the visible bell
           (setq visible-bell t)

(column-number-mode)

;; trying out relative line numbers in the emacsclient

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

;; Disable line numbers for some modes
       (dolist (mode '(
                       term-mode-hook
                       eshell-mode-hook))
         (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; word wrap in buffers 
(global-visual-line-mode 1)

(setq backup-directory-alist '(("." . "~/.emacs.d/backups"))) ;; This is helpful to keep files neat and backups all in one centralised place
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

(set-face-attribute 'default nil :font "DejaVu Sans" :height 130)

;; Download Evil
  (unless (package-installed-p 'evil)
    (package-install 'evil))

  (setq evil-want-keybinding nil)  
  ;; Enable Evil
  (require 'evil)
  (evil-mode 1)

(use-package evil-collection
  :ensure t
  :after evil
  :init
  (evil-collection-init))

;;some elisp code go here

(setq org-agenda-files
        '("~/Documents/tasks.org"))

(require 'org-tempo) ;;this orgmode feature is a must <s tab brings up a src code block!

(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; Collapse headers

(setq org-ellipsis " ▾")

(setq org-format-latex-options (plist-put org-format-latex-options :scale 2.5))

(use-package magit
:ensure t)

(load-theme 'tango-dark)

(add-to-list 'default-frame-alist '(alpha-background . 90)) ; For all new frames henceforth

(use-package which-key
:init (which-key-mode)
:diminish which-key-mode
:config
(setq which-key-idle-delay 0.3))
