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
(setq default-directory "C:/Users/Danny")

;;Package dir
(add-to-list 'load-path' "C:/Users/Danny/.emacs.d/packages")


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
(blink-cursor-mode 1)
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

;; Try to use which-key?  This is already a part of emacs30, read emacs news to find out more.
;; (use-package which-key
;; :init (which-key-mode)
;; :diminish which-key-mode
;; :config
;; (setq which-key-idle-delay 0.3))


;; Themes for emacs

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("8d3ef5ff6273f2a552152c7febc40eabca26bae05bd12bc85062e2dc224cde9a"
     "5c8a1b64431e03387348270f50470f64e28dfae0084d33108c33a81c1e126ad6"
     "9d5124bef86c2348d7d4774ca384ae7b6027ff7f6eb3c401378e298ce605f83a"
     "87fa3605a6501f9b90d337ed4d832213155e3a2e36a512984f83e847102a42f4"
     "e8bd9bbf6506afca133125b0be48b1f033b1c8647c628652ab7a2fe065c10ef0"
     "5244ba0273a952a536e07abaad1fdf7c90d7ebb3647f36269c23bfd1cf20b0b8"
     "dfcd2b13f10da4e5e26eb1281611e43a134d4400b06661445e7cbb183c47d2ec"
     "8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098"
     default))
 '(package-selected-packages
   '(auctex company company-box doom-themes geiser-racket lsp-mode magit
	    org-bullets racket-mode rainbow-delimiters sicp
	    smartparens yasnippet yasnippet-snippets)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; Loading in a dark theme.
;; review the themes here https://github.com/doomemacs/themes/tree/screenshots
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-dracula t))

;;(load-theme 'tango-dark)

;;What about transparency

;; (add-to-list 'default-frame-alist '(alpha-background . 90)) ;; doesn't work on windows.

;; SICP Racket specific:
;; download the ob-racket package manually from github as it doesn't exist on melpa

(require 'ob-racket)

;; requirements for programming in orgmode

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (scheme . t)
   (C . t)
   (racket . t)))   ;; Python support using python-shell-interpreter

;; More org-babel stuff, making it better to work with python:
(setq org-babel-default-header-args:python '((:results . "output")))

(global-set-key (kbd "C-c p s") #'run-python)


;;  ignore the prompt when eval some source code blocks

(defun my-org-confirm-babel-evaluate (lang body)
  (not (string= lang "python")))  ;don't ask for ditaa
(setq org-confirm-babel-evaluate #'my-org-confirm-babel-evaluate)


;; Language Servers
(use-package lsp-mode
  :commands (lsp lsp--buffer-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

;; Following advice from emacs elements I'm going to try out company mode first...

(use-package company
    :ensure t
    :hook ((prog-mode . company-mode))
    :bind (:map company-active-map
                ("<return>" . nil)
                ("RET" . nil)
                ("C-<return>" . company-complete-selection)
                ([tab] . company-complete-selection)
                ("TAB" . company-complete-selection)))
  (use-package company-box
    :ensure t
    :hook (company-mode . company-box-mode))
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0.2)
(setq company-minimum-prefix-length 1)
(setq company-tooltip-align-annotations t)

;; ;; Language support:
(add-hook 'python-mode-hook #'lsp)
;; (add-hook 'c-mode-hook 'company-mode)
;; (add-hook 'scheme-mode-hook 'company-mode)
;; (add-hook 'geiser-repl-mode-hook 'company-mode)
(add-hook 'emacs-lisp-mode-hook 'company-mode)
(add-hook 'org-mode-hook 'company-mode)
;; for orgmode as it has not backend
(setq company-backends '((company-capf company-dabbrev)))

;; lsp-mode:
(require 'lsp-ui)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(setq lsp-enable-symbol-highlighting t
      lsp-ui-sideline-show-hover t
      lsp-ui-doc-enable t)


;; Scheme Racket REPL
(require 'geiser-racket)

;; pairs of parens
(use-package smartparens
  :ensure smartparens  ;; install the package
  :hook (prog-mode text-mode markdown-mode) ;; add `smartparens-mode` to these hooks
  :config
  ;; load default config
  (require 'smartparens-config))
(add-hook 'geiser-repl-mode-hook #'smartparens-mode)

;; rainbow-delimiters colours based on nesting.
(use-package rainbow-delimiters
  :ensure t
  :hook ((prog-mode . rainbow-delimiters-mode)
         (geiser-repl-mode . rainbow-delimiters-mode)))

;; A world of LaTeX with AUCTeX

(use-package auctex
  :ensure t)

;; From Karthinks site.. lets try that out.
(setq org-preview-latex-default-process 'dvisvgm)
(defun my/text-scale-adjust-latex-previews ()
  "Adjust the size of latex preview fragments when changing the
buffer's text scale."
  (pcase major-mode
    ('latex-mode
     (dolist (ov (overlays-in (point-min) (point-max)))
       (if (eq (overlay-get ov 'category)
               'preview-overlay)
           (my/text-scale--resize-fragment ov))))
    ('org-mode
     (dolist (ov (overlays-in (point-min) (point-max)))
       (if (eq (overlay-get ov 'org-overlay-type)
               'org-latex-overlay)
           (my/text-scale--resize-fragment ov))))))

(defun my/text-scale--resize-fragment (ov)
  (overlay-put
   ov 'display
   (cons 'image
         (plist-put
          (cdr (overlay-get ov 'display))
          :scale (+ 1.0 (* 0.25 text-scale-mode-amount))))))

(add-hook 'text-scale-mode-hook #'my/text-scale-adjust-latex-previews)

;; LaTeX fragments in org files: Make them large.

(add-hook 'org-mode-hook
          (lambda ()
            (setq org-format-latex-options
                  '(:foreground default :background default :scale 1.6
                    :html-foreground "Black" :html-background "Transparent"
                    :html-scale 1.0 :matchers ("begin" "$1" "$" "$$" "\\(" "\

\[")))
            (org-latex-preview)))
