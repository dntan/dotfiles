;; Dannys windows basic config
;; UPDATE 5/10/25

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

;; THEMES
;; review the themes here https://github.com/doomemacs/themes/tree/screenshots
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-dracula t))

;;ORGMODE

(require 'org-tempo)
(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(add-hook 'org-mode-hook #'yas-minor-mode)


;; Collapse headers
(setq org-ellipsis " ▾")

;; which-key, when you hit a keychord it shows you options.
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
   '(cdlatex company-auctex company-box company-math doom-themes
	     geiser-racket laas lsp-ui magit org-bullets org-roam
	     racket-mode rainbow-delimiters sicp smartparens
	     yasnippet-snippets)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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

;; ORG-ROAM, Second brain
;; This is for a zettlekasten approach to notetaking
;; I have ideas of running a ML algo on my org-roam-directory, to auto add tags and backlinks

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-directory (file-truename "~/dt_2b"))
  :custom
  (org-roam-capture-templates
   '(("w" "Website" plain
      "* Notes\n** Fleeting notes\n%?\n\n* Suggested backlinks\n:BEGIN_AUTOBACKLINKS:\n:END_AUTOBACKLINKS:\n"
      :if-new (file+head "websites/${slug}.org"
                         "#+title: ${title}\n#+filetags: :website:\n#+created: %U\n\n- Source: ${url}\n")
      :unnarrowed t)

     ("y" "YouTube" plain
      "* Notes\n** Fleeting notes\n%?\n\n* Suggested backlinks\n:BEGIN_AUTOBACKLINKS:\n:END_AUTOBACKLINKS:\n"
      :if-new (file+head "youtube/${slug}.org"
                         "#+title: ${title}\n#+filetags: :youtube:video:\n#+created: %U\n\n- URL: ${url}\n")
      :unnarrowed t)

     ("z" "Zotero PDF" plain
      "* Reference Notes\n** Fleeting notes\n%?\n\n* Suggested backlinks\n:BEGIN_AUTOBACKLINKS:\n:END_AUTOBACKLINKS:\n"
      :if-new (file+head "zotero/${citekey}.org"
                         "#+title: ${title}\n#+filetags: :zotero:pdf:article:\n#+created: %U\n\n- Citation: [cite:@${citekey}]\n- PDF: ${file}\n")
      :unnarrowed t)))

  (org-roam-dailies-capture-templates
 '(("d" "default" entry
    "* Daily notes\n%?\n\n* Suggested backlinks\n:BEGIN_AUTOBACKLINKS:\n:END_AUTOBACKLINKS:\n"
    :if-new (file+head "%<%Y-%m-%d>.org"
                       "#+title: %<%Y-%m-%d>\n#+created: %U\n"))))



  (org-roam-completion-everywhere t)
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n g" . org-roam-graph)
         ("C-c n c" . org-roam-capture)
         ("C-c n d" . org-roam-dailies-goto-today))
  :config
  (org-roam-db-autosync-mode))


;; Need to set up capture templates here, so that I can save time creating templates from scratch.


;; PROGRAMMING
;;Language Servers
(use-package lsp-mode
  :commands (lsp lsp--buffer-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

;; Following advice from emacs elements I'm going to try out company mode first...

(use-package company
  :ensure t
  :hook ((prog-mode . company-mode)
         (org-mode  . company-mode)
         (LaTeX-mode . company-mode))   ;; enable in LaTeX buffers too
  :bind (:map company-active-map
              ("<return>" . nil)
              ("RET" . nil)
              ("C-<return>" . company-complete-selection)
              ([tab] . company-complete-selection)
              ("TAB" . company-complete-selection)))

;; LaTeX completions in tex files via AUCTeX
(use-package company-auctex
  :after (company tex)
  :config
  (company-auctex-init))

;; company-box is the visual popup for completions
  (use-package company-box
    :ensure t
    :hook (company-mode . company-box-mode))
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0.1)
(setq company-minimum-prefix-length 3)
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

;; SICP Racket specific:
(require 'ob-racket)

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


;; ================================
;;  LaTeX + Org-mode Math Workflow
;; ================================

;; --- AUCTeX ---
;; AUCTeX setup without calc integration
(use-package auctex
  :ensure t
  :hook ((LaTeX-mode . prettify-symbols-mode)))

;; --- CDLaTeX ---
(use-package cdlatex
  :ensure t
  :hook ((LaTeX-mode . turn-on-cdlatex)
         (org-mode   . turn-on-org-cdlatex))
  :bind (:map cdlatex-mode-map
              ("<tab>" . cdlatex-tab)))

;; --- YASnippet ---
(use-package yasnippet
  :ensure t
  :hook ((LaTeX-mode . yas-minor-mode)
         (org-mode   . yas-minor-mode)
         (post-self-insert . my/yas-try-expanding-auto-snippets))
  :config
  (setq yas-triggers-in-field t)

  ;; Auto-expand snippets when possible
  (defun my/yas-try-expanding-auto-snippets ()
    "Auto-expand snippets when possible."
    (when (and (boundp 'yas-minor-mode) yas-minor-mode)
      (let ((yas-buffer-local-condition
             '(require-snippet-condition . auto)))
        (yas-expand))))

  ;; TAB integration between Yasnippet, CDLaTeX, and Company
  (bind-keys :map yas-keymap
             ("<tab>"     . yas-next-field-or-cdlatex-or-company)
             ("TAB"       . yas-next-field-or-cdlatex-or-company)
             ("<backtab>" . yas-prev-field)) ;; Shift-TAB to go backward

  ;; Allow cdlatex-tab to work inside Yas fields
  (defun cdlatex-in-yas-field ()
    "Allow `cdlatex-tab` to work inside Yas fields."
    (when-let* ((_   (overlayp yas--active-field-overlay))
                (end (overlay-end yas--active-field-overlay)))
      (if (>= (point) end)
          (let ((s (thing-at-point 'sexp)))
            (unless (and s (assoc (substring-no-properties s)
                                  cdlatex-command-alist-comb))
              (yas-next-field-or-maybe-expand)
              t))
        (let (cdlatex-tab-hook minp)
          (setq minp (min (save-excursion
                            (cdlatex-tab)
                            (point))
                          (overlay-end yas--active-field-overlay)))
          (goto-char minp)
          t))))

  ;; Smart TAB dispatcher
  (defun yas-next-field-or-cdlatex-or-company ()
    "Smart TAB: prefer Yas fields, then CDLaTeX, then Company."
    (interactive)
    (cond
     ;; 1. If inside a yasnippet field, jump forward
     ((and (boundp 'yas-minor-mode) yas-minor-mode
           (overlayp yas--active-field-overlay))
      (yas-next-field-or-maybe-expand))

     ;; 2. If CDLaTeX is active and can expand
     ((and (bound-and-true-p cdlatex-mode)
           (cdlatex-in-yas-field)))

     ;; 3. Otherwise, trigger Company completion manually
     ((and (boundp 'company-mode) company-mode
           (company-manual-begin))
      t)

     ;; 4. Fallback: just indent
     (t (indent-for-tab-command)))))


;; --- Org-mode LaTeX Preview Scaling ---

;; Making previewing latex in org easier! rather than C-c C-x C-l
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-;") #'org-latex-preview))

(setq org-preview-latex-default-process 'dvisvgm)

(setq org-latex-packages-alist '(("" "tikz")))

(defun my/text-scale-adjust-latex-previews ()
  "Adjust LaTeX preview size when text is scaled."
  (pcase major-mode
    ('latex-mode
     (dolist (ov (overlays-in (point-min) (point-max)))
       (when (eq (overlay-get ov 'category) 'preview-overlay)
         (my/text-scale--resize-fragment ov))))
    ('org-mode
     (dolist (ov (overlays-in (point-min) (point-max)))
       (when (eq (overlay-get ov 'org-overlay-type) 'org-latex-overlay)
         (my/text-scale--resize-fragment ov))))))

(defun my/text-scale--resize-fragment (ov)
  (let* ((img-props (cdr (overlay-get ov 'display)))
         (base-scale (or (plist-get img-props :scale) 1.0)))
    (overlay-put
     ov 'display
     (cons 'image
           (plist-put img-props
                      :scale (* base-scale
                                (+ 1.0 (* 0.25 text-scale-mode-amount))))))))

(add-hook 'text-scale-mode-hook #'my/text-scale-adjust-latex-previews)

(add-hook 'org-mode-hook
          (lambda ()
            (setq org-format-latex-options
                  '(:foreground default :background default :scale 1.6
                    :html-foreground "Black" :html-background "Transparent"
                    :html-scale 1.0 :matchers ("begin" "$1" "$" "$$" "\\(" "\

\[")))))


