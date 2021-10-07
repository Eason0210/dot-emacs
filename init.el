;;; init.el --- Emacs init.el                        -*- lexical-binding: t; -*-

;; Copyright (C) 2021, 2022 Eason Huang

;; Author: Eason Huang <aqua0210@163.com>
;; Keywords: local

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; My personal Emacs configuration.

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
(setq debug-on-error t)

(let ((minver "26.2"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "28.0.50")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(defconst *spell-check-support-enabled* t) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))

;; Adjust garbage collection thresholds during startup, and thereafter

(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; Bootstrap config

(setq custom-file (locate-user-emacs-file "custom.el"))


;;; package.el & use-package setup

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(require 'package)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(custom-set-variables
 '(use-package-enable-imenu-support t))

(eval-when-compile (require 'use-package))

(use-package diminish :ensure t)

(use-package bind-key
  :bind ("C-h y" . describe-personal-keybindings))


;;; Scratch

(use-package scratch :ensure t)
(setq-default initial-scratch-message
              (concat ";; Happy hacking, " user-login-name " - Emacs ♥ you!\n\n"))

;;; Osx-key

(when *is-a-mac*
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none))

;;; Themes

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :init
  ;; Ensure that themes will be applied even if they have not been customized
  (defun reapply-themes ()
    "Forcibly load the themes listed in `custom-enabled-themes'."
    (dolist (theme custom-enabled-themes)
      (unless (custom-theme-p theme)
        (load-theme theme)))
    (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

  ;; Toggle between light and dark
  (defun light ()
    "Activate a light color theme."
    (interactive)
    (setq custom-enabled-themes '(sanityinc-tomorrow-day))
    (reapply-themes))

  (defun dark ()
    "Activate a dark color theme."
    (interactive)
    (setq custom-enabled-themes '(sanityinc-tomorrow-bright))
    (reapply-themes))

  :config
  ;; Don't prompt to confirm theme safety. This avoids problems with
  ;; first-time startup on Emacs > 26.3.
  (setq custom-safe-themes t)
  ;; If you don't customize it, this is the theme you get.  
  (setq-default custom-enabled-themes '(sanityinc-tomorrow-bright))
  (add-hook 'after-init-hook 'reapply-themes))


(use-package dimmer
  :ensure t
  :init
  (defun sanityinc/display-non-graphic-p ()
    (not (display-graphic-p)))
  (setq-default dimmer-fraction 0.15)
  :config
  (advice-add 'frame-set-background-mode :after (lambda (&rest args)
						  (dimmer-process-all)))
  (add-to-list 'dimmer-exclusion-predicates 'sanityinc/display-non-graphic-p))


;;; Dired mode

(use-package dired
  :demand t
  :init
  (setq-default dired-kill-when-opening-new-dired-buffer t)
  :config
  (setq dired-recursive-copies 'always))

(use-package diredfl
  :ensure t
  :init
  (diredfl-global-mode))

;;; Show line number

(use-package display-line-numbers
  :hook (prog-mode . display-line-numbers-mode)
  :config
  (setq-default display-line-numbers-width 3))


;;; Minibuffer config

(use-package vertico
  :ensure t
  :demand t
  :init
  (vertico-mode))

(use-package orderless
  :ensure t
  :init
  (defun aqua/use-orderless-in-minibuffer ()
    (setq-local completion-styles '(substring orderless)))
  (add-hook 'minibuffer-setup-hook 'aqua/use-orderless-in-minibuffer)
  
  (setq completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  (setq enable-recursive-minibuffers t))


;; YASnippet
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :init
  (use-package yasnippet-snippets :ensure t :after yasnippet)
  :hook ((prog-mode org-mode) . yas-minor-mode)
  :bind
  (:map yas-minor-mode-map ("C-c C-n" . yas-expand-from-trigger-key))
  (:map yas-keymap
        (("TAB" . smarter-yas-expand-next-field)
         ([(tab)] . smarter-yas-expand-next-field)))
  :config
  (yas-reload-all)
  (defun smarter-yas-expand-next-field ()
    "Try to `yas-expand' then `yas-next-field' at current cursor position."
    (interactive)
    (let ((old-point (point))
          (old-tick (buffer-chars-modified-tick)))
      (yas-expand)
      (when (and (eq old-point (point))
                 (eq old-tick (buffer-chars-modified-tick)))
        (ignore-errors (yas-next-field))))))

;; Completion with company
(use-package company
  :ensure t
  :diminish
  :bind
  (:map company-active-map
        ("TAB" . smarter-tab-to-complete)
        ("<tab>" . smarter-tab-to-complete))
  :hook (after-init . global-company-mode)
  :config
  (defun smarter-tab-to-complete ()
    "Try to `org-cycle', `yas-expand', and `yas-next-field' at current cursor position.
If all failed, try to complete the common part with `company-complete-common'"
    (interactive)
    (when yas-minor-mode
      (let ((old-point (point))
            (old-tick (buffer-chars-modified-tick))
            (func-list
             (if (equal major-mode 'org-mode) '(org-cycle yas-expand yas-next-field)
               '(yas-expand yas-next-field))))
        (catch 'func-suceed
          (dolist (func func-list)
            (ignore-errors (call-interactively func))
            (unless (and (eq old-point (point))
                         (eq old-tick (buffer-chars-modified-tick)))
              (throw 'func-suceed t)))
          (company-complete-common))))))


(use-package consult
  :ensure t
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c b" . consult-bookmark)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s F" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch)
         :map isearch-mode-map
         ("M-e" . consult-isearch)                 ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch)               ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi))           ;; needed by consult-line to detect isearch

  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-file consult--source-project-file consult--source-bookmark
   :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")


  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project))))))

(use-package marginalia
  :ensure t
  :demand t
  :config
  (marginalia-mode 1)
  (setq marginalia-annotators '(marginalia-annotators-heavy
                                marginalia-annotators-light)))

;;; Editing utils

(when (fboundp 'electric-pair-mode)
  (add-hook 'after-init-hook 'electric-pair-mode))
(add-hook 'after-init-hook 'electric-indent-mode)

;; Show matching parens
(add-hook 'after-init-hook 'show-paren-mode)

(setq-default
 blink-cursor-interval 0.4
 bookmark-default-file (locate-user-emacs-file ".bookmarks.el")
 buffers-menu-max-size 30
 column-number-mode t
 indent-tabs-mode nil
 create-lockfiles nil
 auto-save-default nil
 make-backup-files nil
 mouse-yank-at-point t
 save-interprogram-paste-before-kill t
 scroll-preserve-screen-position 'always
 ;; set-mark-command-repeat-pop t
 ;; truncate-lines nil
 truncate-partial-width-windows nil
 tooltip-delay 1.5)

(add-hook 'after-init-hook 'delete-selection-mode)


(use-package autorevert
  :diminish
  :hook (after-init . global-auto-revert-mode)
  :config
  (setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil))

;; A simple visible bell which works in all terminal types
(use-package mode-line-bell
  :ensure t
  :hook (after-init . mode-line-bell-mode))

;; A light will shine on top of cursor when window scrolls
(use-package beacon
  :ensure t
  :init
  (setq-default beacon-lighter "")
  (setq-default beacon-size 20)
  :config
  (beacon-mode 1))

;; Display buffer boundaries and fill column indicator
(when (boundp 'display-fill-column-indicator)
  (setq-default indicate-buffer-boundaries 'left)
  (setq-default display-fill-column-indicator-character ?\u254e)
  (add-hook 'prog-mode-hook 'display-fill-column-indicator-mode))


(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package symbol-overlay
  :diminish
  :ensure t
  :hook
  ((prog-mode html-mode yaml-mode conf-mode) . symbol-overlay-mode)
  :bind (:map symbol-overlay-mode
              ("M-i" . symbol-overlay-put)        
              ("M-n" . symbol-overlay-jump-next)
              ("M-p" . symbol-overlay-jump-prev)))

(use-package move-dup
  :ensure t
  :bind (
         ("C-c d" . move-dup-duplicate-down)
         ("C-c u" . move-dup-duplicate-up)
         ([M-up] . move-dup-move-lines-up)
         ([M-down] . move-dup-move-lines-down)))

;; Display available keybindings
(use-package which-key
  :diminish
  :ensure t
  :hook (after-init . which-key-mode)
  :config
  (setq-default which-key-idle-delay 1.5))

;; Treat undo history as a tree
(use-package undo-tree
  :ensure t
  :diminish
  :hook (after-init . global-undo-tree-mode)
  :init
  (setq undo-tree-visualizer-timestamps t
        undo-tree-enable-undo-in-region nil
        undo-tree-auto-save-history nil)

  ;; HACK: keep the diff window
  (with-no-warnings
    (make-variable-buffer-local 'undo-tree-visualizer-diff)
    (setq-default undo-tree-visualizer-diff t)))


;;; GUI frames

;; Suppress GUI features

(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)

;; Window size and features

(setq-default
 window-resize-pixelwise t
 frame-resize-pixelwise t)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

(menu-bar-mode -1)

(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

(use-package ns-auto-titlebar
  :if *is-a-mac*
  :ensure t
  :config
  (ns-auto-titlebar-mode 1))
  

;;; Version control

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package diff-hl
  :ensure t
  :bind (:map diff-hl-mode
              ("<left-fringe> <mouse-1>" . diff-hl-diff-goto-hunk))
  :hook
  (magit-post-refresh . diff-hl-magit-post-refresh)
  (after-init . global-diff-hl-mode)
  (dired-mode . diff-hl-dired-mode))


;;; Settings for tracking recent files

(use-package recentf
  :hook (after-init . recentf-mode)
  :config
  (setq-default
   recentf-max-saved-items 1000
   recentf-exclude `("/tmp/" "/ssh:" ,(concat package-user-dir "/.*-autoloads\\.el\\'"))))

;;; Ibuffer settings

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))


;;; Miscellaneous config

(defalias 'yes-or-no-p #'y-or-n-p)

(add-hook 'prog-mode-hook 'goto-address-prog-mode)
(setq goto-address-mail-face 'link)

;; Auto save
(use-package auto-save
  :config
  (setq auto-save-silent t)
  (setq auto-save-idle 1.5)
  (auto-save-enable))

;; Rime
(defconst rime-usr-data-exists-p
  (file-exists-p "~/emacs-data/rime")
  "Checking if there is a rime user data.")

(use-package rime
  :if rime-usr-data-exists-p
  :ensure t
  :bind (("C-\\" . toggle-input-method)
         ("C-`" . rime-send-keybinding))
  :init

  (setq
   rime-inline-predicates '(rime-predicate-space-after-cc-p
                            rime-predicate-current-uppercase-letter-p)
   rime-translate-keybindings '("C-f" "C-b" "C-n" "C-p" "C-g")
   rime-inline-ascii-trigger 'shift-r
   rime-inline-ascii-holder ?a
   default-input-method "rime"
   rime-cursor "|"
   rime-show-candidate nil
   window-min-height 1
   rime-user-data-dir "~/emacs-data/rime"
   rime-title "")

  (when (eq system-type 'windows-nt)
    (setq rime-share-data-dir
          "~/scoop/apps/msys2/current/mingw64/share/rime-data"))
  (when *is-a-mac*
    (setq rime-librime-root  "~/emacs-data/librime/dist")
    (setq rime-emacs-module-header-root "~/.nix-profile/include"))

  (defun rime-toggle-show-candidate ()
    "Use minibuffer for candidate if current is nil."
    (interactive)
    (if (equal rime-show-candidate nil)
        (setq rime-show-candidate 'minibuffer)
      (setq rime-show-candidate nil)))

  ;; change cursor color automatically
  (use-package im-cursor-chg
    :after (rime)
    :config
    (cursor-chg-mode 1)))


;;; Programming languages support

;; C/C++ Mode

(use-package cc-mode
  :bind (:map c-mode-base-map
              ("C-c c" . compile))
  :hook (c-mode-common . (lambda () (c-set-style "stroustrup")))
  :init (setq-default c-basic-offset 4))

(use-package modern-cpp-font-lock
  :ensure t
  :init
  (modern-c++-font-lock-global-mode +1))

(use-package cmake-mode :ensure t)
(use-package cmake-font-lock :ensure t)

;; Haskell mode

(use-package haskell-mode
  :ensure t
  :hook
  (haskell-mode . interactive-haskell-mode)
  (haskell-mode . turn-on-haskell-indentation))

;; Lisp mode

(use-package paredit
  :ensure t
  :diminish
  :hook ((lisp-mode emacs-lisp-mode) . paredit-mode)
  :bind (:map paredit-mode-map
              ("[")
              ;; ("M-k"   . paredit-raise-sexp)
              ("M-I"   . paredit-splice-sexp)
              ;; ("C-M-l" . paredit-recentre-on-sexp)
              ("C-c ( n"   . paredit-add-to-next-list)
              ("C-c ( p"   . paredit-add-to-previous-list)
              ("C-c ( j"   . paredit-join-with-next-list)
              ("C-c ( J"   . paredit-join-with-previous-list))
  :bind (:map lisp-mode-map       ("<return>" . paredit-newline))
  :bind (:map emacs-lisp-mode-map ("<return>" . paredit-newline))
  :hook (paredit-mode
         . (lambda ()
             (unbind-key [M-up] paredit-mode-map)
             (unbind-key [M-down] paredit-mode-map)
             (unbind-key "M-r" paredit-mode-map)
             (unbind-key "M-s" paredit-mode-map)))
  :config
  (require 'eldoc)
  (eldoc-add-command 'paredit-backward-delete
                     'paredit-close-round))

;; Rust mode

(use-package rust-mode :ensure t)

;; Support Yaml files

(use-package yaml-mode
  :ensure t
  :hook (yaml-mode . goto-address-prog-mode))

;; Support for the Nix package manager

(use-package nix-mode :ensure t)
(use-package nixpkgs-fmt :after (nix-mode))

;;Support MSCL mode

(use-package mscl-mode :mode "\\.pwx?macro\\'")


;;; LSP

(use-package eglot
  :ensure t
  :bind (:map eglot-mode-map              
              ("C-c l a" . 'eglot-code-actions)         
              ("C-c l r" . 'eglot-rename)
              ("C-c l f" . 'eglot-format)
              ("C-c l d" . 'eldoc))
  :config
  (add-to-list 'eglot-server-programs '(rust-mode "rust-analyzer"))
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd")))


;;; Configuration for quickrun

(use-package quickrun
  :ensure t
  :bind (("<f5>" . quickrun)
         ("C-<f5>" . quickrun-shell))
  :config
  (quickrun-add-command "c++/c1z"
    '((:command . "g++")
      (:exec    . ("%c -std=c++1z %o -o %e %s"
		   "%e %a"))
      (:remove  . ("%e")))
    :default "c++"))


;;; Org configurations

(use-package org
  :bind (("C-c a" . org-agenda)
         ("C-c x" . org-capture)
         :map org-mode-map
         ("C-c i a" . org-id-get-create))
  :config
  ;; To speed up startup, don't put to init section
  (setq org-hide-emphasis-markers t)

  ;; Babel
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t)

  (org-babel-do-load-languages
   'org-babel-load-languages
   `((C . t)
     (calc . t)
     (dot . t)
     (emacs-lisp . t)
     (haskell . t)
     (python . t)
     (sql . t)
     (sqlite . t)))

  ;; Export to docx
  (defun org-export-docx ()
    (interactive)
    (let ((docx-file (concat (file-name-sans-extension (buffer-file-name)) ".docx"))
          (template-file (expand-file-name "template/template.docx"
                                           user-emacs-directory)))
      (shell-command (format "pandoc %s -o %s --reference-doc=%s"
                             (buffer-file-name)
                             docx-file
                             template-file))
      (message "Convert finish: %s" docx-file))))


;; Writing mode similar to the famous Writeroom editor for OS X
(use-package writeroom-mode
  :ensure t
  :hook (org-mode . prose-mode)
  :config
  (define-minor-mode prose-mode
    "Set up a buffer for prose editing.
This enables or modifies a number of settings so that the
experience of editing prose is a little more like that of a
typical word processor."
    :init-value nil :lighter " Prose" :keymap nil
    (if prose-mode
        (progn
          (when (fboundp 'writeroom-mode)
            (writeroom-mode 1))
          (setq truncate-lines nil)
          (setq word-wrap t)
          (setq word-wrap-by-category t)
          (setq cursor-type 'bar)
          (when (eq major-mode 'org)
            (kill-local-variable 'buffer-face-mode-face))
          (buffer-face-mode 1)
          ;;(delete-selection-mode 1)
          (setq-local blink-cursor-interval 0.6)
          (setq-local show-trailing-whitespace nil)
          (setq-local line-spacing 0.2)
          (setq-local electric-pair-mode nil)
          (ignore-errors (flyspell-mode 1))
          (visual-line-mode 1))
      (kill-local-variable 'truncate-lines)
      (kill-local-variable 'word-wrap)
      (kill-local-variable 'word-wrap-by-category)
      (kill-local-variable 'cursor-type)
      (kill-local-variable 'blink-cursor-interval)
      (kill-local-variable 'show-trailing-whitespace)
      (kill-local-variable 'line-spacing)
      (kill-local-variable 'electric-pair-mode)
      (buffer-face-mode -1)
      ;; (delete-selection-mode -1)
      (flyspell-mode -1)
      (visual-line-mode -1)
      (when (fboundp 'writeroom-mode)
        (writeroom-mode 0)))))


;; Roam

(when (and (executable-find "sqlite3") (executable-find "cc"))
  (use-package org-roam
    :ensure t
    :diminish
    :hook (after-init . org-roam-db-autosync-enable)
    :bind (("C-c n l" . org-roam-buffer-toggle)
           ("C-c n f" . org-roam-node-find)
           ("C-c n g" . org-roam-graph)
           ("C-c n i" . org-roam-node-insert)
           ("C-c n c" . org-roam-capture)
           ("C-c n j" . org-roam-dailies-capture-today)
           ("C-c n r" . org-roam-ref-find))
    :init
    (setq org-roam-directory (file-truename "~/.org/org-roam")
          org-roam-db-location "~/.org/org-roam.db"
          org-roam-db-gc-threshold most-positive-fixnum
          org-roam-v2-ack t)
    :config
    (unless (file-exists-p org-roam-directory)
      (make-directory org-roam-directory t))
    
    (add-to-list 'display-buffer-alist
                 '("\\*org-roam\\*"
                   (display-buffer-in-direction)
                   (direction . right)
                   (window-width . 0.33)
                   (window-height . fit-window-to-buffer)))))


;;; Dictionaries

(use-package osx-dictionary
  :if *is-a-mac*
  :ensure t
  :bind (("C-c t i" . osx-dictionary-search-input)
         ("C-c t x" . osx-dictionary-search-pointer)))


(use-package fanyi
  :ensure t
  :bind (("C-c t f" . fanyi-dwim)
         ("C-c t d" . fanyi-dwim2))
  :config
  (setq fanyi-haici-chart-inhibit-same-window t)
  :custom
  (fanyi-providers '(fanyi-haici-provider
                     fanyi-youdao-thesaurus-provider
                     fanyi-etymon-provider
                     fanyi-longman-provider)))


;;; Spell check settings

(use-package flyspell
  :diminish
  :if (and (executable-find "aspell") *spell-check-support-enabled*)
  ;; Add spell-checking in comments for all programming language modes
  :hook ((prog-mode . flyspell-prog-mode)
         (flyspell-mode . (lambda ()
                            (dolist (key '("C-;" "C-."))
                              (unbind-key key flyspell-mode-map)))))
  :init (setq flyspell-issue-message-flag nil
              ispell-program-name "aspell"
              ispell-extra-args '("--sug-mode=fast" "--lang=en_US" "--camel-case")
              ispell-personal-dictionary
              (expand-file-name "en_US.personal" "~/.config/aspell/"))
  :config
  ;; Correcting words with flyspell via completing-read
  (use-package flyspell-correct
    :ensure t
    :after flyspell
    :bind (:map flyspell-mode-map ("C-," . flyspell-correct-wrapper))))


;;; Font

(defvar font-list
  (cond
   ((eq system-type 'darwin)
    '(("SF Mono" . 130) ("Monaco" . 130) ("Menlo" . 130)))
   ((eq system-type 'windows-nt)
    '(("SF Mono" . 120) ("Consolas" . 140) ("Cascadia Mono" . 125)))
   (t
    '(("SF Mono" . 190) ("Consolas" . 200) ("Cascadia Mono" . 190))))
  "List of fonts and sizes.  The first one available will be used.")

(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(defun change-font ()
  "Set English font from the `font-list'."
  (interactive)
  (let* (available-fonts font-name font-size)
    (dolist (font font-list
                  (setq available-fonts (nreverse available-fonts)))
      (when (font-installed-p (car font))
        (push font available-fonts)))
    (if (not available-fonts)
        (message "No fonts from the chosen set are available")
      (if (called-interactively-p 'interactive)
          (let* ((chosen (assoc-string
                          (completing-read "What font to use? "
                                           available-fonts nil t)
                          available-fonts)))
            (setq font-name (car chosen)
                  font-size (read-number "Font size: " (cdr chosen))))
        (setq font-name (caar available-fonts)
              font-size (cdar available-fonts)))
      (set-face-attribute 'default nil :font font-name :height font-size))))

(when (display-graphic-p)
  (change-font)

  (dolist (font '("Segoe UI Symbol" "Apple Color Emoji" "Symbola" "Symbol"))
    (if (font-installed-p font)
        (set-fontset-font t 'unicode font nil 'prepend)))
  
  (dolist (font '("Microsoft Yahei" "Hiragino Sans GB" "Noto Sans Mono CJK SC"))
    (if (font-installed-p font)
       (set-fontset-font t '(#x4e00 . #x9fff) font))))

  
;;; Allow access from emacsclient

(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))


;;; Variables configured via the interactive 'customize' interface

(when (file-exists-p custom-file)
  (load custom-file))



(provide 'init)
;;; init.el ends here
