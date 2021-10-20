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
;; (setq debug-on-error t)

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

;; Install into separate package dirs for each Emacs version, to prevent bytecode incompatibility
(setq package-user-dir
      (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
                        user-emacs-directory))

;; Standard package repositories
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")))

(require 'package)
(package-initialize)

;; Setup `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Should set before loading `use-package'
(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-expand-minimally t)
  (setq use-package-compute-statistics t)
  (setq use-package-enable-imenu-support t))

(eval-when-compile (require 'use-package))

;; package.el updates the saved version of package-selected-packages correctly only
;; after custom-file has been loaded, which is a bug. We work around this by adding
;; the required packages to package-selected-packages after startup is complete.
;; Make `package-autoremove' work with `use-package'

(defvar use-package-selected-packages '(use-package)
  "Packages pulled in by use-package.")

(eval-and-compile
  (define-advice use-package-handler/:ensure (:around (fn name-symbol keyword args rest state) select)
    (let ((items (funcall fn name-symbol keyword args rest state)))
      (dolist (ensure args items)
        (let ((package
               (or (and (eq ensure t) (use-package-as-symbol name-symbol))
                   ensure)))
          (when package
            (when (consp package)
              (setq package (car package)))
            (push `(add-to-list 'use-package-selected-packages ',package) items)))))))

(when (fboundp 'package--save-selected-packages)
  (add-hook 'after-init-hook
            (lambda ()
              (package--save-selected-packages
               (seq-uniq (append use-package-selected-packages package-selected-packages))))))


;; Auto update packages
(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe))


(use-package diminish)

(use-package bind-key
  :bind ("C-h y" . describe-personal-keybindings))

;;; Elisp helper functions and commands

;; Like diminish, but for major modes
(defun sanityinc/set-major-mode-name (name)
  "Override the major mode NAME in this buffer."
  (setq-local mode-name name))

(defun sanityinc/major-mode-lighter (mode name)
  (add-hook (derived-mode-hook-name mode)
            (apply-partially 'sanityinc/set-major-mode-name name)))

;; Delete the current file

(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

;; Rename the current file

(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

;;; Scratch

(use-package scratch :defer t)

;;; Osx-key

(when *is-a-mac*
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none))

;;; Themes

(use-package color-theme-sanityinc-tomorrow
  :hook (after-init . reapply-themes)
  :bind ("C-c t b" . sanityinc-tomorrow-themes-toggle)
  :custom
  ;; Don't prompt to confirm theme safety. This avoids problems with
  ;; first-time startup on Emacs > 26.3.
  (custom-safe-themes t)
  ;; If you don't customize it, this is the theme you get.
  (custom-enabled-themes '(sanityinc-tomorrow-bright))
  :preface
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

  (defun sanityinc-tomorrow-themes-toggle ()
    "Toggle between `sanityinc-tomorrow-bright' and `sanityinc-tomorrow-day'."
    (interactive)
    (if (eq (car custom-enabled-themes) 'sanityinc-tomorrow-bright)
        (light)
      (dark))))


(use-package dimmer
  :hook (after-init . dimmer-mode)
  :config
  (setq-default dimmer-fraction 0.15)
  (advice-add 'frame-set-background-mode :after (lambda (&rest args) (dimmer-process-all)))  
  (add-to-list 'dimmer-exclusion-predicates 'sanityinc/display-non-graphic-p)
  :preface
  (defun sanityinc/display-non-graphic-p ()
    (not (display-graphic-p))))


;;; Configure FlyCheck global behavior

(use-package flycheck
  :hook (after-init . global-flycheck-mode)
  :config
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)

  (use-package flycheck-color-mode-line
    :hook (flycheck-mode . flycheck-color-mode-line-mode)
    :after flycheck))


;;; Dired mode

(use-package dired
  :ensure nil
  :config
  (setq-default dired-kill-when-opening-new-dired-buffer t)
  (setq dired-recursive-copies 'always))

(use-package diredfl
  :config
  (diredfl-global-mode 1))

;;; Show line number

(use-package display-line-numbers
  :ensure nil
  :hook (prog-mode . display-line-numbers-mode)
  :config
  (setq-default display-line-numbers-width 3))


;;; Minibuffer config

(use-package vertico
  :init
  (vertico-mode))

(use-package orderless
  :hook (minibuffer-setup . sanityinc/use-orderless-in-minibuffer)
  :config
  (setq completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion))))
  :preface
  (defun sanityinc/use-orderless-in-minibuffer ()
    (setq-local completion-styles '(substring orderless))))

;; A few more useful configurations...
(use-package emacs
  :ensure nil
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
  :diminish yas-minor-mode
  :init
  (use-package yasnippet-snippets :after yasnippet)
  :hook ((prog-mode org-mode) . yas-minor-mode)
  :bind
  (:map yas-minor-mode-map ("C-c C-n" . yas-expand-from-trigger-key))
  (:map yas-keymap
        (("TAB" . smarter-yas-expand-next-field)
         ([(tab)] . smarter-yas-expand-next-field)))
  :config
  (yas-reload-all)
  :preface
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
  :diminish
  :bind
  (:map company-active-map
        ("TAB" . smarter-tab-to-complete)
        ("<tab>" . smarter-tab-to-complete))
  :hook (after-init . global-company-mode)
  :preface
  (defun smarter-tab-to-complete ()
    "Try to `org-cycle', `yas-expand', and `yas-next-field' at current
 cursor position. If all failed, try to complete the common part with
 `company-complete-common'"
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
         ("M-g f" . consult-flycheck)               ;; Alternative: consult-flymake
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

(use-package consult-flycheck
  :defer t
  :after (consult flycheck))

(use-package marginalia :init (marginalia-mode))


;;; Settings for hippie-expand

(use-package hippie-exp
  :ensure nil
  :bind ("M-/" . hippie-expand)
  :config
  (setq hippie-expand-try-functions-list
        '(try-complete-file-name-partially
          try-complete-file-name
          try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill)))


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
 set-mark-command-repeat-pop t
 ;; truncate-lines nil
 truncate-partial-width-windows nil
 tooltip-delay 1.5)

(bind-key "C-x x p" 'pop-to-mark-command)

(add-hook 'after-init-hook 'delete-selection-mode)


(use-package autorevert
  :ensure nil
  :diminish
  :hook (after-init . global-auto-revert-mode)
  :config
  (setq global-auto-revert-non-file-buffers t
        auto-revert-verbose nil))

;; Huge files

(use-package so-long
  :ensure nil
  :hook (after-init . global-so-long-mode))

(use-package vlf
  :defer t
  :preface
  (defun ffap-vlf ()
    "Find file at point with VLF."
    (interactive)
    (require 'ffap)
    (let ((file (ffap-file-at-point)))
      (unless (file-exists-p file)
        (error "File does not exist: %s" file))
      (vlf file))))


;; A simple visible bell which works in all terminal types
(use-package mode-line-bell
  :hook (after-init . mode-line-bell-mode))

;; A light will shine on top of cursor when window scrolls
(use-package beacon
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
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package symbol-overlay
  :diminish
  :hook
  ((prog-mode html-mode yaml-mode conf-mode) . symbol-overlay-mode)
  :bind (:map symbol-overlay-mode
              ("M-i" . symbol-overlay-put)
              ("M-n" . symbol-overlay-jump-next)
              ("M-p" . symbol-overlay-jump-prev)))

(use-package move-dup
  :bind (
         ("C-c d" . move-dup-duplicate-down)
         ("C-c u" . move-dup-duplicate-up)
         ([M-up] . move-dup-move-lines-up)
         ([M-down] . move-dup-move-lines-down)))

;; Multiple cursors
(use-package multiple-cursors
  :bind (("C-<" . mc/mark-previous-like-this)
         ("C->" . mc/mark-next-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

;; Display available keybindings
(use-package which-key
  :diminish
  :hook (after-init . which-key-mode)
  :config
  (setq-default which-key-idle-delay 1.5))

;; Treat undo history as a tree
(use-package undo-tree
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


;;; Whitespace

(defun sanityinc/show-trailing-whitespace ()
  "Enable display of trailing whitespace in this buffer."
  (setq-local show-trailing-whitespace t))

(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook 'sanityinc/show-trailing-whitespace))

(bind-key [remap just-one-space] 'cycle-spacing)


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

(when *is-a-mac*
  (use-package ns-auto-titlebar
    :config
    (ns-auto-titlebar-mode 1)))

(when (and *is-a-mac* (fboundp 'toggle-frame-fullscreen))
  ;; Command-Option-f to toggle fullscreen mode
  ;; Hint: Customize `ns-use-native-fullscreen'
  (bind-key "M-ƒ" 'toggle-frame-fullscreen))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Change global font size easily
(use-package default-text-scale
  :bind (("C-M-=" . default-text-scale-increase)
         ("C-M--" . default-text-scale-decrease)
         ("C-M-0" . default-text-scale-reset)))


;;; Version control

(use-package magit
  :bind (("C-x g" . magit-status)))

(use-package diff-hl
  :bind (:map diff-hl-mode
              ("<left-fringe> <mouse-1>" . diff-hl-diff-goto-hunk))
  :hook
  (magit-post-refresh . diff-hl-magit-post-refresh)
  (after-init . global-diff-hl-mode)
  (dired-mode . diff-hl-dired-mode))


;;; Settings for tracking recent files

(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :config
  (setq-default
   recentf-max-saved-items 1000
   recentf-exclude `("/tmp/" "/ssh:" ,(concat package-user-dir "/.*-autoloads\\.el\\'"))))

;;; Ibuffer settings

(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer))


;;; Working with Windows within frames

;; Navigate window layouts with "M-N" and "M-P"
(use-package winner
  :ensure nil
  :defer 5
  :bind (("M-N" . winner-redo)
         ("M-P" . winner-undo))
  :config
  (winner-mode 1))

;; Make "C-x o" prompt for a target window when there are more than 2
(use-package switch-window
  :bind ("C-x o" . switch-window)
  :config
  (setq-default switch-window-shortcut-style 'alphabet)
  (setq-default switch-window-timeout nil))

;; When splitting window, show (other-buffer) in the new window
(defun split-window-func-with-other-buffer (split-function)
  (lambda (&optional arg)
    "Split this window and switch to the new window unless ARG is provided."
    (interactive "P")
    (funcall split-function)
    (let ((target-window (next-window)))
      (set-window-buffer target-window (other-buffer))
      (unless arg
        (select-window target-window)))))

(bind-key "C-x 2" (split-window-func-with-other-buffer 'split-window-vertically))
(bind-key "C-x 3" (split-window-func-with-other-buffer 'split-window-horizontally))

;; Rearrange split windows
(defun split-window-horizontally-instead ()
  "Kill any other windows and re-split such that the current window is on the top half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-horizontally)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

(defun split-window-vertically-instead ()
  "Kill any other windows and re-split such that the current window is on the left half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-vertically)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

(bind-key "C-x |" 'split-window-horizontally-instead)
(bind-key "C-x _" 'split-window-vertically-instead)

;; Borrowed from http://postmomentum.ch/blog/201304/blog-on-emacs
(defun sanityinc/split-window()
  "Split the window to see the most recent buffer in the other window.
Call a second time to restore the original window configuration."
  (interactive)
  (if (eq last-command 'sanityinc/split-window)
      (progn
        (jump-to-register :sanityinc/split-window)
        (setq this-command 'sanityinc/unsplit-window))
    (window-configuration-to-register :sanityinc/split-window)
    (switch-to-buffer-other-window nil)))

(bind-key "<f7>" 'sanityinc/split-window)

;; Toggle to dedicated window
(defun sanityinc/toggle-current-window-dedication ()
  "Toggle whether the current window is dedicated to its current buffer."
  (interactive)
  (let* ((window (selected-window))
         (was-dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not was-dedicated))
    (message "Window %sdedicated to %s"
             (if was-dedicated "no longer " "")
             (buffer-name))))

(bind-key "C-c <down>" 'sanityinc/toggle-current-window-dedication)

;;; Miscellaneous config

(defalias 'yes-or-no-p #'y-or-n-p)

(use-package goto-addr
  :ensure nil
  :hook (prog-mode . goto-address-prog-mode)
  :config
  (setq goto-address-mail-face 'link))

(use-package shift-number
  :bind (("C-c +" . shift-number-up)
         ("C-c -" . shift-number-down)))

;; Auto save
(use-package super-save
  :diminish
  :defer 0.5
  :config
  (add-to-list 'super-save-triggers 'switch-window)
  (setq super-save-idle-duration 1)
  (setq super-save-auto-save-when-idle t)
  (setq save-silently t)  
  (super-save-mode 1))

;; Rime

(use-package rime
  :if rime-usr-data-exists-p
  :bind (("C-\\" . toggle-input-method)
         ("C-`" . rime-send-keybinding)
         ([f8] . rime-toggle-show-candidate))
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
  :config
  ;; change cursor color automatically
  (use-package im-cursor-chg
    :ensure nil
    :after rime
    :config
    (cursor-chg-mode 1))
  :preface
  (defconst rime-usr-data-exists-p
    (file-exists-p "~/emacs-data/rime")
    "Checking if there is a rime user data.")
  
  (defun rime-toggle-show-candidate ()
    "Use minibuffer for candidate if current is nil."
    (interactive)
    (if (equal rime-show-candidate nil)
        (setq rime-show-candidate 'minibuffer)
      (setq rime-show-candidate nil))))


;;; Helpers for M-x compile

(use-package compile
  :ensure nil
  :bind ([f6] . recompile)
  :config
  (setq-default compilation-scroll-output t))


;;; Web configurations

;; SASS and SCSS mode
;; (use-package sass-mode :defer t)
(setq-default scss-compile-at-save nil)

;; LESS
(use-package skewer-less
  :hook (less-css-mode . skewer-less-mode))

;; JavaScript
(use-package js2-mode
  :mode "\\.js\\'"
  :init
  (setq-default js-indent-level 2)
  :config
  (add-to-list 'flycheck-disabled-checkers #'javascript-jshint)
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (with-eval-after-load 'js2-mode
    (sanityinc/major-mode-lighter 'js2-mode "JS2")
    (sanityinc/major-mode-lighter 'js2-jsx-mode "JSX2")))


;; JSON mode
(use-package json-mode
  :mode "\\.json\\'")

(use-package json-reformat
  :bind (:map json-mode-map
              ("C-c C-f" . json-reformat-region))
  :after json-mode)

(use-package json-snatcher
  :after json-mode)

;; Live browser JavaScript, CSS, and HTML interaction
(use-package skewer-mode
  :diminish
  :hook (((js-mode js2-mode). skewer-mode)
         (css-mode . skewer-css-mode)
         (web-mode . skewer-html-mode)
         (html-mode . skewer-html-mode)))

(use-package typescript-mode
  :mode ("\\.ts[x]\\'" . typescript-mode))

;; Major mode for editing web templates
(use-package web-mode
  :mode "\\.\\(phtml\\|php|[gj]sp\\|as[cp]x\\|erb\\|djhtml\\|html?\\|hbs\\|ejs\\|jade\\|swig\\|tm?pl\\|vue\\)$"
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))


;;; Programming languages support

;; C/C++ Mode
(use-package cc-mode
  :ensure nil
  :bind (:map c-mode-base-map
              ("C-c c" . compile))
  :hook (c-mode-common . (lambda () (c-set-style "stroustrup")))
  :init (setq-default c-basic-offset 4))

(use-package modern-cpp-font-lock
  :diminish modern-c++-font-lock-mode
  :hook (c++-mode . modern-c++-font-lock-mode))


;; Haskell mode
(use-package haskell-mode
  :bind (:map haskell-mode-map
              ("C-c C-f" . ormolu-buffer))
  :hook
  (haskell-mode . interactive-haskell-mode)
  (haskell-mode . haskell-indentation-mode)
  (haskell-mode . haskell-auto-insert-module-template))

(use-package dante
  :after (haskell-mode flycheck)
  :hook (haskell-mode . dante-mode)
  :config
  (flycheck-add-next-checker 'haskell-dante '(warning . haskell-hlint)))

(use-package reformatter
  :config
  (reformatter-define hindent
    :program "hindent"
    :lighter " Hin")

  (defalias 'hindent-mode 'hindent-on-save-mode)

  (reformatter-define ormolu
    :program "ormolu"
    :lighter " Orm"))

;; Lisp mode
(use-package paredit
  :diminish paredit-mode " Par"
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


;; Make C-x C-e run 'eval-region if the region is active

(use-package lisp-mode
  :ensure nil
  :bind (([remap eval-expression] . pp-eval-expression)
         :map emacs-lisp-mode-map
         ("C-x C-e" . sanityinc/eval-last-sexp-or-region)
         ("C-c C-e" . pp-eval-expression)
         ("C-c C-l" . sanityinc/load-this-file))
  :hook ((emacs-lisp-mode . (lambda () (setq mode-name "ELisp")))
         (emacs-lisp-mode . sanityinc/maybe-set-bundled-elisp-readonly))
  :config
  (setq-default debugger-bury-or-kill 'kill)
  (setq-default initial-scratch-message
                (concat ";; Happy hacking, " user-login-name " - Emacs ♥ you!\n\n"))

  ;; Load .el if newer than corresponding .elc

  (setq load-prefer-newer t)

  ;; Make C-x C-e run 'eval-region if the region is active
  (defun sanityinc/eval-last-sexp-or-region (prefix)
    "Eval region from BEG to END if active, otherwise the last sexp."
    (interactive "P")
    (if (and (mark) (use-region-p))
        (eval-region (min (point) (mark)) (max (point) (mark)))
      (pp-eval-last-sexp prefix)))

  (defun sanityinc/make-read-only (expression out-buffer-name)
    "Enable `view-mode' in the output buffer - if any - so it can be closed with `\"q\"."
    (when (get-buffer out-buffer-name)
      (with-current-buffer out-buffer-name
        (view-mode 1))))
  (advice-add 'pp-display-expression :after 'sanityinc/make-read-only)

  ;; C-c C-l to load buffer or file
  (defun sanityinc/load-this-file ()
    "Load the current file or buffer.
The current directory is temporarily added to `load-path'.  When
there is no current file, eval the current buffer."
    (interactive)
    (let ((load-path (cons default-directory load-path))
          (file (buffer-file-name)))
      (if file
          (progn
            (save-some-buffers nil (apply-partially 'derived-mode-p 'emacs-lisp-mode))
            (load-file (buffer-file-name))
            (message "Loaded %s" file))
        (eval-buffer)
        (message "Evaluated %s" (current-buffer)))))

  (defun sanityinc/maybe-set-bundled-elisp-readonly ()
    "If this elisp appears to be part of Emacs, then disallow editing."
    (when (and (buffer-file-name)
               (string-match-p "\\.el\\.gz\\'" (buffer-file-name)))
      (setq buffer-read-only t)
      (view-mode 1)))

  ;;respawn the scratch buffer when it's killed
  (use-package immortal-scratch
    :after lisp-mode
    :hook (after-init . immortal-scratch-mode))

  ;; Extras for theme editing
  (use-package highlight-quoted
    :hook (emacs-lisp-mode . highlight-quoted-mode)))

(use-package rainbow-mode
  :diminish
  :hook ((emacs-lisp-mode . sanityinc/enable-rainbow-mode-if-theme)
         (help-mode . rainbow-mode))
  :preface
  (defun sanityinc/enable-rainbow-mode-if-theme ()
    (when (and (buffer-file-name) (string-match-p "\\(color-theme-\\|-theme\\.el\\)" (buffer-file-name)))
      (rainbow-mode))))

(use-package macrostep
  :bind ("C-c e m" . macrostep-expand))

(use-package ert
  :ensure nil
  :bind (:map ert-results-mode-map
              ("g" . ert-results-rerun-all-tests)))

(use-package flycheck-package
  :after (flycheck elisp-mode)
  :config
  (flycheck-package-setup))

(use-package flycheck-relint
  :defer t
  :after (flycheck elisp-mode))


;; Enable desired features for all lisp modes

(defun set-up-hippie-expand-for-elisp ()
  "Locally set `hippie-expand' completion functions for use with Emacs Lisp."
  (make-local-variable 'hippie-expand-try-functions-list)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol t)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol-partially t))

(defun sanityinc/enable-check-parens-on-save ()
  "Run `check-parens' when the current buffer is saved."
  (add-hook 'after-save-hook #'check-parens nil t))

(defvar sanityinc/lispy-modes-hook
  '(enable-paredit-mode
    sanityinc/enable-check-parens-on-save)
  "Hook run in all Lisp modes.")

(use-package aggressive-indent
  :config
  (add-to-list 'sanityinc/lispy-modes-hook 'aggressive-indent-mode))

(defun sanityinc/lisp-setup ()
  "Enable features useful in any Lisp mode."
  (run-hooks 'sanityinc/lispy-modes-hook))

(defun sanityinc/emacs-lisp-setup ()
  "Enable features useful when working with elisp."
  (set-up-hippie-expand-for-elisp))

(defconst sanityinc/elispy-modes
  '(emacs-lisp-mode ielm-mode)
  "Major modes relating to elisp.")

(defconst sanityinc/lispy-modes
  (append sanityinc/elispy-modes
          '(lisp-mode inferior-lisp-mode lisp-interaction-mode))
  "All lispy major modes.")

(require 'derived)

(dolist (hook (mapcar #'derived-mode-hook-name sanityinc/lispy-modes))
  (add-hook hook 'sanityinc/lisp-setup))

(dolist (hook (mapcar #'derived-mode-hook-name sanityinc/elispy-modes))
  (add-hook hook 'sanityinc/emacs-lisp-setup))

(when (boundp 'eval-expression-minibuffer-setup-hook)
  (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode))

(add-to-list 'auto-mode-alist '("\\.emacs-project\\'" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("archive-contents\\'" . emacs-lisp-mode))


;; Rust mode
(use-package rustic
  :mode ("\\.rs\\'" . rustic-mode)
  :bind (:map rustic-mode-map
              ("C-c C-f" . rustic-format-buffer))
  :config
  (setq rustic-lsp-client 'eglot)
  (with-eval-after-load 'flycheck
    (push 'rustic-clippy flycheck-checkers)))

;; Lua mode
(use-package lua-mode
  :mode "\\.lua\\'")

;; Basic support for programming in J
(use-package j-mode
  :defer t
  :hook (inferior-j-mode . (lambda () (electric-pair-mode -1)))
  :config
  (setq-default j-console-cmd "jconsole"))

;; Markdown support
(use-package markdown-mode
  :mode (("\\.md\\.html\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode)))

;; Support Yaml files
(use-package yaml-mode
  :mode "\\.ya?ml\\'"
  :hook (yaml-mode . goto-address-prog-mode))

;; Support for the Nix package manager
(use-package nix-mode
  :mode "\\.nix\\'")

(use-package nixpkgs-fmt
  :after nix-mode
  :bind (:map nix-mode-map
              ("C-c C-f" . nixpkgs-fmt)))

;;Support MSCL mode
(use-package mscl-mode
  :ensure nil
  :mode "\\.pwx?macro\\'")


;;; LSP

(use-package eglot
  :defer t
  :bind (:map eglot-mode-map
              ("C-c l a" . eglot-code-actions)
              ("C-c l r" . eglot-rename)
              ("C-c l f" . eglot-format)
              ("C-c l d" . eldoc))
  :hook (eglot-managed-mode . (lambda () (flymake-mode -1)))
  :config
  (add-to-list 'eglot-server-programs '(rust-mode "rust-analyzer"))
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))

  (setq read-process-output-max (* 1024 1024))
  (push :documentHighlightProvider eglot-ignored-server-capabilities)
  (setq eldoc-echo-area-use-multiline-p nil))


;;; Configuration for quickrun

(use-package quickrun
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
  :ensure nil
  :bind (("C-c a" . org-agenda)
         ("C-c x" . org-capture)
         :map org-mode-map
         ("C-c i a" . org-id-get-create)
         ("C-c e d" . org-export-docx))
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

  :preface
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
  :hook (org-mode . prose-mode)
  :preface
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
(when *is-a-mac*
  (use-package osx-dictionary
    :bind (("C-c t i" . osx-dictionary-search-input)
           ("C-c t x" . osx-dictionary-search-pointer))))

(use-package fanyi
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
  :ensure nil
  :if (and (executable-find "aspell") *spell-check-support-enabled*)
  ;; Add spell-checking in comments for all programming language modes
  :hook ((prog-mode . flyspell-prog-mode)
         (flyspell-mode . (lambda ()
                            (dolist (key '("C-;" "C-."))
                              (unbind-key key flyspell-mode-map)))))
  :init
  (setq flyspell-issue-message-flag nil
        ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=fast" "--lang=en_US" "--camel-case")
        ispell-personal-dictionary
        (expand-file-name "en_US.personal" "~/.config/aspell/")))

;; Correcting words with flyspell via completing-read
(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map ("C-," . flyspell-correct-wrapper)))

;;; Font

(defvar font-list
  (cond
   ((eq system-type 'darwin)
    '(("SF Mono" . 130) ("Monaco" . 130) ("Menlo" . 130)))
   ((eq system-type 'windows-nt)
    '(("SF Mono" . 110) ("Consolas" . 120) ("Cascadia Mono" . 110)))
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

  (dolist (font '("Segoe UI Symbol" "Apple Color Emoji" "Noto Color Emoji"))
    (if (font-installed-p font)
        (set-fontset-font t 'unicode font nil 'prepend)))

  (dolist (font '("Microsoft Yahei" "Hiragino Sans GB" "Noto Sans Mono CJK SC"))
    (if (font-installed-p font)
        (set-fontset-font t '(#x4e00 . #x9fff) font))))


;;; Save and restore editor sessions between restarts

;; Show Emacs init time
(defun sanityinc/time-subtract-millis (b a)
  (* 1000.0 (float-time (time-subtract b a))))

(defun sanityinc/show-init-time ()
  (message "init completed in %.2fms"
           (sanityinc/time-subtract-millis after-init-time before-init-time)))

(add-hook 'after-init-hook 'sanityinc/show-init-time)

;; Save a list of open files in ~/.emacs.d/.emacs.desktop
(use-package desktop
  :ensure nil
  :config
  (setq desktop-path (list user-emacs-directory)
        desktop-auto-save-timeout 600)
  
  (advice-add 'desktop-read :around 'sanityinc/desktop-time-restore)
  (advice-add 'desktop-create-buffer :around 'sanityinc/desktop-time-buffer-create)

  ;; Save a bunch of variables to the desktop file
  ;; for lists specify the len of the maximal saved data also
  (setq desktop-globals-to-save
        '((comint-input-ring        . 50)
          (compile-history          . 30)
          desktop-missing-file-warning
          (dired-regexp-history     . 20)
          (extended-command-history . 30)
          (face-name-history        . 20)
          (file-name-history        . 100)
          (grep-find-history        . 30)
          (grep-history             . 30)
          (magit-revision-history   . 50)
          (minibuffer-history       . 50)
          (org-clock-history        . 50)
          (org-refile-history       . 50)
          (org-tags-history         . 50)
          (query-replace-history    . 60)
          (read-expression-history  . 60)
          (regexp-history           . 60)
          (regexp-search-ring       . 20)
          register-alist
          (search-ring              . 20)
          (shell-command-history    . 50)
          tags-file-name
          tags-table-list))

  (desktop-save-mode 1)
  :preface
  (defun sanityinc/desktop-time-restore (orig &rest args)
    (let ((start-time (current-time)))
      (prog1
          (apply orig args)
        (message "Desktop restored in %.2fms"
                 (sanityinc/time-subtract-millis (current-time)
                                                 start-time)))))
  
  (defun sanityinc/desktop-time-buffer-create (orig ver filename &rest args)
    (let ((start-time (current-time)))
      (prog1
          (apply orig ver filename args)
        (message "Desktop: %.2fms to restore %s"
                 (sanityinc/time-subtract-millis (current-time)
                                                 start-time)
                 (when filename
                   (abbreviate-file-name filename)))))))

;; Restore histories and registers after saving

(use-package savehist
  :ensure nil
  :init
  (savehist-mode))

(use-package session
  :hook (after-init . session-initialize)
  :config
  (setq session-save-file (locate-user-emacs-file ".session"))
  (setq session-name-disable-regexp "\\(?:\\`'/tmp\\|\\.git/[A-Z_]+\\'\\)")
  (setq session-save-file-coding-system 'utf-8))


;;; Allow access from emacsclient

(use-package server
  :ensure nil
  :config
  (unless (server-running-p)
    (server-start)))

;;; Variables configured via the interactive 'customize' interface

(when (file-exists-p custom-file)
  (load custom-file))


;;; Configure default locale

(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(unless (eq system-type 'windows-nt)
  (set-selection-coding-system 'utf-8))



(provide 'init)
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
