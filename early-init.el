;;; early-init.el --- Emacs 27+ pre-initialisation config

;;; Commentary:

;; Emacs 27+ loads this file before (normally) calling
;; `package-initialize'.  We use this file to suppress that automatic
;; behaviour so that startup is consistent across Emacs versions.

;;; Code:

(setq package-enable-at-startup nil)
(setq org-modules-loaded t)
(setq frame-inhibit-implied-resize t)

;; Set default font before frame creation to make sure the first frame have the correct size
(defvar font-list
  (cond
   ((eq system-type 'darwin)
    '(("SF Mono" . 13) ("Monaco" . 13) ("Menlo" . 13)))
   ((eq system-type 'windows-nt)
    '(("SF Mono" . 11) ("Consolas" . 12) ("Cascadia Mono" . 11)))
   (t
    '(("SF Mono" . 11) ("Consolas" . 12) ("Cascadia Mono" . 11))))
  "List of fonts and sizes.  The first one available will be used.")

(add-to-list 'default-frame-alist (cons 'font (format "%s-%d" (caar font-list) (cdar font-list))))


;; So we can detect this having been loaded
(provide 'early-init)

;;; early-init.el ends here
