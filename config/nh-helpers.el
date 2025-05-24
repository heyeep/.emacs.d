;;; nh-helpers.el --- Helper functions and utilities -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for helper functions and utility packages.

;;; Code:

(defun nh/load-directory (dir)
  "Load all .el files from DIR."
  (add-to-list 'load-path dir)
  (dolist (file (directory-files dir t "\.el$"))
    (when (not (string-match-p "\`\." (file-name-nondirectory file)))
      (require (intern (file-name-base file))))))

;; Indentation helpers: quickly indent buffer or region, clean up whitespace, and untabify if needed
(defun nh/indent-buffer ()
  "Indent the currently visited buffer, remove trailing whitespace, and untabify if needed."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (unless indent-tabs-mode
    (untabify (point-min) (point-max))))

(defun nh/indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer. Cleans up whitespace and untabifies if needed."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (unless indent-tabs-mode
            (untabify (point-min) (point-max)))
          (message "Indented selected region."))
      (progn
        (nh/indent-buffer)
        (message "Indented buffer.")))))

;; Helper: Return a list of major modes for various Lisp dialects and REPLs
(defun nh/lisp-modes ()
  "Return a list of major mode symbols for common Lisp dialects and REPLs. Useful for batch operations on all Lisp modes."
  '(lisp-mode
    lisp-interaction-mode
    emacs-lisp-mode
    common-lisp-mode
    slime-mode
    clojure-mode
    cider-mode
    cider-repl-mode
    scheme-mode
    geiser-mode
    geiser-repl-mode))

;; Helper: Given a mode symbol, return its hook symbol
(defun nh/mode-hook (mode)
  "Return the hook symbol for a given MODE symbol."
  (intern (concat (symbol-name mode) "-hook")))

;; Helper: Return a list of all Lisp mode hook symbols
(defun nh/lisp-hooks ()
  "Return a list of hook symbols for all Lisp-related modes."
  (mapcar #'nh/mode-hook (nh/lisp-modes)))

(provide 'nh-helpers)
;;; nh-helpers.el ends here 