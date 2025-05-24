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

(provide 'nh-helpers)
;;; nh-helpers.el ends here 