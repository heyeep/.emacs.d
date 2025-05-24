;;; nh-helpers.el --- Helper functions and utilities -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for helper functions and utility packages.

;;; Code:

(defun nh-load-directory (dir)
  "Load all .el files from DIR."
  (add-to-list 'load-path dir)
  (dolist (file (directory-files dir t "\\.el$"))
    (when (not (string-match-p "\\`\\." (file-name-nondirectory file)))
      (require (intern (file-name-base file))))))

(provide 'nh-helpers)
;;; nh-helpers.el ends here 