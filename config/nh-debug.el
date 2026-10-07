;;; nh-debug.el --- debug -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun nh/toggle-debug-on-error ()
  "Toggle debug on error."
  (interactive)
  (setq debug-on-error (not debug-on-error))
  (message "Debug on error %s" (if debug-on-error "enabled" "disabled")))

(setq debug-on-quit nil)

(provide 'nh-debug)
;;; nh-debug.el ends here
