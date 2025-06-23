;;; nh-debug.el --- debug -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Enable debug on error when needed
(defun nh/toggle-debug-on-error ()
  "Toggle debug on error."
  (interactive)
  (setq debug-on-error (not debug-on-error))
  (message "Debug on error %s" (if debug-on-error "enabled" "disabled")))

;; Show detailed error traces
(setq debug-on-quit nil)

(provide 'nh-debug)
;;; nh-debug.el ends here
