;;; nh-code.el --- Code development tools configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for code development tools including debugging, linting, and formatting.

;;; Code:

;; Debug Adapter Protocol for Emacs (dape)
;; Uses dape's built-in adapter configs (debugpy, js-debug, rdbg, ...).
;; Once loaded, dape puts its full command map on C-x C-a.
(use-package dape
  :ensure t
  :bind (("<f5>" . dape)
         ("<f9>" . dape-breakpoint-toggle)))

(provide 'nh-code)
;;; nh-code.el ends here
