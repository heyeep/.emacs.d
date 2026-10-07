;;; nh-code.el --- Code development tools configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Debugger setup.

;;; Code:

;; dape: runs debuggers through the Debug Adapter Protocol, with its built-in configs.
;; Once loaded, dape puts its full command map on C-x C-a.
(use-package dape
  :ensure t
  :bind (("<f5>" . dape)
         ("<f9>" . dape-breakpoint-toggle)))

(provide 'nh-code)
;;; nh-code.el ends here
