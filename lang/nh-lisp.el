;;; nh-lisp.el --- Lisp language configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for Lisp development (Common Lisp, Emacs Lisp, Scheme, etc.)

;;; Code:

;; Provides a powerful IDE for Common Lisp development in Emacs,
;; including a REPL, interactive debugger, code navigation, and more.
;; https://github.com/slime/slime
(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program "sbcl") ;; or your preferred CL implementation
  (slime-setup '(slime-fancy)))

;; Integrates Scheme interpreters with Emacs, offering a REPL,
;; code evaluation, and interactive development for Scheme dialects.
;; https://www.nongnu.org/geiser/
(use-package geiser
  :ensure t)

;; Enables structured editing of S-expressions, helping keep
;; parentheses balanced and making manipulation of Lisp code safer and easier.
;; https://github.com/emacsmirror/paredit
(use-package paredit
  :ensure t
  :hook ((emacs-lisp-mode lisp-mode lisp-interaction-mode scheme-mode) . paredit-mode))

(provide 'nh-lisp)
;;; nh-lisp.el ends here

