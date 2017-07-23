;;;; -*- lexical-binding: t; -*-

(use-package lisp-mode
  :ensure nil
  :config
  ;; https://github.com/jwiegley/use-package/issues/152
  ;; Edebug a defun or defmacro
  (defvar modi/fns-in-edebug nil
    "List of functions for which `edebug' is instrumented.")

  (defconst modi/fns-regexp (concat "(\\s-*"
                                    "\\(defun\\|defmacro\\)\\s-+"
                                    "\\(?1:\\(\\w\\|\\s_\\)+\\)\\_>")
    "Regexp to find defun or defmacro definition.")

  (defun modi/toggle-edebug-defun ()
    (interactive)
    (let (fn)
      (save-mark-and-excursion
        (search-backward-regexp modi/fns-regexp)
        (setq fn (match-string 1))
        (mark-sexp)
        (narrow-to-region (point) (mark))
        (if (member fn modi/fns-in-edebug)
            ;; If the function is already being edebugged, uninstrument it
            (progn
              (setq modi/fns-in-edebug (delete fn modi/fns-in-edebug))
              (eval-region (point) (mark))
              (setq-default eval-expression-print-length 12)
              (setq-default eval-expression-print-level  4)
              (message "Edebug disabled: %s" fn))
          ;; If the function is not being edebugged, instrument it
          (progn
            (add-to-list 'modi/fns-in-edebug fn)
            (setq-default eval-expression-print-length nil)
            (setq-default eval-expression-print-level  nil)
            (edebug-defun)
            (message "Edebug: %s" fn)))
        (widen)))))

(use-package elisp-slime-nav
  :ensure t
  :diminish elisp-slime-nav-mode
  :commands (turn-on-elisp-slime-nav-mode)
  :init
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook 'turn-on-elisp-slime-nav-mode))
  :config
  (turn-on-elisp-slime-nav-mode))

(use-package eval-sexp-fu
  :ensure t
  :commands (eval-sexp-fu-flash-mode)
  :init
  (add-hook 'cider-mode-hook 'eval-sexp-fu-flash-mode)
  (add-hook 'clojure-mode-hook 'eval-sexp-fu-flash-mode)
  (add-hook 'emacs-lisp-mode-hook 'eval-sexp-fu-flash-mode)
  :config
  (eval-sexp-fu-flash-mode 1))

(provide 'yzm-lisp)