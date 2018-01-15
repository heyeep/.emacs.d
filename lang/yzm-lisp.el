;;;; -*- lexical-binding: t; -*-

(use-package lisp-mode
  :ensure nil
  :mode
  "\\.lisp\\'")

(use-package slime
  :ensure t
  :init
  (cond
   ((eq system-type 'gnu/linux)
    (setq inferior-lisp-program "/usr/bin/sbcl"))
   (t
    (setq inferior-lisp-program "/usr/local/bin/sbcl")))
  :config
  (use-package slime-company
    :ensure t
    :commands (company-slime)
    :init
    (add-hook 'lisp-mode-hook
              (lambda ()
                (+company-push-backend 'company-slime t)))
    :config
    (setq slime-company-completion 'fuzzy))
  (setq slime-contribs '(slime-asdf
                         slime-fancy
                         slime-indentation
                         slime-sbcl-exts
                         slime-scratch
                         slime-company))
  ;; Enable fuzzy matching in code buffer and SLIME REPL.
  (setq slime-complete-symbol*-fancy t)

  (slime-setup)

  (with-eval-after-load 'slime
    (evil-define-key 'normal slime-mode-map
"\C-j" #'slime-eval-print-last-expression)))

(defun +commonlisp-mode ()
  "Bootstrap 'yzm-commonlisp'."
  (setq auto-mode-alist (rassq-delete-all #'+commonlisp-mode auto-mode-alist))
  (lisp-mode))

(provide 'yzm-lisp)
