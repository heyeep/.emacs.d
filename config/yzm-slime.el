;; Install SLIME
(use-package slime
  :ensure t
  :commands (slime slime-lisp-mode-hook)
  :diminish)

;; Set path
(setq inferior-lisp-program "/usr/bin/sbcl")
(setq slime-contribs '(slime-fancy))

(load (expand-file-name "~/.quicklisp/slime-helper.el"))
(provide 'yzm-slime)
