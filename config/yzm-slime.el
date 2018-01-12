;; Install SLIME
(add-to-list 'load-path "~/.emacs.d/site-lisp/slime")
(require 'slime-autoloads)

(setq inferior-lisp-program "/usr/bin/sbcl")

;; (load (expand-file-name "~/.quicklisp/slime-helper.el"))
(setq slime-contribs '(slime-fancy slime-indentation))
;;(slime-setup '(slime-company))
(provide 'yzm-slime)
