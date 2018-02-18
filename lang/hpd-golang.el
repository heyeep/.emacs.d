;;;; -*- lexical-binding: t; -*-
(use-package go-mode
  :ensure t
  :init
  (defun +go-mode-setup ()
    (add-hook 'before-save-hook 'gofmt-before-save nil :local)
    (setq-local tab-width 4))
  (add-hook 'go-mode-hook #'+go-mode-setup)
  :config
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-env "GOPATH")))
(use-package go-guru
  :ensure t
  :init
  (add-hook 'go-mode-hook #'go-guru-hl-identifier-mode))
(use-package company-go
  :ensure t
  :config
  (setq company-go-show-annotation t)
  (add-hook 'go-mode-hook
            (lambda ()
              (setq-local company-backends
                          '((company-go company-yasnippet))))))
(use-package godoctor
  :ensure t
  :commands (godoctor-rename
             godoctor-rename-dry-run
             godoctor-extract
             godoctor-extract-dry-run
             godoctor-toggle
             godoctor-toggle-dry-run
             godoctor-godoc
             godoctor-godoc-dry-run
             godoctor-set-scope))
(use-package go-dlv
  :ensure t
  :commands (dlv-current-func dlv))
(defun +go-mode ()
  "Bootstrap `hpd-golang'."
  (dolist (alist auto-mode-alist)
    (when (eq (cdr alist) '+go-mode)
      (setf (cdr alist) 'go-mode)))
  (go-mode))
(provide 'hpd-golang)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; hpd-golang.el ends here

