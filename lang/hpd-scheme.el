(use-package geiser
  :ensure t
  :commands (geiser-mode)
  :init
  (add-hook 'scheme-mode-hook #'geiser-mode)
  :config
  (setq geiser-mode-start-repl-p t)
  (setq geiser-repl-query-on-kill-p nil)
  (setq geiser-default-implementation '(racket))
  (setq geiser-racket-binary "/Applications/Racket/bin/racket"))

;;;###autoload
(defun +scheme-mode ()
  "Bootstrap `hpd-scheme'."
  (setq auto-mode-alist (rassq-delete-all #'+scheme-mode auto-mode-alist))
  (scheme-mode))

(provide 'hpd-scheme)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
