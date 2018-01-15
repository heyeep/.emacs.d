;;;; -*- lexical-binding: t; -*-

(use-package haskell-mode
  :ensure t
  :mode "\\.hs\\'"
  :init
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (add-to-list 'completion-ignored-extensions ".hi"))

;;;###autoload
(defun +haskell-mode ()
  "Bootstrap `yzm-haskell'."
  (setq auto-mode-alist (rassq-delete-all #'+haskell-mode auto-mode-alist))
  (haskell-mode))

(provide 'yzm-haskell)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
