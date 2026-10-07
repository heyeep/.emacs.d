;;; nh-go.el --- Modern Go development configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Go support through go-mode and the gopls language server.
;;
;; gopls replaces the older go-guru, godoctor, go-dlv and go-eldoc packages.

;;; Code:

;; Defined before go-mode's :hook so the hook works even if a later block fails.
(defun nh/go-mode-setup ()
  "Setup function for Go mode."
  (add-hook 'before-save-hook 'gofmt-before-save nil t)

  (setq-local tab-width 4)
  (setq-local indent-tabs-mode t)  ;; gofmt requires tabs.

  (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
  (local-set-key (kbd "C-c C-g") 'go-goto-imports)
  (local-set-key (kbd "C-c C-k") 'godoc)
  (local-set-key (kbd "C-c C-d") 'godef-jump))

;; go-mode: major mode for Go files.
;; https://github.com/dominikh/go-mode.el
(use-package go-mode
  :ensure t
  :mode ("\\.go\\'" . go-mode)
  :hook (go-mode . nh/go-mode-setup)
  :config
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-env "GOPATH")))

;; gopls settings. nh-autocompletion already starts LSP in programming modes.
(with-eval-after-load 'lsp-mode
  (setq lsp-go-use-gofumpt t)
  (setq lsp-go-analyses
        '((nilness . t)
          (unusedparams . t)
          (unusedwrite . t)
          (useany . t))))

(when (fboundp 'lsp-deferred)
  (add-hook 'go-mode-hook #'lsp-deferred))

(with-eval-after-load 'yasnippet
  (add-hook 'go-mode-hook #'yas-minor-mode))

(provide 'nh-go)

;;; nh-go.el ends here
