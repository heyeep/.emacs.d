;;; nh-go.el --- Modern Go development configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Go language support for Emacs 30.1
;; Provides syntax highlighting, completion, documentation via gopls (LSP).
;;
;; NOTE: The older go-guru / godoctor / go-dlv / go-eldoc packages were
;; removed here.  They are superseded by gopls (navigation, eldoc, docs,
;; rename) which is configured via lsp-mode below.  Debugging can be added
;; back later via dape if desired.

;;; Code:

;; Go Mode setup function (defined before use so the :hook reference is
;; always bound, even if a later use-package block fails to install).
(defun nh/go-mode-setup ()
  "Setup function for Go mode."
  ;; Format code on save (gofmt; gopls also handles this via LSP formatting)
  (add-hook 'before-save-hook 'gofmt-before-save nil t)

  ;; Configure indentation
  (setq-local tab-width 4)
  (setq-local indent-tabs-mode t)  ;; Go uses tabs for indentation

  ;; Set up keybindings for Go development
  (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
  (local-set-key (kbd "C-c C-g") 'go-goto-imports)
  (local-set-key (kbd "C-c C-k") 'godoc)
  (local-set-key (kbd "C-c C-d") 'godef-jump))

;; Go Mode: Major mode for editing Go files
;; Provides syntax highlighting, indentation, and basic editing features.
;; https://github.com/dominikh/go-mode.el
(use-package go-mode
  :ensure t
  :mode ("\\.go\\'" . go-mode)
  :hook (go-mode . nh/go-mode-setup)
  :config
  ;; Ensure GOPATH is correctly set from shell
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-env "GOPATH")))

;; LSP for Go
;; Uses gopls (the official Go language server) for IDE-like features.
;; The prog-mode LSP hook in nh-autocompletion also covers activation;
;; this block just carries Go-specific gopls settings.
(with-eval-after-load 'lsp-mode
  (setq lsp-go-use-gofumpt t)          ;; Use gofumpt formatting
  (setq lsp-go-analyses
        '((nilness . t)                 ;; Check for nil pointers
          (unusedparams . t)            ;; Check for unused parameters
          (unusedwrite . t)             ;; Check for unused writes
          (useany . t))))              ;; Use "any" type when appropriate

(when (fboundp 'lsp-deferred)
  (add-hook 'go-mode-hook #'lsp-deferred))

;; Yasnippet integration for Go
;; Provides code snippets for common Go patterns
(with-eval-after-load 'yasnippet
  (add-hook 'go-mode-hook #'yas-minor-mode))

(provide 'nh-go)

;;; nh-go.el ends here
