;;; nh-go.el --- Modern Go development configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Go language support for Emacs 30.1
;; Provides syntax highlighting, completion, documentation, and debugging.

;;; Code:

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

;; Go Guru: Semantic analysis for Go
;; Provides code navigation, identifier highlighting, and contextual information
;; https://github.com/dominikh/go-mode.el/tree/master/go-guru.el
(use-package go-guru
  :ensure t
  :after go-mode
  :hook (go-mode . go-guru-hl-identifier-mode))

;; Go Doctor: Refactoring tool for Go
;; Provides refactoring capabilities for Go code
;; https://github.com/microamp/godoctor.el
(use-package godoctor
  :ensure t
  :after go-mode
  :commands (godoctor-rename
             godoctor-rename-dry-run
             godoctor-extract
             godoctor-extract-dry-run
             godoctor-toggle
             godoctor-toggle-dry-run
             godoctor-godoc
             godoctor-godoc-dry-run
             godoctor-set-scope))

;; Go Delve: Debugger for Go
;; Provides an interface to the Delve debugger for Go
;; https://github.com/benma/go-dlv.el
(use-package go-dlv
  :ensure t
  :after go-mode
  :commands (dlv-current-func dlv))

;; LSP for Go (optional)
;; Uses gopls (the official Go language server) for IDE-like features
(when (fboundp 'lsp-deferred)
  (use-package lsp-mode
    :ensure t
    :hook (go-mode . lsp-deferred)
    :config
    (setq lsp-go-use-gofumpt t)          ;; Use gofumpt formatting
    (setq lsp-go-analyses
          '((nilness . t)                 ;; Check for nil pointers
            (unusedparams . t)            ;; Check for unused parameters
            (unusedwrite . t)             ;; Check for unused writes
            (useany . t)))))              ;; Use "any" type when appropriate

;; Company backend for Go
;; Provides completion for Go code (used when LSP is not available)
;; https://github.com/nsf/gocode
(use-package company-go
  :ensure t
  :after (go-mode company)
  :config
  (setq company-go-show-annotation t)
  :hook (go-mode . nh/go-company-setup))

;; Go Eldoc: Documentation in minibuffer for Go
;; Shows function signatures and documentation in the minibuffer
;; https://github.com/syohex/emacs-go-eldoc
(use-package go-eldoc
  :ensure t
  :after go-mode
  :hook (go-mode . go-eldoc-setup))

;; Yasnippet integration for Go
;; Provides code snippets for common Go patterns
(with-eval-after-load 'yasnippet
  (add-hook 'go-mode-hook #'yas-minor-mode))

;; Go Mode setup function
(defun nh/go-mode-setup ()
  "Setup function for Go mode."
  ;; Format code on save
  (add-hook 'before-save-hook 'gofmt-before-save nil t)
  
  ;; Configure indentation
  (setq-local tab-width 4)
  (setq-local indent-tabs-mode t)  ;; Go uses tabs for indentation

  ;; Set up keybindings for Go development
  (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
  (local-set-key (kbd "C-c C-g") 'go-goto-imports)
  (local-set-key (kbd "C-c C-k") 'godoc)
  (local-set-key (kbd "C-c C-d") 'godef-jump)
  
  ;; Refactoring keybindings
  (local-set-key (kbd "C-c r r") 'godoctor-rename)
  (local-set-key (kbd "C-c r e") 'godoctor-extract)
  (local-set-key (kbd "C-c r t") 'godoctor-toggle)
  
  ;; Debugging keybindings
  (local-set-key (kbd "C-c d d") 'dlv)
  (local-set-key (kbd "C-c d f") 'dlv-current-func))

;; Company setup for Go
(defun nh/go-company-setup ()
  "Setup company backends for Go mode."
  (setq-local company-backends '((company-go company-yasnippet))))

;; Legacy compatibility function
(defun nh/go-mode ()
  "Bootstrap Go mode configuration."
  ;; Update auto-mode-alist entries that point to this function
  (dolist (alist auto-mode-alist)
    (when (eq (cdr alist) 'nh/go-mode)
      (setf (cdr alist) 'go-mode)))
  (go-mode))

(provide 'nh-go)

;;; nh-go.el ends here
