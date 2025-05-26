;;; lang/nh-markdown.el --- Markdown configuration -*- lexical-binding: t; -*-

;; This file configures Emacs for working with Markdown files.

(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\.mdown\\'" . markdown-mode)
         ("\\.mkd\\'" . markdown-mode)
         ("\\.mkdn\\'" . markdown-mode)
         ("\\.text\\'" . markdown-mode)
         ("\\.txt\\'" . markdown-mode))
  :config
  ;; Enable GitHub Flavored Markdown features by default for markdown-mode
  (setq markdown-enable-github-flavored-markdown t)

  ;; Hide markup characters for a cleaner view
  (setq markdown-hide-markup t)

  ;; Fontify code blocks natively using Emacs's built-in font-lock
  (setq markdown-fontify-code-blocks-natively t)

  ;; Auto-fill mode for wrapping text
  (add-hook 'markdown-mode-hook 'auto-fill-mode)

  ;; Enable visual line mode for soft wrapping
  (add-hook 'markdown-mode-hook 'visual-line-mode)

  ;; Set up keybindings for markdown-mode (optional, customize as needed)
  ;; (with-eval-after-load 'markdown-mode
  ;;   (define-key markdown-mode-map (kbd "C-c C-p") 'markdown-preview-mode))
  )

(use-package markdown-preview-mode
  :ensure t
  :hook ((markdown-mode . markdown-preview-mode)
         (gfm-mode . markdown-preview-mode))
  :config
  ;; Set the browser command for preview (optional, defaults to system default)
  ;; (setq markdown-preview-mode-browser-command "firefox")

  ;; Automatically refresh preview on buffer save
  (setq markdown-preview-mode-auto-refresh t)

  ;; Set the port for the local server
  ;; (setq markdown-preview-mode-port 8080)
  )

(provide 'nh-markdown)

;;; lang/nh-markdown.el ends here
