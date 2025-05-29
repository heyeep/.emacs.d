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

  ;; Function to toggle between raw markdown and preview
  (defun nh/markdown-toggle-preview ()
    "Toggle between raw markdown and preview mode."
    (interactive)
    (if markdown-preview-mode
        (progn
          (markdown-preview-mode -1)
          (message "Showing raw markdown"))
      (progn
        (markdown-preview-mode 1)
        (message "Showing preview"))))

  ;; Function to toggle markup hiding
  (defun nh/markdown-toggle-markup-hiding ()
    "Toggle hiding of markdown markup characters."
    (interactive)
    (if markdown-hide-markup
        (progn
          (setq markdown-hide-markup nil)
          (markdown-toggle-markup-hiding 0)
          (message "Markup visible"))
      (progn
        (setq markdown-hide-markup t)
        (markdown-toggle-markup-hiding 1)
        (message "Markup hidden"))))

  ;; Set up keybindings for markdown-mode
  (with-eval-after-load 'markdown-mode
    (define-key markdown-mode-map (kbd "C-c C-p") 'nh/markdown-toggle-preview)
    (define-key markdown-mode-map (kbd "C-c C-m") 'nh/markdown-toggle-markup-hiding))

  ;; Enable markup hiding by default after mode is fully initialized
  (add-hook 'markdown-mode-hook
            (lambda ()
              (run-with-idle-timer 0.1 nil
                                  (lambda ()
                                    (when (derived-mode-p 'markdown-mode)
                                      (markdown-toggle-markup-hiding 1))))))
  )

(use-package markdown-preview-mode
  :ensure t
  :config
  ;; Set the browser command for preview (optional, defaults to system default)
  (setq markdown-preview-mode-browser-command "firefox")

  ;; Automatically refresh preview on buffer save
  (setq markdown-preview-mode-auto-refresh t)
  )

(provide 'nh-markdown)

;;; lang/nh-markdown.el ends here
