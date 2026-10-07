;;; lang/nh-markdown.el --- Markdown configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; markdown-mode: major mode for Markdown, with GitHub-flavored extras.
;; GitHub: https://github.com/jrblevin/markdown-mode
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
  (setq markdown-enable-github-flavored-markdown t)

  (setq markdown-hide-markup t)

  ;; Color code blocks with each language's own major mode.
  (setq markdown-fontify-code-blocks-natively t)

  (add-hook 'markdown-mode-hook 'auto-fill-mode)

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

  (defun nh/markdown-toggle-markup-hiding ()
    "Toggle hiding of markdown markup characters.
    This function switches between showing and hiding markdown syntax characters
    like #, *, _, `, etc. When markup is hidden, you see the formatted text.
    When markup is visible, you see the raw markdown syntax."
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

  (with-eval-after-load 'markdown-mode
    (define-key markdown-mode-map (kbd "C-c C-p") 'nh/markdown-toggle-preview)
    (define-key markdown-mode-map (kbd "C-c C-m") 'nh/markdown-toggle-markup-hiding))

    ;; Give code the normal background instead of a shaded box.
    (set-face-background 'markdown-code-face (face-background 'default nil t))
    (set-face-background 'markdown-inline-code-face (face-background 'default nil t))
    (set-face-background 'markdown-pre-face (face-background 'default nil t)))

  (add-hook 'markdown-mode-hook
            (lambda ()
              ;; Wait until the mode finishes starting, or hiding doesn't apply.
              (run-with-idle-timer 0.1 nil
                                  (lambda ()
                                    (when (derived-mode-p 'markdown-mode)
                                      (markdown-toggle-markup-hiding 1))))))

;; markdown-preview-mode: live HTML preview in a browser, refreshed on save.
;; GitHub: https://github.com/ancane/markdown-preview-mode
(use-package markdown-preview-mode
  :ensure t
  :config
  (setq markdown-preview-mode-browser-command "firefox")

  (setq markdown-preview-mode-auto-refresh t))

(provide 'nh-markdown)

;;; lang/nh-markdown.el ends here
