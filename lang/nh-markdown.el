;;; lang/nh-markdown.el --- Markdown configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Markdown Mode: Major mode for editing Markdown files
;; Provides syntax highlighting, live preview, and editing features for
;; Markdown with support for various flavors including GitHub Flavored Markdown.
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
    "Toggle hiding of markdown markup characters.
    This function switches between showing and hiding markdown syntax characters
    like #, *, _, `, etc. When markup is hidden, you see the formatted text.
    When markup is visible, you see the raw markdown syntax."
    (interactive)
    ;; Check current state of markup hiding
    (if markdown-hide-markup
        ;; If markup is currently hidden, show it
        (progn
          (setq markdown-hide-markup nil)
          (markdown-toggle-markup-hiding 0)
          (message "Markup visible"))
      ;; If markup is currently visible, hide it
      (progn
        (setq markdown-hide-markup t)
        (markdown-toggle-markup-hiding 1)
        (message "Markup hidden"))))

  ;; Function to show raw markdown when cursor is on a markdown block
  (defun nh/markdown-show-raw-on-markup ()
    "Show raw markdown when cursor is on a markdown block."
    (when (derived-mode-p 'markdown-mode)
      (let ((pos_ (point))
            ;; Regexp to match markdown syntax characters, ensuring they're not escaped
            ;; Matches: # (headers), * and _ (emphasis), ` (code), ~ (strikethrough), [] and () (links)
            (markup-regexp "\\(^\\|[^\\]\\)\\([#*_`~]\\|\\[\\|\\]\\|(\\|)\\)"))
        (save-excursion
          ;; Move to start of current line to search from there
          (beginning-of-line)
          ;; Check if current line contains any markdown syntax
          (if (re-search-forward markup-regexp (line-end-position) t)
              ;; If we found markup on this line, show the raw markdown
              (when markdown-hide-markup
                (setq markdown-hide-markup nil)
                (markdown-toggle-markup-hiding 0))
            ;; If no markup found, hide the raw markdown to show formatted text
            (unless markdown-hide-markup
              (setq markdown-hide-markup t)
              (markdown-toggle-markup-hiding 1)))))))

  (with-eval-after-load 'markdown-mode
    ;; Set up keybindings for markdown-mode
    (define-key markdown-mode-map (kbd "C-c C-p") 'nh/markdown-toggle-preview)
    (define-key markdown-mode-map (kbd "C-c C-m") 'nh/markdown-toggle-markup-hiding))

    ;; Use default background for code faces
    (set-face-background 'markdown-code-face (face-background 'default nil t))
    (set-face-background 'markdown-inline-code-face (face-background 'default nil t))
    (set-face-background 'markdown-pre-face (face-background 'default nil t)))

  ;; Enable markup hiding by default after mode is fully initialized
  (add-hook 'markdown-mode-hook
            (lambda ()
              ;; Initialize with markup hidden by default
              ;; Use idle timer to ensure mode is fully initialized
              (run-with-idle-timer 0.1 nil
                                  (lambda ()
                                    (when (derived-mode-p 'markdown-mode)
                                      (markdown-toggle-markup-hiding 1))))
              ;; Add cursor movement hook to dynamically show/hide markup
              ;; The 't' at the end makes this hook buffer-local (only affects current buffer)
              (add-hook 'post-command-hook 'nh/markdown-show-raw-on-markup nil t)))

;; Markdown Preview Mode: Live preview for Markdown files
;; Provides real-time HTML preview of Markdown files in a web browser with
;; automatic refresh on save and customizable browser selection.
;; GitHub: https://github.com/ancane/markdown-preview-mode
(use-package markdown-preview-mode
  :ensure t
  :config
  ;; Set the browser command for preview (optional, defaults to system default)
  (setq markdown-preview-mode-browser-command "firefox")

  ;; Automatically refresh preview on buffer save
  (setq markdown-preview-mode-auto-refresh t))

(provide 'nh-markdown)

a;;; lang/nh-markdown.el ends here
