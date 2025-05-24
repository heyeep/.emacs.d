;;; nh-org.el --- Org mode configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Org mode and related tools configuration.

;;; Code:

;; htmlize: Used for exporting Org buffers to HTML with syntax highlighting
(use-package htmlize
  :ensure t
  :commands (htmlize-buffer htmlize-region htmlize-file htmlize-many-files htmlize-many-files-dired)
  :custom
  (htmlize-output-type 'css)
  (htmlize-html-charset "UTF-8")
  (htmlize-generate-hyperlinks t)
  (htmlize-generate-anchors t))

;; --- Org mode configuration ---

(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :init
  ;; Set up writing enhancements for encrypted org files (like M.org.gpg)
  (defun nh/set-up-writing-conditionally ()
    ;; If the buffer is "M.org.gpg", enable auto-fill and set fill column
    (when (string-equal (buffer-name) "M.org.gpg")
      (turn-on-auto-fill)
      (set-fill-column 80)))

  ;; Customize the appearance of Org headings and document title for readability
  (defun nh/customize-org-ui ()
    (set-face-attribute 'org-document-title nil :weight 'bold :height 1.4)
    (set-face-attribute 'org-level-1 nil :inherit 'outline-1 :height 1.3 :weight 'bold)
    (set-face-attribute 'org-level-2 nil :inherit 'outline-2 :height 1.2 :weight 'bold)
    (set-face-attribute 'org-level-3 nil :inherit 'outline-3 :height 1.1)
    (set-face-attribute 'org-level-4 nil :inherit 'outline-4 :height 1.0))

  ;; Improve source block editing: indent code or cycle org structure
  (defun nh/indent-org-block-automatically-or-cycle ()
    "Indent source code in source blocks, otherwise org-cycle."
    (interactive)
    (if (org-in-src-block-p)
        (progn
          (org-edit-special)
          (indent-region (point-min) (point-max))
          (org-edit-src-exit))
      (call-interactively #'org-cycle)))

  ;; Add hooks for writing and UI customization
  (add-hook 'org-mode-hook #'nh/set-up-writing-conditionally)
  (add-hook 'org-mode-hook #'nh/customize-org-ui)
  :config
  ;; Enable ODT export (Open Document Text)
  (require 'ox-odt nil t)
  ;; Enable Graphviz DOT support in org-babel
  (require 'ob-dot nil t)
  ;; Enable notmuch email integration in Org
  (require 'org-notmuch nil t)
  ;; Automatically redisplay inline images after executing code blocks
  (add-hook 'org-babel-after-execute-hook #'org-redisplay-inline-images)
  ;; Org source block and export settings
  (setq org-src-fontify-natively t      ;; Syntax highlight code in src blocks
        org-src-preserve-indentation nil
        org-edit-src-content-indentation 0
        org-src-tab-acts-natively t      ;; TAB acts as expected in src blocks
        org-export-backends '(ascii html icalendar latex md))
  ;; Enable more languages for code blocks (babel)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (ruby . t)
     (dot . t)
     (js . t)
     (lisp . t)))
  ;; For React, use js or typescript blocks, e.g. #+BEGIN_SRC js or #+BEGIN_SRC typescript
  )

;; Visual enhancement: Prettify Org bullets
(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

;; Modern Org appearance: borders, checkboxes, etc.
(use-package org-modern
  :ensure t
  :hook (org-mode . org-modern-mode))
;; Note: org-modern overrides some bullet styles, but org-bullets can still provide extra flair for headings.

;; Make Org text more readable with variable-pitch font for prose
(add-hook 'org-mode-hook #'variable-pitch-mode)

;; Drag-and-drop and paste images directly into Org files
(use-package org-download
  :ensure t
  :hook (org-mode . org-download-enable))

;; Indent content to match heading level
(add-hook 'org-mode-hook #'org-indent-mode)

;; Graphviz DOT mode: Major mode for editing and previewing .dot and .gv files
;; Integrates with org-babel for rendering diagrams in Org documents
(use-package graphviz-dot-mode
  :ensure t
  :mode (("\\.dot\\'" . graphviz-dot-mode)
         ("\\.gv\\'" . graphviz-dot-mode))
  :init
  (setq default-tab-width 4))

(provide 'nh-org)
;;; nh-org.el ends here 