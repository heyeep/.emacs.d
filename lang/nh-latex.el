;;; nh-latex.el --- LaTeX configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; AUCTeX: Integrated environment for TeX
;; A sophisticated environment for writing, editing, and processing TeX documents
;; that offers full support for LaTeX, ConTeXt, and DocTeX, with preview, folding,
;; and source specials for superior document editing.
;; GitHub: https://www.gnu.org/software/auctex/
(use-package tex
  :ensure auctex
  :defer t
  :init
  (add-hook 'LaTeX-mode-hook #'nh/latex-mode-setup)
  :config
  ;; Enable document parsing for completion and structure view
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)

  ;; Enable source specials for improved synchronization with PDF
  (setq TeX-source-correlate-mode t)
  (setq TeX-source-correlate-start-server t)

  ;; Modern compilation options
  (setq TeX-PDF-mode t))

;; Fix for smartparens LaTeX issue
;; (with-eval-after-load 'smartparens
;;   (sp-with-modes '(tex-mode plain-tex-mode latex-mode LaTeX-mode)
;;     ;; Properly define quote pairs for LaTeX
;;     (sp-local-pair "``" "''" :trigger "\"")
;;     ;; Disable the problematic single quote pairing
;;     (sp-local-pair "'" "'" :actions nil)))

;; LSP integration for LaTeX
;; Language Server Protocol support for LaTeX documents providing intelligent
;; code completion, syntax checking, and other language features through
;; texlab or digestif language servers.
;; GitHub: https://github.com/ROCKTAKEY/lsp-latex
(use-package lsp-latex
  :ensure t
  :when (fboundp 'lsp-deferred)
  :hook (LaTeX-mode . lsp-deferred)
  :config
  (setq lsp-latex-build-executable "latexmk")
  (setq lsp-latex-build-args '("-pdf" "-interaction=nonstopmode" "-synctex=1")))

;; PDF Tools integration for superior PDF viewing
;; An Emacs PDF reader that renders through the poppler library for crisp display
;; and fast navigation, with support for annotations and SyncTeX integration.
;; GitHub: https://github.com/vedang/pdf-tools
(when (package-installed-p 'pdf-tools)
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
        TeX-source-correlate-start-server t)

  ;; Update PDF buffers after successful compilation
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer))

(defun nh/latex-mode-setup ()
  "Set up LaTeX mode with visual enhancements and keybindings."
  ;; Visual enhancements
  (turn-on-visual-line-mode)
  (setq-local fill-column 80)

  ;; Spell checking
  (when (fboundp 'flyspell-mode)
    (flyspell-mode 1))

  ;; Auto-fill for comments
  (setq-local comment-auto-fill-only-comments t)
  (auto-fill-mode 1)

  ;; Electric pairs for LaTeX
  (when (fboundp 'electric-pair-local-mode)
    (electric-pair-local-mode 1))

  ;; Enable math mode in AUCTeX
  (LaTeX-math-mode 1)

  ;; Setup outline mode for better navigation
  (outline-minor-mode 1)
  (setq-local outline-regexp "\\\\\\(sub\\)*\\(section\\|paragraph\\|chapter\\)")

  ;; Set up keybindings for LaTeX
  (local-set-key (kbd "C-c C-c") 'TeX-command-run-all)
  (local-set-key (kbd "C-c C-e") 'LaTeX-environment)
  (local-set-key (kbd "C-c C-s") 'LaTeX-section))

;; Add the hook
(add-hook 'LaTeX-mode-hook #'nh/latex-mode-setup)

(provide 'nh-latex)

;;; nh-latex.el ends here
