;;; nh-latex.el --- LaTeX configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; AUCTeX: writes, builds and previews LaTeX documents.
;; GitHub: https://www.gnu.org/software/auctex/
(use-package tex
  :ensure auctex
  :defer t
  :init
  (add-hook 'LaTeX-mode-hook #'nh/latex-mode-setup)
  :config
  ;; Parse documents so completion knows their labels and macros.
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)

  ;; Let the PDF viewer jump between source lines and the output.
  (setq TeX-source-correlate-mode t)
  (setq TeX-source-correlate-start-server t)

  (setq TeX-PDF-mode t))

;; Disabled: fixes LaTeX quote pairs in smartparens.
;; (with-eval-after-load 'smartparens
;;   (sp-with-modes '(tex-mode plain-tex-mode latex-mode LaTeX-mode)
;;     ;; Properly define quote pairs for LaTeX
;;     (sp-local-pair "``" "''" :trigger "\"")
;;     ;; Disable the problematic single quote pairing
;;     (sp-local-pair "'" "'" :actions nil)))

;; lsp-latex: LaTeX language server support through texlab.
;; GitHub: https://github.com/ROCKTAKEY/lsp-latex
(use-package lsp-latex
  :ensure t
  :when (fboundp 'lsp-deferred)
  :hook (LaTeX-mode . lsp-deferred)
  :config
  (setq lsp-latex-build-executable "latexmk")
  (setq lsp-latex-build-args '("-pdf" "-interaction=nonstopmode" "-synctex=1")))

;; PDF Tools: view LaTeX output inside Emacs, synced with the source.
;; GitHub: https://github.com/vedang/pdf-tools
(when (package-installed-p 'pdf-tools)
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
        TeX-source-correlate-start-server t)

  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer))

(defun nh/latex-mode-setup ()
  "Set up LaTeX mode with visual enhancements and keybindings."
  (turn-on-visual-line-mode)
  (setq-local fill-column 80)

  (when (fboundp 'flyspell-mode)
    (flyspell-mode 1))

  (setq-local comment-auto-fill-only-comments t)
  (auto-fill-mode 1)

  (when (fboundp 'electric-pair-local-mode)
    (electric-pair-local-mode 1))

  (LaTeX-math-mode 1)

  ;; Fold the document by section, subsection and chapter.
  (outline-minor-mode 1)
  (setq-local outline-regexp "\\\\\\(sub\\)*\\(section\\|paragraph\\|chapter\\)")

  (local-set-key (kbd "C-c C-c") 'TeX-command-run-all)
  (local-set-key (kbd "C-c C-e") 'LaTeX-environment)
  (local-set-key (kbd "C-c C-s") 'LaTeX-section))

(add-hook 'LaTeX-mode-hook #'nh/latex-mode-setup)

(provide 'nh-latex)

;;; nh-latex.el ends here
