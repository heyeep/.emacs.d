(use-package auctex
  :defer t
  :ensure t)

;; Parsing
(setq TeX-auto-save t)
(setq TeX-parse-self t)

;; Master file
(setq-default TeX-master nil)

(global-font-lock-mode t)

;;(latex-preview-pane-enable)
(provide 'yzm-latex)
