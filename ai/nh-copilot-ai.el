;;; nh-copilot-ai.el --- AI code completion and chat tools -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for AI-powered code completion and chat in Emacs.

;;; Code:

;; Copilot.el: GitHub Copilot integration for Emacs
;; https://github.com/copilot-emacs/copilot.el
(use-package copilot
  :ensure t
  :hook (prog-mode . copilot-mode)
  :custom
  (copilot-idle-delay 0.2)
  :config
  ;; Accept Copilot suggestion with TAB
  (define-key copilot-mode-map (kbd "TAB") #'copilot-accept-completion)
  (define-key copilot-mode-map (kbd "<tab>") #'copilot-accept-completion))

(provide 'nh-copilot-ai)
;;; nh-copilot-ai.el ends here 