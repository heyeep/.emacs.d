;;; Vaibe-mode.el --- Clean working solution -*- lexical-binding: t; -*-

;;; Commentary:

;; Loads vaibe-mode, my own AI chat package, from a local checkout.

;;; Code:

;; Load vaibe-mode from its local checkout.
  (add-to-list 'load-path "~/Code/claude/vaibe/vaibe-mode")
  (require 'vaibe)
  (setq vaibe-enable-ollama nil)
;;  (global-vaibe-mode 1)

  ;; Corfu popups get in the way while typing chat messages.
  (add-hook 'vaibe-chat-mode-hook
            (lambda ()
              (when (fboundp 'corfu-mode) (corfu-mode -1))))

;; (add-to-list 'load-path "~/vaibe-mode")
;; (require 'vaibe)
;;   (setq vaibe-enable-ollama nil)

(provide 'nh-vaibe-ai)

;;; nh-vaibe-ai.el ends here
