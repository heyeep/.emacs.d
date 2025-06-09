;;; Vaibe-mode.el --- Clean working solution -*- lexical-binding: t; -*-

;;; Commentary:

;; A clean solution that addresses all issues

;;; Code:

;; Add vaibe-mode to load path ;; vaibe-mode with all fixes
  (add-to-list 'load-path "/Users/hiep/Code/claude/vaibe/vaibe-mode")
  (require 'vaibe)
  (setq vaibe-enable-ollama nil)
;;  (global-vaibe-mode 1)

  ;; Disable completion in chat buffers
  (add-hook 'vaibe-chat-mode-hook
            (lambda ()
              (when (fboundp 'corfu-mode) (corfu-mode -1))))

;; (add-to-list 'load-path "~/vaibe-mode")
;; (require 'vaibe)
;;   (setq vaibe-enable-ollama nil)

(provide 'nh-vaibe-ai)

;;; nh-vaibe-ai.el ends here
