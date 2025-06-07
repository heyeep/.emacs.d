;;; vaibe-mode.el --- Clean working solution -*- lexical-binding: t; -*-
;;; Commentary:
;; A clean solution that addresses all issues
;;; Code:

;; Add vaibe-mode to load path
(add-to-list 'load-path "~/vaibe-mode")
(require 'vaibe)

;; Register OpenAI provider
;; (vaibe-llm-register-provider (vaibe-llm-openai-make-provider))
;; (setq vaibe-llm-active-provider-name 'openai)

;; ;; Optional: Set default model
;; (setq vaibe-api-default-model "gpt-4o")

;; ;; Bind to a convenient key
;; (global-set-key (kbd "C-c v") 'vaibe-menu)

(provide 'nh-vaibe-ai)

;;; nh-vaibe-ai.el ends here
