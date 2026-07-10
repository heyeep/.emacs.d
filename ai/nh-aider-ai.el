;;; ai/nh-aider-ai.el --- Aidermacs configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Aidermacs requires the 'aider' Python CLI tool to be installed on your system.
(use-package aidermacs
  :ensure nil
  :defer t
  :bind (("C-c a" . aidermacs-transient-menu))
  :custom
  ;; Enable Architect mode for state-of-the-art results with two specialized models.
  ;; This requires configuring aidermacs-architect-model and optionally aidermacs-editor-model.
  (aidermacs-use-architect-mode t)

  ;; Set the default model to use when Architect mode is not active,
  ;; or as the editor model if aidermacs-editor-model is not explicitly set.
  (aidermacs-default-model "openai/gpt-4o")

  ;; Choose your preferred terminal backend. 'vterm' offers better compatibility.
  ;; Ensure 'vterm' is installed if you choose it.
  (aidermacs-backend 'vterm)

  ;; Disable auto-commits by Aider, allowing manual Git workflow.
  ;; Set to t if you want Aider to auto-commit changes.
  (aidermacs-auto-commits nil)

  ;; Enable showing diffs after AI-generated changes using Emacs' ediff.
  (aidermacs-show-diff-after-change t)

  ;; Enable file watching for AI coding instructions in comments (requires vterm backend).
  ;; (aidermacs-watch-files t)

  ;; Add global read-only files (e.g., AI_RULES.md)
  ;; (aidermacs-global-read-only-files '("~/.aider/AI_RULES.md"))

  ;; Add project-specific read-only files (e.g., documentation)
  ;; (aidermacs-project-read-only-files '("CONVENTIONS.md" "README.md"))

  ;; Add extra arguments to the Aider command line.
  ;; :config
  ;; (add-to-list 'aidermacs-extra-args "--verbose")
  )

(provide 'nh-aider-ai)

;;; ai/nh-aider-ai.el ends here
