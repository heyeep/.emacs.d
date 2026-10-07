;;; ai/nh-aider-ai.el --- Aidermacs configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; aidermacs: runs the aider CLI inside Emacs. Needs aider installed.
(use-package aidermacs
  :ensure nil
  :defer t
  :bind (("C-c a" . aidermacs-transient-menu))
  :custom
  ;; Architect mode plans with one model and edits with another.
  (aidermacs-use-architect-mode t)

  ;; Also used as the editor model when aidermacs-editor-model isn't set.
  (aidermacs-default-model "openai/gpt-4o")

  (aidermacs-backend 'vterm)

  ;; Commit by hand instead of letting aider commit each change.
  (aidermacs-auto-commits nil)

  (aidermacs-show-diff-after-change t)

  ;; Disabled: watch files for AI instructions written in comments.
  ;; (aidermacs-watch-files t)

  ;; Disabled: files aider always reads but never edits.
  ;; (aidermacs-global-read-only-files '("~/.aider/AI_RULES.md"))

  ;; Disabled: per-project files aider reads but never edits.
  ;; (aidermacs-project-read-only-files '("CONVENTIONS.md" "README.md"))

  ;; Disabled: extra arguments for the aider command.
  ;; :config
  ;; (add-to-list 'aidermacs-extra-args "--verbose")
  )

(provide 'nh-aider-ai)

;;; ai/nh-aider-ai.el ends here
