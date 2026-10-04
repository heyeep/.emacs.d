;;; ai/nh-agent-ai.el --- Agent Shell configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Runs Claude Code and Codex in normal Emacs buffers through the Agent Client
;; Protocol. Needs the adapters on PATH:
;;   npm install -g @agentclientprotocol/claude-agent-acp @agentclientprotocol/codex-acp
;; GitHub: https://github.com/xenodium/agent-shell

;;; Code:

;; Both agents log in through their own CLIs (`claude', `codex'), which is the
;; package default, so no API keys are set here.
(use-package agent-shell
  :ensure t
  :bind (("C-c A A" . agent-shell)
         ("C-c A c" . agent-shell-anthropic-start-claude-code)
         ("C-c A x" . agent-shell-openai-start-codex)))

(provide 'nh-agent-ai)
;;; nh-agent-ai.el ends here
