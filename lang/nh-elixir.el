;;; nh-elixir.el --- Modern Elixir development configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Elixir Mode: Major mode for editing Elixir files
;; Provides syntax highlighting, indentation, and basic editing features.
;; https://github.com/elixir-editors/emacs-elixir
(use-package elixir-mode
  :ensure t
  :mode (("\\.elixir\\'" . elixir-mode)
         ("\\.ex\\'" . elixir-mode)
         ("\\.exs\\'" . elixir-mode))
  :config
  ;; Add hook for Elixir mode configuration
  (add-hook 'elixir-mode-hook #'nh/elixir-mode-setup))

;; Alchemist: Elixir Tooling Integration
;; Provides project management, documentation lookup, testing framework, and more.
;; https://github.com/tonini/alchemist.el
(use-package alchemist
  :ensure t
  :commands alchemist-mode
  :hook (elixir-mode . alchemist-mode)
  :config
  ;; Prevent asking about saving before running tests
  (setq alchemist-test-ask-about-save nil)

  ;; Configure source directories for better navigation
  (setq alchemist-goto-elixir-source-dir "~/.source/elixir/elixir-1.4.1")
  (setq alchemist-goto-erlang-source-dir "~/.source/erlang/otp_src_19.2")

  ;; Add hook for alchemist mode-specific setup
  (add-hook 'alchemist-mode-hook #'nh/alchemist-mode-setup)

  ;; Enhanced Erlang integration for better navigation
  (add-hook 'erlang-mode-hook #'nh/elixir-erlang-mode-hook))

;; LSP Mode integration for Elixir (optional)
;; ElixirLS provides advanced features like autocompletion, go to definition, etc.
(when (boundp 'lsp-mode)
  (add-hook 'elixir-mode-hook #'lsp-deferred))

;; Setup functions
(defun nh/elixir-mode-setup ()
  "Setup function for Elixir mode."
  ;; Set indentation preferences
  (setq-local tab-width 2)
  (setq-local indent-tabs-mode nil)

  ;; Setup electric pair mode for auto-closing delimiters
  (when (fboundp 'electric-pair-local-mode)
    (electric-pair-local-mode 1)))

(defun nh/alchemist-mode-setup ()
  "Setup function for alchemist mode."
  ;; Set up keybindings for alchemist

  (local-set-key (kbd "C-c t t") #'alchemist-mix-test)
  (local-set-key (kbd "C-c t f") #'alchemist-mix-test-current-file)
  (local-set-key (kbd "C-c t b") #'alchemist-mix-test-this-buffer)
  (local-set-key (kbd "C-c t a") #'alchemist-mix-test-at-point)
  (local-set-key (kbd "C-c d d") #'alchemist-help-search-at-point)
  (local-set-key (kbd "C-c e e") #'alchemist-iex-send-current-line)
  (local-set-key (kbd "C-c e r") #'alchemist-iex-send-region)
  (local-set-key (kbd "C-c e b") #'alchemist-iex-send-buffer))

;; Utility functions
(defun nh/elixir-erlang-pop-back ()
  "Pop back definition function for Erlang mode.
Handles both Erlang and Elixir navigation."
  (interactive)
  (if (ring-empty-p erl-find-history-ring)
      (alchemist-goto-jump-back)
    (erl-find-source-unwind)))

(defun nh/elixir-erlang-mode-hook ()
  "Setup Erlang mode for better Elixir integration."
  (define-key erlang-mode-map (kbd "M-,") #'nh/elixir-erlang-pop-back))

(provide 'nh-elixir)

;;; nh-elixir.el ends here
