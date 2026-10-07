;;; nh-elixir.el --- Modern Elixir development configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; elixir-mode: major mode for Elixir files.
;; https://github.com/elixir-editors/emacs-elixir
(use-package elixir-mode
  :ensure t
  :mode (("\\.elixir\\'" . elixir-mode)
         ("\\.ex\\'" . elixir-mode)
         ("\\.exs\\'" . elixir-mode))
  :config
  (add-hook 'elixir-mode-hook #'nh/elixir-mode-setup))

;; Alchemist: runs Mix tests, looks up docs and talks to IEx.
;; https://github.com/tonini/alchemist.el
(use-package alchemist
  :ensure t
  :commands alchemist-mode
  :hook (elixir-mode . alchemist-mode)
  :config
  (setq alchemist-test-ask-about-save nil)

  ;; Local Elixir and Erlang sources, used when jumping to definitions.
  (setq alchemist-goto-elixir-source-dir "~/.source/elixir/elixir-1.4.1")
  (setq alchemist-goto-erlang-source-dir "~/.source/erlang/otp_src_19.2")

  (add-hook 'alchemist-mode-hook #'nh/alchemist-mode-setup)

  (add-hook 'erlang-mode-hook #'nh/elixir-erlang-mode-hook))

;; Uses ElixirLS when lsp-mode is loaded.
(when (boundp 'lsp-mode)
  (add-hook 'elixir-mode-hook #'lsp-deferred))

(defun nh/elixir-mode-setup ()
  "Setup function for Elixir mode."
  (setq-local tab-width 2)
  (setq-local indent-tabs-mode nil)

  (when (fboundp 'electric-pair-local-mode)
    (electric-pair-local-mode 1)))

(defun nh/alchemist-mode-setup ()
  "Setup function for alchemist mode."
  (local-set-key (kbd "C-c t t") #'alchemist-mix-test)
  (local-set-key (kbd "C-c t f") #'alchemist-mix-test-current-file)
  (local-set-key (kbd "C-c t b") #'alchemist-mix-test-this-buffer)
  (local-set-key (kbd "C-c t a") #'alchemist-mix-test-at-point)
  (local-set-key (kbd "C-c D d") #'alchemist-help-search-at-point)
  (local-set-key (kbd "C-c e e") #'alchemist-iex-send-current-line)
  (local-set-key (kbd "C-c e r") #'alchemist-iex-send-region)
  (local-set-key (kbd "C-c e b") #'alchemist-iex-send-buffer))

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
