;;;; -*- lexical-binding: t; -*-

(use-package elixir-mode
  :ensure t
  :mode
  ("\\.elixir\\'" . elixir-mode)
  ("\\.ex\\'" . elixir-mode)
  ("\\.exs\\'" . elixir-mode)
  :init
  (add-hook 'elixir-mode-hook
            (lambda ()
              (setq tab-width 2)
              (after-evil
               (setq evil-shift-width 2)))))

(use-package alchemist
  :ensure t
  :commands alchemist-mode
  :init
  (add-hook 'elixir-mode-hook #'alchemist-mode)
  (add-hook 'alchemist-mode-hook
            (lambda ()
              (jojo/company-merge-backends)) t)
  :config
  (setq alchemist-test-ask-about-save nil)
  (setq alchemist-goto-elixir-source-dir "~/.source/elixir/elixir-1.4.1")
  (setq alchemist-goto-erlang-source-dir "~/.source/erlang/otp_src_19.2")

  ;; Erlang synergy
  (defun jojo/elixir-erlang-pop-back ()
    "Pop back definition function for Erlang mode."
    (interactive)
    (if (ring-empty-p erl-find-history-ring)
        (alchemist-goto-jump-back)
      (erl-find-source-unwind)))

  (defun jojo/alchemist-erlang-mode-hook ()
    (define-key erlang-mode-map (kbd "M-,") 'jojo/elixir-erlang-pop-back))

  (add-hook 'erlang-mode-hook 'jojo/alchemist-erlang-mode-hook))

(provide 'yzm-elixir)
