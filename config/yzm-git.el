;;;; -*- lexical-binding: t; -*-

(use-package magit
  :ensure t
  :commands (magit-toplevel magit-status magit-blame magit-log)
  :config
  (setq magit-repository-directories '("~/Code"))
  (setq magit-refresh-status-buffer nil)
  (setq magit-completing-read-function 'ivy-completing-read)
  (setq vc-handled-backends (delq 'Git vc-handled-backends)))

(provide 'yzm-git)