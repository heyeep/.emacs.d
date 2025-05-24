;;; nh-terminal.el --- Terminal-specific configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration and settings specific to running Emacs in a terminal environment.

;;; Code:

(use-package vterm
  :ensure t
  :commands vterm
  :config
  (setq vterm-shell "/bin/zsh")             ;; Use zsh as the default shell
  (setq vterm-max-scrollback 20000)         ;; Increase scrollback buffer
  ;; Example keybinding: open new vterm with C-c t
  (global-set-key (kbd "C-c t") #'vterm))

;;; Manage multiple vterm buffers with multi-vterm
(use-package multi-vterm
  :ensure t
  :after vterm
  :config
  ;; Keybindings for multi-vterm navigation and creation
  (define-key vterm-mode-map (kbd "C-c n") 'multi-vterm-next)
  (define-key vterm-mode-map (kbd "C-c p") 'multi-vterm-prev)
  (define-key vterm-mode-map (kbd "C-c c") 'multi-vterm))

(provide 'nh-terminal)
;;; nh-terminal.el ends here 