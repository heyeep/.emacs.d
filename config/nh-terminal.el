;;; nh-terminal.el --- terminal -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Set up shell environment for vterm compilation
(setenv "PATH" (concat "/opt/homebrew/bin:/opt/homebrew/sbin:" (getenv "PATH")))
(setenv "SHELL" "/bin/zsh")

;; Ensure CMake is available for vterm
(setq vterm-cmake-path "/opt/homebrew/bin/cmake")

;; Vterm: Fully-featured terminal emulator
;; Provides a fast, feature-complete terminal emulator within Emacs using
;; libvterm, supporting complex terminal applications and true color output.
;; GitHub: https://github.com/akermu/emacs-libvterm
(use-package vterm
  :ensure t
  :commands vterm
  :config
  (setq vterm-shell "/bin/zsh")              ;; Use zsh as the default shell
  (setq vterm-max-scrollback 200000)         ;; Increase scrollback buffer
  ;; Example keybinding: open new vterm with C-c t
  (global-set-key (kbd "C-c t") #'vterm))

;; Multi Vterm: Manage multiple vterm buffers
;; Provides enhanced management for multiple vterm instances with easy
;; switching between terminals and project-specific terminal sessions.
;; GitHub: https://github.com/suonlight/multi-vterm
(use-package multi-vterm
  :ensure nil
  :after vterm
  :config
  ;; Keybindings for multi-vterm navigation and creation
  (define-key vterm-mode-map (kbd "C-c n") 'multi-vterm-next)
  (define-key vterm-mode-map (kbd "C-c p") 'multi-vterm-prev)
  (define-key vterm-mode-map (kbd "C-c c") 'multi-vterm))

(provide 'nh-terminal)
;;; nh-terminal.el ends here
