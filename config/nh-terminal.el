;;; nh-terminal.el --- Terminal-specific configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration and settings specific to running Emacs in a terminal environment.

;;; Code:

;; Set up shell environment for vterm compilation
(setenv "PATH" (concat "/opt/homebrew/bin:/opt/homebrew/sbin:" (getenv "PATH")))
(setenv "SHELL" "/bin/zsh")

;; Ensure CMake is available for vterm
(setq vterm-cmake-path "/opt/homebrew/bin/cmake")

;; Vterm: Fully-featured terminal emulator
;; Fast terminal emulator based on libvterm providing full terminal capabilities
;; including colors, cursor positioning, and complex terminal applications support.
;; GitHub: https://github.com/akermu/emacs-libvterm
(use-package vterm
  :ensure t
  :commands vterm
  :config
  (setq vterm-shell "/bin/zsh")             ;; Use zsh as the default shell
  (setq vterm-max-scrollback 10000)         ;; Increase scrollback buffer
  (setq vterm-buffer-name-string "vterm %s")
  ;; Example keybinding: open new vterm with C-c t
  (global-set-key (kbd "C-c t") #'vterm))

;; Multi Vterm: Manage multiple vterm buffers
;; Provides convenient functions to create, manage, and switch between multiple
;; vterm terminal sessions with dedicated buffer management and keybindings.
;; GitHub: https://github.com/suonlight/multi-vterm
(use-package multi-vterm
  :ensure t
  :after vterm
  :bind (("C-c t t" . multi-vterm)
         ("C-c t n" . multi-vterm-next)
         ("C-c t p" . multi-vterm-prev)
         ("C-c t d" . multi-vterm-dedicated-toggle)))

(provide 'nh-terminal)
;;; nh-terminal.el ends here
