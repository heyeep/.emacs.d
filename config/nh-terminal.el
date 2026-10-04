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
  ;; C-c v, not C-c t: the old global-set-key in :config silently stole
  ;; C-c t from consult-theme the first time vterm loaded.
  :bind (("C-c v" . vterm))
  :config
  (setq vterm-shell "/bin/zsh")              ;; Use zsh as the default shell
  (setq vterm-max-scrollback 200000)         ;; Increase scrollback buffer
  ;; Command is Meta here, so Cmd+C/Cmd+V arrive as M-c/M-v, which vterm
  ;; would otherwise pass to the shell instead of copying and pasting.
  (define-key vterm-mode-map (kbd "M-c") #'ignore)
  (define-key vterm-mode-map (kbd "M-v") #'vterm-yank)
  (define-key vterm-mode-map (kbd "C-SPC") #'nh/vterm-start-selection)
  (define-key vterm-mode-map [down-mouse-1] #'nh/vterm-mouse-select)
  (define-key vterm-copy-mode-map (kbd "M-w") #'vterm-copy-mode-done)
  (define-key vterm-copy-mode-map (kbd "M-c") #'vterm-copy-mode-done)
  (define-key vterm-copy-mode-map (kbd "C-g") #'nh/vterm-cancel-selection))

;; Selecting in vterm only works in copy mode, because redraws move point back
;; to the terminal cursor and normal keys go to the shell.
(defun nh/vterm-start-selection ()
  "Enter `vterm-copy-mode' and set the mark at point."
  (interactive)
  (vterm-copy-mode 1)
  (set-mark-command nil))

(defun nh/vterm-mouse-select (event)
  "Enter `vterm-copy-mode' and start a mouse selection at EVENT."
  (interactive "e")
  (vterm-copy-mode 1)
  (mouse-drag-region event))

(defun nh/vterm-cancel-selection ()
  "Leave `vterm-copy-mode' without copying."
  (interactive)
  (deactivate-mark)
  (vterm-copy-mode -1))

;; Multi Vterm: Manage multiple vterm buffers
;; Provides enhanced management for multiple vterm instances with easy
;; switching between terminals and project-specific terminal sessions.
;; GitHub: https://github.com/suonlight/multi-vterm
(use-package multi-vterm
  :ensure t
  :bind (("C-c V" . multi-vterm))
  :config
  ;; Keybindings for multi-vterm navigation and creation
  (define-key vterm-mode-map (kbd "C-c n") 'multi-vterm-next)
  (define-key vterm-mode-map (kbd "C-c p") 'multi-vterm-prev)
  (define-key vterm-mode-map (kbd "C-c c") 'multi-vterm))

(provide 'nh-terminal)
;;; nh-terminal.el ends here
