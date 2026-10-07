;;; nh-terminal.el --- terminal -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; vterm compiles a native module on first load and needs Homebrew's tools on PATH.
(setenv "PATH" (concat "/opt/homebrew/bin:/opt/homebrew/sbin:" (getenv "PATH")))
(setenv "SHELL" "/bin/zsh")

(setq vterm-cmake-path "/opt/homebrew/bin/cmake")

;; vterm: a full terminal inside Emacs, built on libvterm.
;; GitHub: https://github.com/akermu/emacs-libvterm
(use-package vterm
  :ensure t
  :commands vterm
  ;; Not C-c t, which belongs to consult-theme.
  :bind (("C-c v" . vterm))
  :config
  (setq vterm-shell "/bin/zsh")
  (setq vterm-max-scrollback 200000)
  ;; Command is Meta here, so Cmd+C/Cmd+V arrive as M-c/M-v, which vterm
  ;; would otherwise pass to the shell instead of copying and pasting.
  (define-key vterm-mode-map (kbd "M-c") #'ignore)
  (define-key vterm-mode-map (kbd "M-v") #'vterm-yank)
  (define-key vterm-mode-map (kbd "C-SPC") #'nh/vterm-start-selection)
  (define-key vterm-mode-map [down-mouse-1] #'nh/vterm-mouse-select)
  (define-key vterm-copy-mode-map (kbd "M-w") #'vterm-copy-mode-done)
  (define-key vterm-copy-mode-map (kbd "M-c") #'vterm-copy-mode-done)
  (define-key vterm-copy-mode-map (kbd "C-g") #'nh/vterm-cancel-selection)
  (define-key vterm-copy-mode-map [mouse-1] #'nh/vterm-mouse-click))

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

(defun nh/vterm-mouse-click (event)
  "Move point to EVENT, and leave `vterm-copy-mode' if nothing is selected.
A plain click to focus the window then doesn't leave the terminal frozen."
  (interactive "e")
  (mouse-set-point event)
  (unless (use-region-p)
    (nh/vterm-cancel-selection)))

(defun nh/vterm-cancel-selection ()
  "Leave `vterm-copy-mode' without copying."
  (interactive)
  (deactivate-mark)
  (vterm-copy-mode -1))

;; multi-vterm: open and switch between several vterm buffers.
;; GitHub: https://github.com/suonlight/multi-vterm
(use-package multi-vterm
  :ensure t
  :bind (("C-c V" . multi-vterm))
  :config
  (define-key vterm-mode-map (kbd "C-c n") 'multi-vterm-next)
  (define-key vterm-mode-map (kbd "C-c p") 'multi-vterm-prev)
  (define-key vterm-mode-map (kbd "C-c c") 'multi-vterm))

(provide 'nh-terminal)
;;; nh-terminal.el ends here
