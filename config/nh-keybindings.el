;;; nh-keybindings.el --- keybindings -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'nh-env)
(require 'nh-helpers)

(global-set-key (kbd "C-x |") #'nh/toggle-window-split)
(global-set-key (kbd "<f8>") #'nh/sidebar-toggle)

;; Ace-Window
(global-set-key (kbd "M-p") 'ace-window)
(windmove-default-keybindings)

;; counsel-git-search
(global-set-key (kbd "C-c C-f") 'counsel-git)
(global-set-key (kbd "C-c C-s") 'counsel-git-grep)

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x C-g") 'magit-status)

;; Searching
(global-unset-key (kbd "s-p"))
(global-set-key (kbd "s-p f") 'projectile-find-file)
(global-set-key (kbd "s-p s") 'projectile-ag)

;; On macOS, set the Command key to act as Meta and Option key to act as Super
(when nh-env/is-mac
  (setq ns-command-modifier 'meta)    ; Remap Command to Meta on macOS
  (setq ns-left-option-modifier 'super)    ; Remap Option to Super on macOS
  (setq ns-right-option-modifier 'super) ; Right Option as Super on macOS
  (setq mac-pass-command-to-system nil)) ; Let Emacs handle Command key, not macOS

;; Ensure C-g always works as keyboard-quit
(global-set-key (kbd "C-g") 'keyboard-quit)

;; Make sure C-g works in minibuffer
(define-key minibuffer-local-map (kbd "C-g") 'abort-recursive-edit)
(define-key minibuffer-local-ns-map (kbd "C-g") 'abort-recursive-edit)
(define-key minibuffer-local-completion-map (kbd "C-g") 'abort-recursive-edit)
(define-key minibuffer-local-must-match-map (kbd "C-g") 'abort-recursive-edit)
(define-key minibuffer-local-isearch-map (kbd "C-g") 'abort-recursive-edit)

;; Make Shift+Enter insert a newline
(global-set-key (kbd "S-<return>") 'newline)

(provide 'nh-keybindings)
;;; nh-keybindings.el ends here
