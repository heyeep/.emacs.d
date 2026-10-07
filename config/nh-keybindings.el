;;; nh-keybindings.el --- keybindings -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'nh-env)
(require 'nh-helpers)

(global-set-key (kbd "C-x |") #'nh/toggle-window-split)
(global-set-key (kbd "<f8>") #'nh/sidebar-toggle)

(global-set-key (kbd "M-p") 'ace-window)
(windmove-default-keybindings)

(global-set-key (kbd "C-c C-f") 'counsel-git)
(global-set-key (kbd "C-c C-s") 'counsel-git-grep)

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x C-g") 'magit-status)

(global-unset-key (kbd "s-p"))
(global-set-key (kbd "s-p f") 'projectile-find-file)
(global-set-key (kbd "s-p s") 'projectile-ag)

;; macOS: Command acts as Meta and Option as Super.
(when nh-env/is-mac
  (setq ns-command-modifier 'meta)
  (setq ns-left-option-modifier 'super)
  (setq ns-right-option-modifier 'super)
  (setq mac-pass-command-to-system nil))

(global-set-key (kbd "C-g") 'keyboard-quit)

;; Some packages rebind C-g in the minibuffer; force it back to quitting.
(define-key minibuffer-local-map (kbd "C-g") 'abort-recursive-edit)
(define-key minibuffer-local-ns-map (kbd "C-g") 'abort-recursive-edit)
(define-key minibuffer-local-completion-map (kbd "C-g") 'abort-recursive-edit)
(define-key minibuffer-local-must-match-map (kbd "C-g") 'abort-recursive-edit)
(define-key minibuffer-local-isearch-map (kbd "C-g") 'abort-recursive-edit)

(global-set-key (kbd "S-<return>") 'newline)

(provide 'nh-keybindings)
;;; nh-keybindings.el ends here
