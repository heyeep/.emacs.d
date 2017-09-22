;;;; -*- lexical-binding: t; -*-

;; Right Alt -> Control
(setq ns-right-option-modifier 'control)

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x C-g") 'magit-status)

;; Neotree
;; (global-set-key (kbd "<f8>") 'neotree-toggle)

;; Dired Sidebar
(global-set-key (kbd "<f8>") 'dired-sidebar-toggle-sidebar)
;; Other
(global-set-key (kbd "<f9>") (lambda() (interactive) (find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "<f7>") (lambda() (interactive) (find-file "~/Code/flappyworld/c/flappy/scene/GameScene.cpp")))

;; Don't let osx swallow Meta key.
(setq mac-pass-command-to-system nil)

;; Command -> Meta
;; Option -> Super
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)

(add-hook 'c++-mode-hook 'hiepc++-mode-hook)

(defun hiepc++-mode-hook ()
   (define-key c++-mode-map (kbd "<f3>") #'compile))

(global-set-key (kbd "<f4>") (lambda () (interactive) (shell-command "./app")))

(provide 'yzm-keybindings)
