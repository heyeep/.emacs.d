;;;; -*- lexical-binding: t; -*-

;; Right Alt -> Control
(setq ns-right-option-modifier 'control)

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)

;; Neotree
(global-set-key [f8] 'neotree-toggle)

;; Don't let osx swallow Meta key.
(setq mac-pass-command-to-system nil)

;; Command -> Meta
;; Option -> Super
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)


(provide 'yzm-keybindings)
