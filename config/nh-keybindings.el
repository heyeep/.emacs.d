;;; nh-keybindings.el --- Keybindings configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for custom keybindings.

;;; Code:

(require 'nh-env)
(require 'nh-helpers)

(global-set-key (kbd "C-x |") #'nh/toggle-window-split)
(global-set-key (kbd "<f8>") #'nh/sidebar-toggle)

;; On macOS, set the Command key to act as Meta and Option key to act as Super
(when nh-env/is-mac
  (setq ns-command-modifier 'meta)    ; Remap Command to Meta on macOS
  (setq ns-option-modifier 'super)    ; Remap Option to Super on macOS
  (setq ns-right-option-modifier 'control) ; Right Option as Control on macOS
  (setq mac-pass-command-to-system nil)) ; Let Emacs handle Command key, not macOS

(provide 'nh-keybindings)
;;; nh-keybindings.el ends here 
