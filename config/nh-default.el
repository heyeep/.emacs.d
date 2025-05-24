;;; nh-default.el --- Default Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Basic Emacs settings and configurations.

;;; Code:

;;; Use setq-default for buffer-local variables (like tab-width, indent-tabs-mode, etc.) to set their default for all
;;; buffers. Use setq for global variables or to set a variable in the current buffer only.
;;; Set global indentation defaults
(setq-default
 indent-tabs-mode nil   ; Use spaces instead of tabs
 tab-width 4            ; Display tab characters as 4 spaces wide
 c-basic-offset 4)      ; Indent C-like languages with 4 spaces per level
(setq-default truncate-lines t)

;;; Automatically reload buffers when files change on disk
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t) ; Also auto-revert dired and other non-file buffers
(setq auto-revert-verbose nil)               ; Don't show messages when reverting

;;; Disable auto-save by default
(setq auto-save-default nil)

;;; Don't make backup files for version-controlled files
(setq vc-make-backup-files nil)

;;; Disable the startup screen
(setq inhibit-startup-screen t)

;;; Mute the system bell
(setq ring-bell-function 'ignore)

;;; Set the frame title to the current file or buffer name
(setq frame-title-format
      '(:eval (if (buffer-file-name)
                  (abbreviate-file-name (buffer-file-name))
                "%b")))

;;; Show pressed keys and commands in the header line using keycast
(use-package keycast
  :ensure t
  :config
  (keycast-header-line-mode 1))

;;; Show possible key combinations as you type using which-key
(use-package which-key
  :ensure t
  :diminish
  :config
  ;; Show completions in the minibuffer instead of a popup
  (setq which-key-popup-type 'minibuffer)
  ;; Decrease the delay before which-key shows completions
  (setq which-key-idle-delay 0.3)
  ;; Sort keybindings alphabetically by key
  (setq which-key-sort-order 'which-key-key-order-alpha)
  (which-key-mode)
  (which-key-show-top-level))
;;  (which-key-show-keymap 'org-mode-map))

;;; Make C-k kill the whole line, including the newline
(setq kill-whole-line t)

;;; Allow y/n instead of yes/no for prompts
(fset 'yes-or-no-p 'y-or-n-p)

;;; Show column numbers in the mode line
(column-number-mode 1)

;;; Show line numbers in most buffers
(global-display-line-numbers-mode 1)

;;; Disable line numbers in terminal and shell modes
(dolist (mode '(term-mode shell-mode eshell-mode vterm-mode))
  (add-hook (intern (concat (symbol-name mode) "-hook"))
            (lambda () (display-line-numbers-mode 0))))

;;; Disable electric-indent-mode globally (no auto-indent by default)
(electric-indent-mode 0)

;;; Enable electric-indent-local-mode in programming and markup modes
(dolist (hook '(prog-mode-hook
                yaml-mode-hook
                css-mode-hook
                html-mode-hook
                nxml-mode-hook))
  (add-hook hook #'electric-indent-local-mode))

;;; Speed up display of large fonts (at the cost of higher memory usage)
(setq inhibit-compacting-font-caches t)

(setq native-comp-async-report-warnings-errors nil)

(provide 'nh-default)
;;; nh-default.el ends here 
