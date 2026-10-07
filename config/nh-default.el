;;; nh-default.el --- default -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; setq-default sets the default of buffer-local variables like tab-width.
;; Plain setq would only change the current buffer.
(setq-default
 indent-tabs-mode nil
 tab-width 4
 c-basic-offset 4)
(setq-default truncate-lines t)

(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

(setq auto-save-default nil)

(setq vc-make-backup-files nil)

(setq inhibit-startup-screen t)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq initial-buffer-choice nil)

(setq initial-scratch-message nil)

(setq ring-bell-function 'ignore)

(setq frame-title-format
      '(:eval (if (buffer-file-name)
                  (abbreviate-file-name (buffer-file-name))
                "%b")))

;; keycast: shows each key you press and the command it runs.
;; GitHub: https://github.com/tarsius/keycast
(use-package keycast
  :ensure t
  :config
  (keycast-header-line-mode 1))

;; which-key: after a prefix key, lists the keys that can follow it.
;; GitHub: https://github.com/justbur/emacs-which-key
(use-package which-key
  :ensure t
  :diminish
  :config
  (setq which-key-popup-type 'minibuffer)
  (setq which-key-idle-delay 0.3)
  (setq which-key-sort-order 'which-key-key-order-alpha)
  (which-key-mode)
  (which-key-show-top-level))
;;  (which-key-show-keymap 'org-mode-map))

(setq kill-whole-line t)

(fset 'yes-or-no-p 'y-or-n-p)

(column-number-mode 1)

(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'absolute)
;; Size the number column once, so it doesn't shift while scrolling.
(setq display-line-numbers-width-start t)
(setq display-line-numbers-grow-only t)

(dolist (mode '(term-mode shell-mode eshell-mode vterm-mode))
  (add-hook (intern (concat (symbol-name mode) "-hook"))
            (lambda () (display-line-numbers-mode 0))))

(electric-indent-mode 0)

;; Auto-indent only in code and markup buffers, not in plain text.
(dolist (hook '(prog-mode-hook
                yaml-mode-hook
                css-mode-hook
                html-mode-hook
                nxml-mode-hook))
  (add-hook hook (lambda () (electric-indent-local-mode 1))))

(add-hook 'prog-mode-hook 'eldoc-mode)

;; Faster display with large fonts, at the cost of more memory.
(setq inhibit-compacting-font-caches t)

(setq native-comp-async-report-warnings-errors nil)

;; expand-region: grows the selection to the next word, string, block and so on.
;; GitHub: https://github.com/magnars/expand-region.el
(use-package expand-region
  :ensure t
  ;; Not C-;, which belongs to embark-dwim.
  :bind ("C-=" . er/expand-region))

;; ace-window: jump to a window by typing the letter shown in it.
;; GitHub: https://github.com/abo-abo/ace-window
(use-package ace-window
  :ensure t
  :commands (ace-delete-window
             ace-swap-window
             ace-delete-other-windows
             ace-window
             aw-select)
  :bind (("M-o" . ace-window)))

;; undo-tree: shows undo history as a tree you can walk through.
;; GitHub: https://github.com/apchamberlain/undo-tree.el
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :demand t
  :bind (("C-x u" . undo-tree-visualize))
  :config
  (global-undo-tree-mode)
  ;; Don't write undo history files next to every edited file.
  (setq undo-tree-auto-save-history nil)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t))

(setq undo-limit 160000)
(setq undo-strong-limit 240000)
(setq undo-outer-limit 24000000)

;; reveal-in-osx-finder: shows the current file in Finder.
;; GitHub: https://github.com/kaz-yos/reveal-in-osx-finder
(use-package reveal-in-osx-finder
    :ensure t)

;; minimap: a small overview of the whole buffer in a side window.
;; GitHub: https://github.com/dengste/minimap
(use-package minimap
    :ensure t
    :commands (minimap-mode minimap-create minimap-kill)
    :config
    (setq minimap-window-location 'right)
    (setq minimap-minimum-width 20)
    (setq minimap-update-delay 0.1)
    (setq minimap-highlight-line t))


;; highlight-indent-guides: draws a line at each indentation level.
;; GitHub: https://github.com/DarthFennec/highlight-indent-guides
(use-package highlight-indent-guides
    :ensure t
    :hook (prog-mode . highlight-indent-guides-mode)
    :config
    (setq highlight-indent-guides-method 'column)
    (setq highlight-indent-guides-character ?\|)
    ;; Guide colors are derived from the theme's colors.
    (setq highlight-indent-guides-auto-character-face-perc 30)
    (setq highlight-indent-guides-auto-top-character-face-perc 50)
    (setq highlight-indent-guides-responsive nil)
    (setq highlight-indent-guides-suppress-auto-error t))
;; Show ANSI color codes as colors in compilation and shell output.
(require 'compile)
(setq compilation-scroll-output t)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

(defun nh/compilation-mode-colorize ()
  "Colorize compilation buffer."
  (when (eq major-mode 'compilation-mode)
    (ansi-color-apply-on-region compilation-filter-start (point-max))))

(add-hook 'compilation-filter-hook 'nh/compilation-mode-colorize)

(when (not (display-graphic-p))
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'alt)
  (set-input-meta-mode t)
  (set-terminal-coding-system 'utf-8)
  (unless (getenv "EMACS_TERM_META_SENDS_ESCAPE")
    (set-input-meta-mode t)))

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'alt))

(provide 'nh-default)

;;; nh-default.el ends here
