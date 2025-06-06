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

;; Exec Path From Shell: Make Emacs use the $PATH set up by the user's shell
;; Ensures Emacs inherits the correct PATH and environment variables from the
;; user's shell, especially important for GUI Emacs and tools managed by asdf.
;; GitHub: https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))

(exec-path-from-shell-copy-envs '("PATH" "AIDER_API_KEY" "OPENAI_API_KEY"))

;; Keycast: Show current command and its key in the mode line
;; Displays the keys you press and the commands they invoke in the header line,
;; useful for presentations, tutorials, and learning new keybindings.
;; GitHub: https://github.com/tarsius/keycast
(use-package keycast
  :ensure t
  :config
  (keycast-header-line-mode 1))

;; Which Key: Show available keybindings in popup
;; Displays a popup showing all available key completions when you pause after
;; typing a prefix key, making it easier to discover and learn keybindings.
;; GitHub: https://github.com/justbur/emacs-which-key
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
(setq display-line-numbers-type 'absolute)

;;; Disable line numbers in terminal and shell modes
(dolist (mode '(term-mode shell-mode eshell-mode vterm-mode))
  (add-hook (intern (concat (symbol-name mode) "-hook"))
            (lambda () (display-line-numbers-mode 0))))

;;; Disable electric-indent-mode globally (no auto-indent by default)
(electric-indent-mode 0)

;;; Always enable electric-indent-local-mode in programming and markup modes
;; This ensures automatic indentation is always active in all relevant buffers.
(dolist (hook '(prog-mode-hook
                yaml-mode-hook
                css-mode-hook
                html-mode-hook
                nxml-mode-hook))
  (add-hook hook (lambda () (electric-indent-local-mode 1))))

(add-hook 'prog-mode-hook 'eldoc-mode)

;;; Speed up display of large fonts (at the cost of higher memory usage)
(setq inhibit-compacting-font-caches t)

(setq native-comp-async-report-warnings-errors nil)

;; Beacon: A light that follows your cursor around so you don't lose it!
;; Provides visual feedback by briefly highlighting your cursor position when
;; the window scrolls, making it easy to track your cursor after large movements.
;; GitHub: https://github.com/Malabarba/beacon
(use-package beacon
  :ensure t
  :config
  (beacon-mode 1))

;; Expand Region: Quickly expand the selected region by semantic units
;; Intelligently expands the region around point by semantic units like words,
;; symbols, quotes, sentences, paragraphs, and code blocks with repeated presses.
;; GitHub: https://github.com/magnars/expand-region.el
(use-package expand-region
  :ensure t
  :bind ("C-;" . er/expand-region))

;; Ace Window: Fast window switching and management
;; Provides quick window navigation by assigning letters to each window,
;; allowing you to jump to any window with just a few keystrokes.
;; GitHub: https://github.com/abo-abo/ace-window
(use-package ace-window
  :ensure t
  :commands (ace-delete-window
             ace-swap-window
             ace-delete-other-windows
             ace-window
             aw-select)
  :bind (("M-o" . ace-window)))

;; Vundo: Modern visual undo tree (C-x u to launch)
;; Provides a visual tree representation of your undo history, allowing you to
;; navigate and restore any previous state of your buffer with precision.
;; GitHub: https://github.com/casouri/vundo
(use-package vundo
  :ensure t
  :bind (("C-x u" . vundo))
  :config
  ;; Use a more compact character set for the tree
  (setq vundo-glyph-alist vundo-unicode-symbols)
  ;; Optionally, set the window size
  (setq vundo-window-max-height 20))

;; Increase undo limits for a more robust undo experience
(setq undo-limit 160000)
(setq undo-strong-limit 240000)
(setq undo-outer-limit 24000000)

;; Reveal in OSX Finder: Reveal buffer-associated file in macOS Finder
;; Opens the current file's location in macOS Finder, useful for quickly
;; accessing the file system from within Emacs on macOS systems.
;; GitHub: https://github.com/kaz-yos/reveal-in-osx-finder
(use-package reveal-in-osx-finder
    :ensure t)

(provide 'nh-default)
;;; nh-default.el ends here
