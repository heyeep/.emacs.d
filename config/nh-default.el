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
;; Ensures Emacs inherits the correct PATH and environment variables from your
;; shell, crucial for GUI Emacs and tools managed by asdf, nvm, pyenv, etc.
;; GitHub: https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

(exec-path-from-shell-copy-envs '("PATH" "AIDER_API_KEY" "OPENAI_API_KEY"))

;; Keycast: Show current command and its key in the mode line
;; Displays pressed keys and the corresponding commands in the mode line,
;; useful for presentations, screen recordings, and learning key bindings.
;; GitHub: https://github.com/tarsius/keycast
(use-package keycast
  :ensure t
  :diminish keycast-mode
  :config
  ;; Optionally enable keycast by default (can be toggled with keycast-mode)
  ;; (keycast-mode-line-mode 1)
  )

;; Which Key: Show available keybindings in popup
;; Displays available key combinations in a popup when you start typing a
;; key sequence, helping discover and remember complex keybindings.
;; GitHub: https://github.com/justbur/emacs-which-key
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode 1)
  (setq which-key-idle-delay 0.3)
  (setq which-key-popup-type 'side-window)
  (setq which-key-side-window-max-height 0.25)
  (setq which-key-show-early-on-C-h t)
  (setq which-key-max-description-length 25)
  (setq which-key-sort-order #'which-key-key-order-alpha))

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

;; Beacon: A light that follows your cursor around so you don't lose it
;; Highlights the cursor position with a brief light flash whenever the window
;; scrolls or you switch windows, making it easy to track cursor location.
;; GitHub: https://github.com/Malabarba/beacon
(use-package beacon
  :ensure t
  :diminish beacon-mode
  :config
  (beacon-mode 1)
  ;; Configure beacon appearance
  (setq beacon-size 40)
  (setq beacon-blink-when-point-moves-vertically 10)
  (setq beacon-blink-when-window-scrolls t)
  (setq beacon-blink-when-window-changes t)
  (setq beacon-blink-when-focused t))

;; Expand Region: Increase selected region by semantic units
;; Intelligently expands the selected region based on semantic units like
;; words, sentences, expressions, and code blocks for efficient text selection.
;; GitHub: https://github.com/magnars/expand-region.el
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;; Ace Window: Navigate between windows using overlays
;; Provides quick window switching by displaying overlay characters, making
;; it easy to jump between multiple windows with a single key press.
;; GitHub: https://github.com/abo-abo/ace-window
(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-background nil))

;; Vundo: Visual undo tree navigation
;; Provides a visual representation of undo history as a tree, allowing you
;; to navigate complex edit histories and recover any previous buffer state.
;; GitHub: https://github.com/casouri/vundo
(use-package vundo
  :ensure t
  :bind (("C-x u" . vundo))
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols))

;; Increase undo limits for a more robust undo experience
(setq undo-limit 160000)
(setq undo-strong-limit 240000)
(setq undo-outer-limit 24000000)

;; Reveal in OSX Finder: Open current file or directory in Finder
;; Provides commands to reveal the current file or directory in macOS Finder,
;; useful for quick access to files in the native file manager.
;; GitHub: https://github.com/kaz-yos/reveal-in-osx-finder
(use-package reveal-in-osx-finder
  :ensure t
  :if (eq system-type 'darwin)
  :bind (("C-c z" . reveal-in-osx-finder)))

(provide 'nh-default)
;;; nh-default.el ends here
