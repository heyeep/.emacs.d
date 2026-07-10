;;; nh-default.el --- default -*- lexical-binding: t; -*-

;;; Commentary:

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
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq initial-buffer-choice nil)

;;; Disable scratch buffer message
(setq initial-scratch-message nil)

;;; Mute the system bell
(setq ring-bell-function 'ignore)

;;; Set the frame title to the current file or buffer name
(setq frame-title-format
      '(:eval (if (buffer-file-name)
                  (abbreviate-file-name (buffer-file-name))
                "%b")))

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
;; Make line numbers fill the width without padding
(setq display-line-numbers-width-start t)
(setq display-line-numbers-grow-only t)

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

;; Expand Region: Quickly expand the selected region by semantic units
;; Intelligently expands the region around point by semantic units like words,
;; symbols, quotes, sentences, paragraphs, and code blocks with repeated presses.
;; GitHub: https://github.com/magnars/expand-region.el
(use-package expand-region
  :ensure t
  ;; C-=, not C-;: embark-dwim owns C-; (nh-autocompletion loads later
  ;; and won that conflict silently).
  :bind ("C-=" . er/expand-region))

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

;; Undo Tree: Visual undo interface for better undo management
;; Provides a visual tree-like interface for Emacs' undo system, allowing you to
;; navigate and restore any previous state of your buffer with precision.
;; GitHub: https://github.com/apchamberlain/undo-tree.el
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :bind (("C-x u" . undo-tree-visualize))
  :config
  ;; Enable undo-tree globally
  (global-undo-tree-mode)
  ;; Prevent undo tree files from cluttering the file system
  (setq undo-tree-auto-save-history nil)
  ;; Show timestamps in the undo tree
  (setq undo-tree-visualizer-timestamps t)
  ;; Show differences between states
  (setq undo-tree-visualizer-diff t))

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

;; Minimap: Display a minimap of the buffer on the side
;; Provides a VS Code-style minimap showing an overview of the entire buffer,
;; useful for navigating large files and getting a bird's eye view of code structure.
;; GitHub: https://github.com/dengste/minimap
(use-package minimap
    :ensure t
    :commands (minimap-mode minimap-create minimap-kill)
    :config
    ;; Set the width of the minimap window
    (setq minimap-window-location 'right)
    ;; Only show the minimap for files larger than this many lines
    (setq minimap-minimum-width 20)
    ;; Update minimap when scrolling
    (setq minimap-update-delay 0.1)
    ;; Show current line highlight in minimap
    (setq minimap-highlight-line t))


;; Highlight Indent Guides: Simple and reliable indentation guides
;; Shows subtle vertical lines to indicate indentation levels
;; GitHub: https://github.com/DarthFennec/highlight-indent-guides
(use-package highlight-indent-guides
    :ensure t
    :hook (prog-mode . highlight-indent-guides-mode)
    :config
    ;; Use column method for clean vertical lines
    (setq highlight-indent-guides-method 'column)
    ;; Use pipe character for column guides
    (setq highlight-indent-guides-character ?\|)  ; Pipe character
    ;; Auto-detect colors from current theme
    (setq highlight-indent-guides-auto-character-face-perc 30)
    (setq highlight-indent-guides-auto-top-character-face-perc 50)
    ;; Responsive guides (highlight current context)
    (setq highlight-indent-guides-responsive nil)
    ;; Show guides on blank lines for better continuity
    (setq highlight-indent-guides-suppress-auto-error t))
;; Configure compilation buffers for better ANSI color support
(require 'compile)
(setq compilation-scroll-output t)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

;; Make compilation buffers handle ANSI color codes
(defun nh/compilation-mode-colorize ()
  "Colorize compilation buffer."
  (when (eq major-mode 'compilation-mode)
    (ansi-color-apply-on-region compilation-filter-start (point-max))))

(add-hook 'compilation-filter-hook 'nh/compilation-mode-colorize)

;; Terminal-specific configurations
(when (not (display-graphic-p))
  ;; Use Command as Meta in terminal
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'alt)
  ;; For other terminals, ensure 8-bit input
  (set-input-meta-mode t)
  (set-terminal-coding-system 'utf-8)
  ;; Ensure terminal sends proper Meta sequences
  (unless (getenv "EMACS_TERM_META_SENDS_ESCAPE")
    (set-input-meta-mode t)))

;; Also set Command as Meta for GUI Emacs
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'alt))

(provide 'nh-default)

;;; nh-default.el ends here
