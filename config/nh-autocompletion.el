;;; nh-autocompletion.el --- autocomplete -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Custom group for our configuration
(defgroup nh-autocompletion nil
  "Autocompletion configuration with global file exclusions."
  :group 'convenience
  :prefix "nh-")

(defcustom nh/globally-ignored-directories
  '("node_modules" "dist" "build" "target" "__pycache__")
  "Directories to exclude globally from all file operations in Emacs.
These directories are excluded from:
- Consult find/fd/ripgrep operations
- Projectile file discovery
- Global completion systems"
  :type '(repeat string)
  :group 'nh-autocompletion)

(defcustom nh/globally-ignored-file-extensions
  '(".elc" ".pyc" ".min.js" ".min.css" ".bunqdle.js" ".chunk.js")
  "File extensions to exclude globally from completion.
These are added to `completion-ignored-extensions'."
  :type '(repeat string)
  :group 'nh-autocompletion)

;; Command builders for different tools
(defun nh--build-find-command ()
  "Build find command arguments as a list for consult-find."
  (append
   '("find" "." "-type" "f")
   (mapcan (lambda (dir)
             (list "-not" "-path" (format "*/%s/*" dir)))
           nh/globally-ignored-directories)))

(defun nh--build-fd-command ()
  "Build fd command arguments as a list for consult-fd."
  (append
   '("fd" "--type" "f" "--hidden" "--follow" "--color=never")
   (mapcan (lambda (dir) (list "--exclude" dir))
           nh/globally-ignored-directories)))

(defun nh--build-ripgrep-command ()
  "Build ripgrep command arguments as a list for consult-ripgrep."
  (append
   '("rg" "--null" "--line-buffered" "--color=never" "--max-columns=1000"
     "--path-separator=/" "--smart-case" "--no-heading" "--line-number")
   (mapcan (lambda (dir) (list "--glob" (format "!%s" dir)))
           nh/globally-ignored-directories)))

;; Apply global exclusions to completion systems
(defun nh--configure-global-completion ()
  "Configure global file completion to respect our exclusion patterns."
  ;; Add file extensions to global ignore list
  (setq completion-ignored-extensions
        (append completion-ignored-extensions nh/globally-ignored-file-extensions))

  ;; Configure ido if present
  (when (boundp 'ido-ignore-directories)
    (setq ido-ignore-directories
          (append ido-ignore-directories nh/globally-ignored-directories))))

;; Helper function for building shell command strings (for projectile)
(defun nh--build-fd-shell-command ()
  "Build fd shell command string for projectile."
  (mapconcat (lambda (dir) (format "--exclude %s" dir))
             nh/globally-ignored-directories " "))

(defun nh--build-ripgrep-shell-command ()
  "Build ripgrep shell command string for projectile."
  (mapconcat (lambda (dir) (format "--glob '!%s'" dir))
             nh/globally-ignored-directories " "))

;; Initialize global completion configuration
(nh--configure-global-completion)

;; Vertico: Vertical interactive completion
;; Provides a minimalist vertical completion interface with fast fuzzy matching,
;; better than ivy/helm with cleaner UI and excellent performance for selection.
;; GitHub: https://github.com/minad/vertico
(use-package vertico
  :ensure t
  :demand
  :init
  ;; Show completions immediately without any input
  (setq completion-auto-select 'second-tab)  ;; Don't auto-select, but show candidates
  (setq completion-show-help t)              ;; Show completions immediately
  (setq completion-auto-help t)              ;; Automatically show completions
  (defun  nh/vertico-insert ()
    (interactive)
    (let* ((mb (minibuffer-contents-no-properties))
           (lc (if (string= mb "") mb (substring mb -1))))
      (cond ((string-match-p "^[/~:]" lc) (self-insert-command 1 ?/))
            ((file-directory-p (vertico--candidate)) (vertico-insert))
            (t (self-insert-command 1 ?/)))))
  :config
  (setq vertico-cycle t)
  ;; currently requires melpa version of vertico
  (setq vertico-preselect 'directory)
  (setq vertico-count 15)  ;; Show up to 15 candidates like ivy-height
  (setq vertico-resize t)  ;; Dynamically resize minibuffer
  (vertico-mode)  ;; Enable vertico after package is loaded

  ;; Load and configure directory extension
  (require 'vertico-directory)
  (define-key vertico-map (kbd "DEL") #'vertico-directory-delete-char)
  (define-key vertico-map (kbd "M-DEL") #'vertico-directory-delete-word)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

  :bind (:map vertico-map
              ("/" . #'nh/vertico-insert)
              ("TAB" . #'vertico-next)       ;; TAB cycles to next candidate
              ("S-TAB" . #'vertico-previous) ;; Shift-TAB cycles to previous candidate
              ("C-j" . #'vertico-exit)       ;; Like ivy-done
              ("RET" . #'vertico-directory-enter) ;; Like ivy-alt-done for directories
              ("M-p" . #'vertico-previous)   ;; Explicit previous for history navigation
              ("M-n" . #'vertico-next)))     ;; Explicit next for history navigation

;; Orderless: Advanced completion style
;; Provides flexible completion matching with space-separated components that
;; can match in any order, supporting regexp, literal, and fuzzy matching styles.
;; GitHub: https://github.com/oantolin/orderless
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless flex basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))))
  (orderless-smart-case t)
  (orderless-component-separator #'orderless-escapable-split-on-space))

;; Marginalia: Rich annotations in the minibuffer
;; Adds helpful annotations and metadata to minibuffer completions, showing file
;; sizes, documentation strings, and other contextual information for candidates.
;; GitHub: https://github.com/minad/marginalia
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode)  ;; Enable after package is loaded
  :custom
  (marginalia-align 'right)
  (marginalia-max-relative-age 0))

;; Persist history over Emacs restarts - Direct configuration to ensure it works
(require 'savehist)
(setq history-length 1000)
(setq history-delete-duplicates t)        ;; Remove duplicates from history
(setq savehist-save-minibuffer-history t) ;; Save all minibuffer histories
(setq savehist-autosave-interval 300)     ;; Autosave every 5 minutes

;; Additional history variables to save
(setq savehist-additional-variables
      '(mark-ring                          ;; Mark ring history
        global-mark-ring                   ;; Global mark ring
        search-ring                        ;; Search history
        regexp-search-ring                 ;; Regexp search history
        extended-command-history           ;; M-x command history
        file-name-history                  ;; File name history
        buffer-name-history                ;; Buffer name history
        minibuffer-history                 ;; General minibuffer history
        query-replace-history              ;; Query replace history
        read-expression-history            ;; Expression evaluation history
        org-read-date-history              ;; Org mode date history
        kill-ring))                        ;; Kill ring (clipboard history)

;; Force enable savehist-mode
(savehist-mode 1)

;; Recent F: Track recently opened files
;; Maintains a list of recently opened files for quick access, with customizable
;; cleanup and size limits to improve file navigation workflow.
(use-package recentf
  :init
  (recentf-mode 1)
  (setq recentf-max-saved-items 1000
        recentf-auto-cleanup 'never))

;; Embark: Context menu and actions
;; Provides context-aware actions and menus for completion candidates, allowing
;; you to perform operations on selected items with customizable action sets.
;; GitHub: https://github.com/oantolin/embark
(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         ;; main action menu
   ("C-;" . embark-dwim)        ;; smarter default actions
   ("C-h B" . embark-bindings)) ;; describe bindings
  :init
  ;; Use Embark to show contextual help
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Optionally replace `describe-bindings` with `embark`
  (setq embark-verbose-indicator-display-action
        '(display-buffer-at-bottom))

  (setq embark-action-indicator
        (lambda (map)
          (which-key--show-keymap "Embark Actions" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command))
  (setq embark-become-indicator embark-action-indicator))

;; Embark Consult: Consult integration for Embark
;; Integrates Embark with Consult to provide enhanced actions for search results
;; and completion candidates, enabling seamless workflow between the two packages.
;; GitHub: https://github.com/oantolin/embark
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Consult: Useful search and navigation commands
;; Provides a comprehensive set of search and navigation commands with live
;; preview, enhanced with filtering, grouping, and integration with completion systems.
;; GitHub: https://github.com/minad/consult
(use-package consult
  :ensure t
  :bind (
         ;; Replace standard commands with Consult equivalents (like Counsel did)
         ;; Note: M-x is automatically enhanced by consult when present
         ("C-x b" . consult-buffer)          ;; Enhanced switch-buffer
         ("C-c r" . consult-recent-file)     ;; counsel-recentf equivalent

         ;; Search commands (replaces Swiper)
         ("C-s" . consult-line)              ;; swiper equivalent

         ;; File finding and project commands
         ("C-c f" . consult-find)            ;; Find files by name/path (like counsel-find-file enhancement)
         ("C-c d" . consult-fd)              ;; Alternative fast find (if fd is available)
         ("C-c l" . consult-locate)          ;; Locate files (counsel-locate equivalent)

         ;; Grep and search commands
         ("C-c k" . consult-ripgrep)         ;; counsel-rg equivalent
         ("C-c g" . consult-grep)            ;; counsel-ag equivalent
         ("C-c G" . consult-git-grep)        ;; counsel-git-grep equivalent

         ;; Additional consult commands
         ("C-M-l" . consult-imenu)           ;; Enhanced imenu
         ("M-y" . consult-yank-pop)          ;; Enhanced yank-pop
         ("C-c h" . consult-history)         ;; History for current buffer
         ("C-c m" . consult-man)             ;; Man pages
         ("C-c i" . consult-info)            ;; Info search
         ("C-c o" . consult-outline)         ;; Navigate headings
         ("C-c t" . consult-theme)           ;; Theme selection with preview

         ;; Resume functionality
         ("C-c C-r" . consult-history)       ;; Use history as resume-like functionality
         )

  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  ;; Configure xref to use consult (only if consult is available)
  (when (fboundp 'consult-xref)
    (setq xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref))

  :config
  ;; Configure consult search command arguments with comprehensive exclusions
  (setq consult-find-args (nh--build-find-command))
  (setq consult-fd-args (nh--build-fd-command))
  (setq consult-ripgrep-args (nh--build-ripgrep-command))

  ;; Make search results appear immediately (like Ivy/Swiper)
  (setq consult-async-min-input 0)           ;; Start searching immediately, no minimum input
  (setq consult-async-input-throttle 0.1)    ;; Very fast response time
  (setq consult-async-input-debounce 0.1)    ;; Quick debounce for responsive typing

  ;; Show all candidates immediately when command is invoked
  (setq consult-async-refresh-delay 0.0)     ;; No delay in refreshing results
  (setq consult-preview-key 'any)            ;; Preview immediately on any key

  ;; Configure project function
  (setq consult-project-function #'consult--default-project-function)

  ;; Configure async splitting style (for grep commands)
  (setq consult-async-split-style 'perl)

  ;; Make consult-line start from current position (more like swiper)
  (setq consult-line-start-from-top nil))

;; Consult Dir: Directory jumping with Consult
;; Provides quick directory navigation and switching with consult integration,
;; allowing you to jump between frequently used directories with preview support.
;; GitHub: https://github.com/karthink/consult-dir
(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-j" . consult-dir-jump-file)))

;; Ag: The Silver Searcher for Emacs
;; Fast text search tool integration providing high-performance full-text
;; search across project files with support for various file types and patterns.
;; GitHub: https://github.com/Wilfred/ag.el
(use-package ag
  :ensure t)

;; Projectile: Project management and navigation
;; Comprehensive project management package providing file navigation, search,
;; compilation, and testing commands with support for multiple project types.
;; GitHub: https://github.com/bbatsov/projectile
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :commands (projectile-find-file projectile-switch-project projectile-ag projectile-mode)
  :hook (after-init . projectile-mode)
  :init
  (setq projectile-track-known-projects-automatically t
        projectile-completion-system 'default
        projectile-enable-caching t
        projectile-indexing-method 'alien
        ;; Add more project root files for better detection
        projectile-project-root-files '(".projectile" ".git" ".hg" ".svn" ".bzr" "_darcs"
                                         "package.json" "Gemfile" "requirements.txt"
                                         "setup.py" "pom.xml" "build.gradle" "Cargo.toml"
                                         "go.mod" "tsconfig.json" "jsconfig.json"
                                         "composer.json" "Makefile" "CMakeLists.txt"))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  ;; Apply global exclusions to projectile
  (dolist (dir nh/globally-ignored-directories)
    (add-to-list 'projectile-globally-ignored-directories dir))
  (dolist (pattern nh/globally-ignored-file-extensions)
    (add-to-list 'projectile-globally-ignored-files pattern))

  ;; Configure projectile to use better tools with exclusions
  (when (executable-find "fd")
    (setq projectile-generic-command
          (concat "fd . --type f --color=never " (nh--build-fd-shell-command))))
  (when (and (not (executable-find "fd")) (executable-find "rg"))
    (setq projectile-generic-command
          (concat "rg --files --color=never " (nh--build-ripgrep-shell-command))))

  ;; Warn if external 'ag' tool is missing
  (unless (executable-find "ag")
    (message "[Projectile] Warning: 'ag' (The Silver Searcher) is not installed.")))

;; Flycheck: On-the-fly syntax checking
;; Real-time syntax checking and error reporting with support for multiple
;; programming languages and customizable checker configurations.
;; GitHub: https://github.com/flycheck/flycheck
(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :hook ((prog-mode . flycheck-mode)
         (emacs-lisp-mode . flycheck-mode))
  :custom
  (flycheck-idle-change-delay 1)
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-disabled-checkers '())                    ;; Enable all checkers for better feedback
  (flycheck-display-errors-delay 0.5)
  (flycheck-check-syntax-automatically '(save idle-change mode-enabled)) ;; More frequent checking
  :bind (:map flycheck-mode-map
              ("M-n" . flycheck-next-error)
              ("M-p" . flycheck-previous-error)
              ("C-c ! l" . flycheck-list-errors)
              ("C-c ! c" . flycheck-buffer)
              ("C-c ! v" . flycheck-verify-setup))
  :config
  ;; Enable more comprehensive checking for Elisp
  (setq flycheck-emacs-lisp-check-declare t)          ;; Check declare-function statements

  ;; Always pop up the Flycheck errors buffer when there are errors
  (add-to-list 'display-buffer-alist
               '("\\*Flycheck errors\\*" (display-buffer-pop-up-window)))
  ;; Always pop up the Warnings buffer when there are warnings
  (add-to-list 'display-buffer-alist
               '("\\*Warnings\\*" (display-buffer-pop-up-window)))

  ;; Elisp-specific enhancements
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              ;; Enable all available checkers for Elisp
              (setq-local flycheck-disabled-checkers '())
              ;; More aggressive checking for development
              (setq-local flycheck-idle-change-delay 0.5))))

;; Flycheck Pos Tip: Show Flycheck errors in tooltips
;; Displays Flycheck error messages in graphical tooltips instead of the
;; echo area, providing better visibility and context for syntax errors.
;; GitHub: https://github.com/flycheck/flycheck-pos-tip
(use-package flycheck-pos-tip
  :ensure t
  :after flycheck
  :if (display-graphic-p)
  :config
  (flycheck-pos-tip-mode))

;; Corfu: In-buffer completion
;; Modern completion UI that displays candidates directly in the buffer with
;; automatic triggering, preview support, and integration with completion backends.
;; GitHub: https://github.com/minad/corfu
(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)                ;; TAB cycles
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-prefix 2)          ;; Auto complete after 2 chars
  (corfu-preview-delay 0.2)      ;; Delay before showing preview
  (corfu-popupinfo-mode 1)       ;; Show detailed candidate info
  (corfu-popupinfo-delay '(0.5 . 0.2))
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-quit-at-boundary 'separator)
  (corfu-scroll-margin 5)
  :init
  :config
  (global-corfu-mode)
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (add-to-list 'completion-at-point-functions #'cape-elisp-symbol))))

;; Cape: Completion at point extensions
;; Provides additional completion backends for various content types including
;; files, keywords, symbols, and dynamic abbreviations with modular design.
;; GitHub: https://github.com/minad/cape
(use-package cape
  :ensure t
  :defer t  ;; Defer loading until actually needed
  :init
  ;; Add desired completion sources to `completion-at-point-functions`
  ;; Order can matter for priority if multiple backends provide completions.

  ;; Setup cape completions when entering a buffer
  (defun nh/safe-add-cape-completions ()
    "Safely add cape completion functions if cape is loaded."
    (when (featurep 'cape)
      ;; Only add if not already present
      (unless (memq 'cape-file completion-at-point-functions)
        (add-to-list 'completion-at-point-functions #'cape-file t))
      (unless (memq 'cape-dabbrev completion-at-point-functions)
        (add-to-list 'completion-at-point-functions #'cape-dabbrev t))
      (unless (memq 'cape-keyword completion-at-point-functions)
        (add-to-list 'completion-at-point-functions #'cape-keyword t))))

  ;; Hook to setup cape completions after cape loads
  (with-eval-after-load 'cape
    (add-hook 'find-file-hook #'nh/safe-add-cape-completions)
    (add-hook 'after-change-major-mode-hook #'nh/safe-add-cape-completions))

  :config
  ;; Cape is now loaded, setup initial completions for current buffer
  (nh/safe-add-cape-completions)

  ;; (add-to-list 'completion-at-point-functions (cape-super-capf #'cape-dabbrev #'cape-keyword))
  ;; (add-to-list 'completion-at-point-functions #'cape-elisp-block) ; Elisp symbols
  ;; Consider adding other cape functions based on your needs:
  ;; cape-ispell, cape-tex, cape-sgml, cape-rfc1345, cape-abbrev, cape-dict, cape-symbol
  ;; Setup elisp-specific completions
  (defun nh/setup-elisp-cape-completions ()
    "Setup cape completions specifically for Emacs Lisp mode."
    (when (and (featurep 'cape) (fboundp 'cape-elisp-symbol))
      (unless (memq 'cape-elisp-symbol completion-at-point-functions)
        (add-to-list 'completion-at-point-functions #'cape-elisp-symbol t))))

  (with-eval-after-load 'cape
    (add-hook 'emacs-lisp-mode-hook #'nh/setup-elisp-cape-completions)))

;; Kind Icon: Icons for Corfu and Cape completions
;; Adds VSCode-style icons to completion candidates in Corfu, providing visual
;; distinction between different types of completions like functions and variables.
;; GitHub: https://github.com/jdtsmith/kind-icon
(use-package kind-icon
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Yasnippet: Template system for Emacs
;; Powerful template expansion system allowing you to insert code snippets with
;; placeholders, transformations, and dynamic content for faster coding.
;; GitHub: https://github.com/joaotavora/yasnippet
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  ;; Load snippets from a community collection
  ;; Yasnippet Snippets: Collection of yasnippet snippets
  ;; Community-maintained collection of snippet templates for various programming
  ;; languages and frameworks, providing ready-to-use code templates.
  ;; GitHub: https://github.com/AndreaCrotti/yasnippet-snippets
  (use-package yasnippet-snippets
    :ensure t
    :after yasnippet
    :config
    (yasnippet-snippets-initialize)))

;; LSP Mode: Language Server Protocol client
;; Comprehensive LSP client providing IDE-like features including code completion,
;; diagnostics, navigation, and refactoring for multiple programming languages.
;; GitHub: https://github.com/emacs-lsp/lsp-mode
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook ((prog-mode . (lambda ()
                        (unless (or (derived-mode-p 'emacs-lisp-mode)
                                    (derived-mode-p 'typescript-mode)
                                    (derived-mode-p 'js2-mode)
                                    (derived-mode-p 'rjsx-mode))
                          (lsp-deferred)))))
  :custom
  (lsp-completion-provider :capf)
  (lsp-eldoc-render-all nil)
  (lsp-idle-delay 0.500)
  (lsp-signature-render-documentation t)
  (lsp-headerline-breadcrumb-enable t)
  :config

  (setq lsp-disabled-clients '(rubocop-ls sorbet-ls typeprof-ls steep-ls ruby-syntax-tree-ls semgrep-ls solargraph))
  (setq lsp-warn-no-matched-clients t)

  ;; Configure project detection for LSP
  (setq lsp-auto-guess-root t)  ;; Automatically detect project root
  (setq lsp-prefer-workspace-root t)  ;; Prefer workspace root for operations

  ;; Define project root patterns
  (setq lsp-project-root-files '(".git" ".projectile" "package.json" "Gemfile"
                                  "Cargo.toml" "go.mod" "pom.xml" "build.gradle"
                                  "tsconfig.json" "jsconfig.json" ".env"
                                  "Makefile" "CMakeLists.txt" ".gitignore"))

  ;; Configure workspace folders
  (setq lsp-enable-file-watchers t)  ;; Watch files for changes
  (setq lsp-file-watch-threshold 1000)  ;; Increase file watch limit

  (require 'lsp-headerline)
  (require 'lsp-modeline)
  (require 'lsp-lens)
  (add-hook 'lsp-mode-hook #'lsp-lens-mode)
  (add-hook 'lsp-mode-hook #'lsp-modeline-workspace-status-mode)
  (add-hook 'lsp-mode-hook #'lsp-headerline-breadcrumb-mode)

  ;; Configure LSP UI features
  (setq lsp-modeline-code-actions-enable t)
  (setq lsp-modeline-diagnostics-enable t)
  (setq lsp-signature-auto-activate t)
  (setq lsp-signature-render-documentation t)
  (setq lsp-hover-enable t)
  (setq lsp-eldoc-enable-hover t)

  ;; Enable flycheck integration with LSP
  (setq lsp-diagnostics-provider :flycheck)
  (setq lsp-flycheck-live-reporting t)

  ;; Suppress certain LSP errors
  (setq lsp-print-io nil)  ;; Disable IO logging for performance
  (setq lsp-log-io nil)    ;; Disable IO logging

  ;; Add advice to handle nil positions in LSP responses
  (defadvice lsp--apply-text-edit (around lsp-handle-nil-positions activate)
    "Handle nil positions in LSP text edits."
    (condition-case err
        ad-do-it
      (wrong-type-argument
       (message "LSP: Ignoring text edit with invalid position: %s" err)
       nil)))

  ;; Wrap LSP hover to handle errors gracefully
  (with-eval-after-load 'lsp-mode
    (defun nh/safe-lsp-hover ()
      "Safe wrapper around lsp-hover that handles errors."
      (interactive)
      (condition-case err
          (lsp-hover)
        (error
         (message "LSP hover error: %s" (error-message-string err)))))

    ;; Replace the default hover keybinding
    (define-key lsp-mode-map [remap xref-find-definitions] 'lsp-find-definition)
    (define-key lsp-mode-map (kbd "K") 'nh/safe-lsp-hover))

  ;; Helper functions for debugging LSP project awareness
  (defun nh/lsp-describe-workspace ()
    "Describe the current LSP workspace and project root."
    (interactive)
    (if (bound-and-true-p lsp-mode)
        (let ((workspace (lsp-find-workspace-root))
              (project-root (projectile-project-root))
              (lsp-root (lsp-workspace-root)))
          (message "LSP Workspace: %s\nProjectile root: %s\nLSP root: %s"
                   workspace project-root lsp-root))
      (message "LSP mode is not active in this buffer")))

  (defun nh/lsp-restart-workspace ()
    "Restart the LSP workspace for the current buffer."
    (interactive)
    (when (bound-and-true-p lsp-mode)
      (lsp-workspace-restart (lsp--read-workspace))
      (message "LSP workspace restarted"))))

;; LSP UI: Enhanced UI for LSP diagnostics and features
;; Provides rich UI components for LSP including documentation popups, diagnostic
;; overlays, code actions, and peek functionality for definitions and references.
;; GitHub: https://github.com/emacs-lsp/lsp-ui
(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :custom
  ;; LSP UI Doc settings
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-show-with-cursor nil)
  (lsp-ui-doc-show-with-mouse t)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-max-width 120)
  (lsp-ui-doc-max-height 15)
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-doc-use-webkit nil)
  ;; Ensure text rendering uses proper backgrounds
  (lsp-ui-doc-text-scale-level 0)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  ;; Offset the documentation box position
  (lsp-ui-doc-alignment 'window)
  ;; Add spacing between cursor and documentation box
  (lsp-ui-doc-position 'top)  ; Position above cursor
  (lsp-ui-doc-delay 0.2)  ; Small delay before showing
  ;; Border configuration - can be a color string or nil for no border
  (lsp-ui-doc-border (face-attribute 'vertical-border :foreground))
  ;; Frame parameters including internal padding
  (lsp-ui-doc-frame-parameters
   '((internal-border-width . 15)
     (left-fringe . 10)
     (right-fringe . 10)))

  :config
  ;; Simple spacing solution using built-in margin
  (setq lsp-ui-doc-child-frame-border-width 3)  ; Add spacing around the frame

  ;; Working solution: Advise the actual move function to add offset
  (defun nh/lsp-ui-doc-move-with-offset (orig-fun &rest args)
    "Add offset when moving the doc frame."
    ;; First call the original function
    (apply orig-fun args)
    ;; Then adjust position if frame exists
    (when (and (boundp 'lsp-ui-doc--frame)
               lsp-ui-doc--frame
               (frame-live-p lsp-ui-doc--frame))
      (let* ((frame-pos (frame-position lsp-ui-doc--frame))
             (x (car frame-pos))
             (y (cdr frame-pos))
             ;; Add offset based on position
             (offset (if (eq lsp-ui-doc-position 'top)
                        -80  ; Move up 80 pixels when on top
                      80)))  ; Move down 80 pixels when on bottom
        (set-frame-position lsp-ui-doc--frame x (+ y offset)))))

  ;; Apply the advice after lsp-ui loads
  (with-eval-after-load 'lsp-ui-doc
    (advice-add 'lsp-ui-doc--move-frame :around #'nh/lsp-ui-doc-move-with-offset))

  ;; LSP UI Flycheck (diagnostics) settings
  (setq lsp-ui-flycheck-enable t)
  (setq lsp-ui-flycheck-list-position 'right)
  (setq lsp-ui-flycheck-live-reporting t)

  ;; LSP UI Sideline settings
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-sideline-show-symbol t)
  (setq lsp-ui-sideline-ignore-duplicate t)
  (setq lsp-ui-sideline-delay 0.5)

  ;; LSP UI Peek settings
  (setq lsp-ui-peek-enable t)
  (setq lsp-ui-peek-peek-height 20)
  (setq lsp-ui-peek-list-width 50)
  (setq lsp-ui-peek-fontify 'on-demand)

  ;; LSP UI Imenu settings
  (setq lsp-ui-imenu-enable t)
  (setq lsp-ui-imenu-kind-position 'top)

  :bind (:map lsp-ui-mode-map
              ("C-c l d" . lsp-ui-doc-show)
              ("C-c l D" . lsp-ui-doc-hide)
              ("C-c l f" . lsp-ui-flycheck-list)
              ("C-c l i" . lsp-ui-imenu)
              ("C-c l r" . lsp-ui-peek-find-references)
              ("C-c l s" . lsp-ui-peek-find-workspace-symbol)
              ("C-c l ." . lsp-ui-peek-find-definitions)
              ("C-c l I" . lsp-ui-peek-find-implementation))

  :hook (lsp-mode . lsp-ui-mode)

  :config
  ;; Custom face definitions to match current theme
  (defun nh/configure-lsp-ui-faces ()
    "Configure lsp-ui faces to match the current theme."
    ;; Get actual colors from the current theme
    (let* ((default-bg (face-attribute 'default :background))
           (mode-line-bg (face-attribute 'mode-line :background))
           (mode-line-inactive-bg (face-attribute 'mode-line-inactive :background))
           ;; Swap the colors - darker for header, lighter for doc
           (header-bg mode-line-inactive-bg)  ; Use darker background for header
           (doc-bg default-bg))  ; Use main background for documentation

      ;; Apply the faces - ONLY for lsp-ui-doc faces
      (custom-set-faces
       ;; Header face - bold with darker background
       `(lsp-ui-doc-header ((t (:inherit font-lock-keyword-face
                               :background ,header-bg
                               :foreground ,(face-attribute 'font-lock-keyword-face :foreground)
                               :weight bold
                               :height 1.1  ; Slightly larger
                               :box (:line-width (4 . 4) :color ,header-bg)))))  ; Padding around text
       ;; Main documentation face - this is the key one
       `(lsp-ui-doc-background ((t (:background ,doc-bg))))
       ;; URL and link faces
       `(lsp-ui-doc-url ((t (:inherit link :background ,doc-bg))))
       ;; Child frame face
       `(lsp-ui-doc ((t (:background ,doc-bg))))
       ;; Markdown code blocks in lsp-ui-doc
       `(lsp-ui-doc-markdown-code-block-face ((t (:background ,doc-bg)))))

      ;; Additional configuration
      (setq lsp-ui-doc-border (face-attribute 'vertical-border :foreground))

      ;; Debug message to check colors
      (message "LSP-UI colors set: header=%s, doc=%s" header-bg doc-bg))

    ;; Force refresh of child frames to apply new colors
    (when (and (fboundp 'lsp-ui-doc--delete-frame)
               (boundp 'lsp-ui-doc--frame)
               lsp-ui-doc--frame)
      (lsp-ui-doc--delete-frame)))

  ;; Apply the face configuration
  (nh/configure-lsp-ui-faces)

  ;; Re-apply when theme changes
  (add-hook 'after-load-theme-hook #'nh/configure-lsp-ui-faces)

  ;; Configure webkit rendering if used
  (when lsp-ui-doc-use-webkit
    (setq lsp-ui-doc-webkit-background-color
          (face-attribute 'mode-line-inactive :background))))

  ;; Override markdown rendering in lsp-ui-doc
  (with-eval-after-load 'lsp-ui-doc
    ;; Custom CSS for webkit rendering
    (when lsp-ui-doc-use-webkit
      (setq lsp-ui-doc-webkit-background-color
            (face-attribute 'mode-line-inactive :background)))

    ;; Override the markdown rendering to remove code block backgrounds
    (defun nh/lsp-ui-doc-remove-code-background (orig-fn &rest args)
      "Remove white background from code blocks in lsp-ui-doc."
      (let ((result (apply orig-fn args)))
        (when (get-buffer " *lsp-ui-doc*")
          (with-current-buffer " *lsp-ui-doc*"
            (let ((inhibit-read-only t)
                  (doc-bg (face-attribute 'mode-line-inactive :background)))
              ;; Find all code blocks and remove their background
              (save-excursion
                (goto-char (point-min))
                (while (re-search-forward "`[^`]+`" nil t)
                  (let ((start (match-beginning 0))
                        (end (match-end 0)))
                    (add-face-text-property start end
                                          `(:background ,doc-bg) t)))
                ;; Also handle triple backtick code blocks
                (goto-char (point-min))
                (while (re-search-forward "```[^`]*```" nil t)
                  (let ((start (match-beginning 0))
                        (end (match-end 0)))
                    (add-face-text-property start end
                                          `(:background ,doc-bg) t)))))))
        result))

    (advice-add 'lsp-ui-doc--render-buffer :around #'nh/lsp-ui-doc-remove-code-background))

  ;; Interactive function to customize lsp-ui-doc colors
  ;; (defun nh/lsp-ui-doc-set-theme-colors ()
  ;;   "Interactively set lsp-ui-doc colors to match theme."
  ;;   (interactive)
  ;;   (let* ((themes '(("Solarized Light" . ("#fdf6e3" "#eee8d5"))
  ;;                   ("Solarized Dark" . ("#002b36" "#073642"))
  ;;                   ("Use mode-line colors" . (mode-line region))
  ;;                   ("Custom..." . custom)))
  ;;          (choice (completing-read "Choose color scheme: " (mapcar #'car themes)))
  ;;          (colors (cdr (assoc choice themes))))
  ;;     (cond
  ;;      ((equal colors 'custom)
  ;;       (let ((header-bg (read-color "Header background: "))
  ;;             (doc-bg (read-color "Documentation background: ")))
  ;;         (custom-set-faces
  ;;          `(lsp-ui-doc-header ((t (:background ,header-bg))))
  ;;          `(lsp-ui-doc-background ((t (:background ,doc-bg)))))))
  ;;      ((listp colors)
  ;;       (if (stringp (car colors))
  ;;           (progn
  ;;             (custom-set-faces
  ;;              `(lsp-ui-doc-header ((t (:background ,(car colors)))))
  ;;              `(lsp-ui-doc-background ((t (:background ,(cadr colors))))))
  ;;             (message "Set lsp-ui-doc colors: header=%s, doc=%s" (car colors) (cadr colors)))
  ;;         (custom-set-faces
  ;;          `(lsp-ui-doc-header ((t (:background ,(face-attribute (car colors) :background)))))
  ;;          `(lsp-ui-doc-background ((t (:background ,(face-attribute (cadr colors) :background)))))
  ;;          (message "Set lsp-ui-doc colors from faces: %s and %s" (car colors) (cadr colors)))))))
  ;;   (when (and (boundp 'lsp-ui-doc-frame) lsp-ui-doc-frame)
  ;;     (lsp-ui-doc-hide)
  ;;     (message "Reopen documentation to see changes")))

  ;; Function to customize border and padding
  (defun nh/lsp-ui-doc-set-border-padding ()
    "Interactively set border and padding for lsp-ui-doc."
    (interactive)
    (let* ((border-options '(("No border" . nil)
                           ("Subtle (vertical-border)" . vertical-border)
                           ("Default text color" . default)
                           ("Mode-line color" . mode-line)
                           ("Custom color..." . custom)))
           (padding-options '(("No padding" . 0)
                            ("Small (5px)" . 5)
                            ("Medium (10px)" . 10)
                            ("Large (15px)" . 15)
                            ("Extra large (20px)" . 20)
                            ("Custom..." . custom)))
           (border-choice (completing-read "Border style: " (mapcar #'car border-options)))
           (padding-choice (completing-read "Padding: " (mapcar #'car padding-options)))
           (border-val (cdr (assoc border-choice border-options)))
           (padding-val (cdr (assoc padding-choice padding-options))))

      ;; Set border
      (cond
       ((null border-val) (setq lsp-ui-doc-border nil))
       ((eq border-val 'custom)
        (setq lsp-ui-doc-border (read-color "Border color: ")))
       ((symbolp border-val)
        (setq lsp-ui-doc-border (face-attribute border-val :foreground))))

      ;; Set padding
      (when (eq padding-val 'custom)
        (setq padding-val (read-number "Padding (pixels): " 10)))

      (setq lsp-ui-doc-frame-parameters
            `((internal-border-width . ,padding-val)))

      ;; Force refresh
      (when (and (boundp 'lsp-ui-doc--frame) lsp-ui-doc--frame)
        (lsp-ui-doc--delete-frame))

      (message "Border: %s, Padding: %dpx. Hover to see changes."
               (or lsp-ui-doc-border "none") padding-val)))

  ;; Bind to convenient keys (using C-c L to avoid conflicts with lsp-ui)
(global-set-key (kbd "C-c L b") 'nh/lsp-ui-doc-set-border-padding)

;; Whitespace: Highlight trailing whitespace and long lines
;; Built-in package for visualizing whitespace issues including trailing spaces,
;; tabs, and lines exceeding length limits to maintain code quality standards.
(use-package whitespace
  :ensure nil
  :init
  (add-hook 'prog-mode-hook #'whitespace-mode)
  :config
  (setq whitespace-style '(face trailing tabs lines-tail)))

;; WS Butler: Intelligently trim whitespace
;; Automatically removes trailing whitespace only from lines you've edited,
;; avoiding unnecessary changes to files while maintaining clean code style.
;; GitHub: https://github.com/lewang/ws-butler
;; (use-package ws-butler
;;   :diminish ws-butler-mode
;;   :ensure t
;;   :config
;;   (setq ws-butler-keep-whitespace-before-point nil)
;;   (ws-butler-global-mode))

;; Reverse search function
(defun nh/consult-line-reverse ()
  "Search backwards using consult-line."
  (interactive)
  (let ((current-line (line-number-at-pos))
        (consult-line-start-from-top nil))
    (consult-line)
    ;; After consult-line completes, if we're still at same position,
    ;; search backwards from current point
    (when (eq current-line (line-number-at-pos))
      (isearch-backward))))

;; Project-specific ripgrep function
(defun nh/consult-ripgrep-project ()
  "Run `consult-ripgrep` in the project root, or current directory if no project."
  (interactive)
  (if-let ((project (project-current)))
      (consult-ripgrep (project-root project))
    (consult-ripgrep default-directory)))

;; Global keybindings
(global-set-key (kbd "C-r") #'nh/consult-line-reverse)
;;(global-set-key (kbd "C-c p s") #'nh/consult-ripgrep-project)

;; Add convenience bindings in isearch-mode for transitioning to consult
(define-key isearch-mode-map (kbd "M-s l") #'consult-line)
(define-key isearch-mode-map (kbd "M-s L") #'consult-line-multi)

;; Enhance minibuffer history
(define-key minibuffer-local-map (kbd "M-r") #'consult-history)

;; Function to show M-x command history
(defun nh/show-command-history ()
  "Show the extended command history (M-x history)."
  (interactive)
  (let ((command (completing-read "Recent commands: " extended-command-history)))
    (command-execute (intern command))))

;; Function to clear specific histories
(defun nh/clear-command-history ()
  "Clear the M-x command history."
  (interactive)
  (when (yes-or-no-p "Clear M-x command history? ")
    (setq extended-command-history nil)
    (message "M-x command history cleared")))

;; Better M-x that shows recent commands first
(defun nh/execute-extended-command ()
  "Execute extended command with history prioritized."
  (interactive)
  (let* ((history-commands (seq-filter (lambda (cmd)
                                         (and (symbolp (intern-soft cmd))
                                              (commandp (intern-soft cmd))))
                                       extended-command-history))
         (all-commands (all-completions "" obarray 'commandp))
         ;; Put history commands first, then remaining commands
         (sorted-commands (append history-commands
                                  (seq-difference all-commands history-commands)))
         (command (completing-read "M-x " sorted-commands nil t nil 'extended-command-history)))
    (command-execute (intern command))))

;; Replace default M-x with enhanced version that shows history first
(global-set-key (kbd "M-x") #'nh/execute-extended-command)

;; Enable recursive minibuffers and depth indication
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)

;; Configure completion behavior for fast, responsive interaction
(setq completion-show-inline-help nil      ;; Don't show help immediately
      completion-auto-help t               ;; Show completions immediately
      completion-cycle-threshold 1         ;; Enable TAB cycling with just 1 candidate
      completions-detailed t               ;; Show detailed completions when available
      completion-show-help t               ;; Show completions right away
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t
      completion-auto-select nil           ;; Don't auto-select first completion
      completions-format 'one-column       ;; Better display format
      tab-always-indent t)                 ;; Make TAB indent first, then complete if already indented

;; NOTE: Elisp-specific development configuration is now in lang/nh-elisp.el

(provide 'nh-autocompletion)

;;; nh-autocompletion.el ends here
