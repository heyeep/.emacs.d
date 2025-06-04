;;; nh-autocompletion.el --- Autocompletion configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for autocompletion and related packages using Vertico, Consult, and friends.
;; This configuration globally excludes build artifacts and node_modules from all file operations.

;;; Code:

;; Custom group for our configuration
(defgroup nh-autocompletion nil
  "Autocompletion configuration with global file exclusions."
  :group 'convenience
  :prefix "nh-")

;; ===== GLOBAL FILE EXCLUSION CONFIGURATION =====

(defcustom nh-globally-ignored-directories
  '("node_modules" "dist" "build" "target" "__pycache__")
  "Directories to exclude globally from all file operations in Emacs.
These directories are excluded from:
- Consult find/fd/ripgrep operations
- Projectile file discovery
- Global completion systems"
  :type '(repeat string)
  :group 'nh-autocompletion)

(defcustom nh-globally-ignored-file-extensions
  '(".elc" ".pyc" ".min.js" ".min.css" ".bundle.js" ".chunk.js")
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
           nh-globally-ignored-directories)))

(defun nh--build-fd-command ()
  "Build fd command arguments as a list for consult-fd."
  (append 
   '("fd" "--type" "f" "--hidden" "--follow" "--color=never")
   (mapcan (lambda (dir) (list "--exclude" dir))
           nh-globally-ignored-directories)))

(defun nh--build-ripgrep-command ()
  "Build ripgrep command arguments as a list for consult-ripgrep."
  (append
   '("rg" "--null" "--line-buffered" "--color=never" "--max-columns=1000" 
     "--path-separator=/" "--smart-case" "--no-heading" "--line-number")
   (mapcan (lambda (dir) (list "--glob" (format "!%s" dir)))
           nh-globally-ignored-directories)))

;; Apply global exclusions to completion systems
(defun nh--configure-global-completion ()
  "Configure global file completion to respect our exclusion patterns."
  ;; Add file extensions to global ignore list
  (setq completion-ignored-extensions
        (append completion-ignored-extensions nh-globally-ignored-file-extensions))
  
  ;; Configure ido if present
  (when (boundp 'ido-ignore-directories)
    (setq ido-ignore-directories
          (append ido-ignore-directories nh-globally-ignored-directories))))

;; Helper function for building shell command strings (for projectile)
(defun nh--build-fd-shell-command ()
  "Build fd shell command string for projectile."
  (mapconcat (lambda (dir) (format "--exclude %s" dir))
             nh-globally-ignored-directories " "))

(defun nh--build-ripgrep-shell-command ()
  "Build ripgrep shell command string for projectile."
  (mapconcat (lambda (dir) (format "--glob '!%s'" dir))
             nh-globally-ignored-directories " "))

;; Initialize global completion configuration
(nh--configure-global-completion)

;; ===== VERTICO AND COMPLETION FRAMEWORK =====

;; Vertico: Vertical completion UI
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
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless flex basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))))
  (orderless-smart-case t)
  (orderless-component-separator #'orderless-escapable-split-on-space))

;; Marginalia for annotations
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

;; Track recent files
(use-package recentf
  :init
  (recentf-mode 1)
  (setq recentf-max-saved-items 1000
        recentf-auto-cleanup 'never))

;; Embark: Context menu and actions
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

;; Enable consult integration with embark
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Consult for commands - replaces Counsel functionality
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
  ;; Configure xref to use consult
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

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

;; Configure consult-dir for directory jumping
(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-j" . consult-dir-jump-file)))

;; Enhanced search with ag (still used by projectile-ag)
(use-package ag
  :ensure t)

;; ===== PROJECT MANAGEMENT =====

;; Projectile: Project management and navigation
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :commands (projectile-find-file projectile-switch-project projectile-ag projectile-mode)
  :hook (after-init . projectile-mode)
  :init
  (setq projectile-track-known-projects-automatically t
        projectile-completion-system 'default
        projectile-enable-caching t
        projectile-indexing-method 'alien)
  :config
  ;; Apply global exclusions to projectile
  (dolist (dir nh-globally-ignored-directories)
    (add-to-list 'projectile-globally-ignored-directories dir))
  (dolist (pattern nh-globally-ignored-file-extensions)
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

;; ===== CODE QUALITY AND SYNTAX CHECKING =====

;; On-the-fly syntax checking
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

;; Show Flycheck errors in tooltips (GUI only)
(use-package flycheck-pos-tip
  :ensure t
  :after flycheck
  :if (display-graphic-p)
  :config
  (flycheck-pos-tip-mode))

;; Corfu: In-buffer completion
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
              (add-to-list 'completion-at-point-functions #'cape-symbol))))

;; Provides completion backends (sources) for Corfu by extending Emacs's
;; built-in completion-at-point-functions (CAPF).
(use-package cape
  :ensure t
  :init
  ;; Add desired completion sources to `completion-at-point-functions`
  ;; Order can matter for priority if multiple backends provide completions.

  ;; File paths
    (add-to-list 'completion-at-point-functions #'cape-file)

  ;; Dynamic abbreviations
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)

  ;; Keywords for current mode
  (add-to-list 'completion-at-point-functions #'cape-keyword)

  ;; (add-to-list 'completion-at-point-functions (cape-super-capf #'cape-dabbrev #'cape-keyword))
  ;; (add-to-list 'completion-at-point-functions #'cape-elisp-block) ; Elisp symbols
  ;; Consider adding other cape functions based on your needs:
  ;; cape-ispell, cape-tex, cape-sgml, cape-rfc1345, cape-abbrev, cape-dict, cape-symbol
  :config
   (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              ;; `cape-elisp-symbol` completes Elisp symbols from current buffer & loaded libs.
              ;; `cape-elisp-block`
              (add-to-list 'completion-at-point-functions #'cape-elisp-symbol t))))

;; For better icons in Corfu and Cape completions
(use-package kind-icon
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; ===== SNIPPETS =====

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  ;; Load snippets from a community collection
  (use-package yasnippet-snippets
    :ensure t
    :after yasnippet
    :config
    (yasnippet-snippets-initialize)))

;; ===== LANGUAGE SERVER PROTOCOL =====

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook ((prog-mode . (lambda ()
                        (unless (derived-mode-p 'emacs-lisp-mode)
                          (lsp-deferred)))))
  :custom
  (lsp-completion-provider :capf)
  (lsp-eldoc-render-all nil)
  (lsp-idle-delay 0.500)
  (lsp-signature-render-documentation t)
  (lsp-headerline-breadcrumb-enable t)
  :config
  (add-to-list 'lsp-language-id-configuration '(enh-ruby-mode . "ruby"))
  (add-to-list 'lsp-disabled-clients 'rubocop-ls)
  (setq lsp-warn-no-matched-clients t)
  
  (require 'lsp-headerline)
  (require 'lsp-modeline)
  (require 'lsp-lens)
  (add-hook 'lsp-mode-hook #'lsp-lens-mode)
  (add-hook 'lsp-mode-hook #'lsp-modeline-workspace-status-mode)
  (add-hook 'lsp-mode-hook #'lsp-headerline-breadcrumb-mode))

;; ===== CODE FORMATTING AND CLEANUP =====

;; Highlight trailing whitespace, tabs, and long lines in programming modes
(use-package whitespace
  :ensure nil
  :init
  (add-hook 'prog-mode-hook #'whitespace-mode)
  :config
  (setq whitespace-style '(face trailing tabs lines-tail)))

;; Only trims trailing whitespace on lines you edit, not the whole file
(use-package ws-butler
  :diminish ws-butler-mode
  :ensure t
  :config
  (setq ws-butler-keep-whitespace-before-point nil)
  (ws-butler-global-mode))

;; ===== CUSTOM FUNCTIONS =====

;; Reverse search function
(defun nh-consult-line-reverse ()
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
(defun nh-consult-ripgrep-project ()
  "Run `consult-ripgrep` in the project root, or current directory if no project."
  (interactive)
  (if-let ((project (project-current)))
      (consult-ripgrep (project-root project))
    (consult-ripgrep default-directory)))

;; ===== GLOBAL KEYBINDINGS =====

;; Global keybindings
(global-set-key (kbd "C-r") #'nh-consult-line-reverse)
(global-set-key (kbd "C-c p s") #'nh-consult-ripgrep-project)

;; Add convenience bindings in isearch-mode for transitioning to consult
(define-key isearch-mode-map (kbd "M-s l") #'consult-line)
(define-key isearch-mode-map (kbd "M-s L") #'consult-line-multi)

;; Enhance minibuffer history
(define-key minibuffer-local-map (kbd "M-s") #'consult-history)
(define-key minibuffer-local-map (kbd "M-r") #'consult-history)

;; ===== COMMAND HISTORY ENHANCEMENTS =====

;; Function to show M-x command history
(defun nh-show-command-history ()
  "Show the extended command history (M-x history)."
  (interactive)
  (let ((command (completing-read "Recent commands: " extended-command-history)))
    (command-execute (intern command))))

;; Function to clear specific histories
(defun nh-clear-command-history ()
  "Clear the M-x command history."
  (interactive)
  (when (yes-or-no-p "Clear M-x command history? ")
    (setq extended-command-history nil)
    (message "M-x command history cleared")))

;; Better M-x that shows recent commands first
(defun nh-execute-extended-command ()
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

;; Keybindings for history functions
(global-set-key (kbd "C-c x h") #'nh-show-command-history)
(global-set-key (kbd "C-c x c") #'nh-clear-command-history)

;; Replace default M-x with enhanced version that shows history first
(global-set-key (kbd "M-x") #'nh-execute-extended-command)

;; ===== COMPLETION FRAMEWORK CONFIGURATION =====

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
      tab-always-indent 'complete)         ;; Make TAB always try completion

;; NOTE: Elisp-specific development configuration is now in lang/nh-elisp.el

(provide 'nh-autocompletion)

;;; nh-autocompletion.el ends here