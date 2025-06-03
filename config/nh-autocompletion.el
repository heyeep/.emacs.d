;;; nh-autocompletion.el --- Autocompletion configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for autocompletion and related packages.

;;; Code:

;; Ivy: Flexible minibuffer completion
(use-package ivy
  :ensure t
  :diminish
  :custom
  (ivy-use-virtual-buffers t)
  (enable-recursive-minibuffers t)
  (ivy-count-format "(%d/%d) ")
  :config
  (ivy-mode 1)
  (setq ivy-display-style 'fancy)
  (setq ivy-count-format "") ;; Hide candidate count
  (setq ivy-height 15)       ;; Show up to 15 candidates
  ;; Swap RET and C-j: RET does ivy-alt-done, C-j does ivy-done
  (define-key ivy-minibuffer-map (kbd "RET") 'ivy-alt-done)
  (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-done)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)) ;; Resume last Ivy session

;; Ivy-enhanced versions of common Emacs commands
(use-package counsel
  :ensure t
  :after ivy
  :config
  (counsel-mode 1)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "s-x") 'counsel-M-x)
  (global-set-key (kbd "C-c r") 'counsel-recentf) ;; Quick access to recent files
  (setq counsel-find-file-at-point t) ;; Enable preview for counsel-find-file
  (let ((nh/counsel-ignore-patterns
         '("\\.elc\\'"      ; Byte-compiled Emacs Lisp files
           "\\`#.*#\\'"     ; Emacs auto-save files
           "~\\'"           ; Emacs backup files
           )))
    (setq counsel-find-file-ignore-regexp
          (mapconcat #'identity nh/counsel-ignore-patterns "\\|"))))

;; Swiper: Ivy-powered in-buffer search
(use-package swiper
  :ensure t
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper)
         ("C-c C-r" . ivy-resume)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-x b" . ivy-switch-buffer))
  :config
  (setq ivy-count-format "")      ;; Hide candidate count
  (setq counsel-ag-base-command "ag -U --nocolor --nogroup %s -- .")
  (setq swiper-goto-start-of-match t)) ;; Highlight line number in Swiper

(use-package ag
  :ensure t)

;; Modern project management and navigation
(use-package projectile
  :ensure t
  :diminish projectile-mode
  ;; Enable Projectile globally after Emacs starts (eager loading, not lazy)
  ;; This makes all project features and commands available everywhere.
  :commands (projectile-find-file projectile-switch-project projectile-ag projectile-mode)
  :hook (after-init . projectile-mode)
  :init
  ;; Track known projects automatically when switching projects or opening files
  (setq projectile-track-known-projects-automatically t)
  ;; Use Ivy for completion (change to 'vertico or 'helm if you use those)
  (setq projectile-completion-system 'ivy)
  ;; Enable caching for faster project file lookups
  (setq projectile-enable-caching t)
  ;; Use the fastest available indexing method
  (setq projectile-indexing-method 'alien)
  ;; Use fd or rg for file discovery if available (much faster than find)
  (when (executable-find "fd")
    (setq projectile-generic-command "fd . --type f --color=never"))
  (when (and (not (executable-find "fd")) (executable-find "rg"))
    (setq projectile-generic-command "rg --files --color=never"))
  :config
  ;; Ignore bulky directories globally for all projects
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (add-to-list 'projectile-globally-ignored-directories "dist")
  ;; Warn if the external 'ag' tool is missing (needed for projectile-ag)
  (unless (executable-find "ag")
    (message "[Projectile] Warning: 'ag' (The Silver Searcher) is not installed. Install it for projectile-ag to work.")))

;; Add extra info to Ivy candidates
(use-package ivy-rich
  :ensure t
  :config
  (ivy-rich-mode 1)
  ;; Remove ffip transformer from ivy-rich to prevent interference with icons
  (with-eval-after-load 'ivy-rich
    (setq ivy-rich-display-transformers-list
          (assq-delete-all 'find-file-in-project ivy-rich-display-transformers-list))))

;; Smarter sorting and filtering
(use-package ivy-prescient
  :ensure t
  :after ivy
  :config
  (ivy-prescient-mode 1)
  (prescient-persist-mode 1))

;; Pretty icons in Ivy
(use-package all-the-icons-ivy-rich
  :ensure t
  :after (ivy-rich all-the-icons)
)

;; On-the-fly syntax checking for programming modes
(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :hook ((prog-mode . flycheck-mode)
         (emacs-lisp-mode . flycheck-mode)) ; Also enable in Emacs Lisp buffers
  :custom
  (flycheck-idle-change-delay 1)
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (flycheck-display-errors-delay 0.5)
  :bind (:map flycheck-mode-map
              ("M-n" . flycheck-next-error)
              ("M-p" . flycheck-previous-error)
              ("C-c ! l" . flycheck-list-errors))
  :config
  ;; Always pop up the Flycheck errors buffer when there are errors
  (add-to-list 'display-buffer-alist
               '("\\*Flycheck errors\\*" (display-buffer-pop-up-window)))
  ;; Always pop up the Warnings buffer when there are warnings
  (add-to-list 'display-buffer-alist
               '("\\*Warnings\\*" (display-buffer-pop-up-window))))

;; Show Flycheck errors in tooltips (GUI only)
(use-package flycheck-pos-tip
  :ensure t
  :after flycheck
  :if (display-graphic-p)
  :config
  (flycheck-pos-tip-mode))

(use-package flycheck-inline
  :ensure t
  :after flycheck
  :hook (flycheck-mode . flycheck-inline-mode))


(use-package orderless
  :ensure t
  :custom
  ;; Use orderless, with basic as a fallback. Basic is important for file paths.
  (completion-styles '(orderless basic))
  ;; For file paths, allow partial completion (e.g., "/u/s/b" for "/usr/share/bin")
  ;; and don't use orderless for files, as it can be confusing.
  (completion-category-overrides '((file (styles . (basic partial-completion))))))

;; Provides the in-buffer completion UI. Lightweight, fast, and overlays
;; completion candidates directly near your cursor.
(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)                ;; TAB cycles, M-TAB for indent-for-tab-command
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-prefix 2)          ;; Auto complete after 2 chars typed
  (corfu-preview-delay 0.2)      ;; Delay before showing preview of current candidate
  (corfu-popupinfo-mode 1)       ;; Show detailed candidate info in a child frame/popup
  (corfu-popupinfo-delay '(0.5 . 0.2)) ;; Delay for full doc popup (if corfu-popupinfo-mode is on)
  (corfu-separator ?\s)          ;; Orderless field separator (for Orderless completion style)
  (corfu-quit-at-boundary 'separator) ;; Automatically quit at word boundary (e.g., space)
  (corfu-scroll-margin 5)        ;; Number of lines at top/bottom before scrolling popup
  :init
  ;; (corfu-candidate-history-mode 1) ;; Optional: Enable candidate history
  :config
  (global-corfu-mode)            ;; Enable Corfu globally

  ;; For tab completion, use corfu-complete
  ;; (setq corfu-map (let ((map (make-sparse-keymap)))
  ;;                   (define-key map (kbd "TAB") #'corfu-complete)
  ;;                   (define-key map (kbd "<tab>") #'corfu-complete)
  ;;                   map))
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
              ;; `cape-elisp-block` completes Elisp code blocks.
              ;; `cape-symbol` completes symbols from all buffers matching the current mode.
              (add-to-list 'completion-at-point-functions #'cape-elisp-symbol t)
              ;; The 't' appends it to the local completion-at-point-functions,
              ;; which is often preferred for mode-specific additions.
              ))
  )

;; For better icons in Corfu and Cape completions
(use-package kind-icon
  :ensure t
  :after corfu ; Or :after cape, if cape initializes things kind-icon might use
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

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

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred) ; Autoload lsp and lsp-deferred commands
  ;; Hook LSP to start in programming modes. Use lsp-deferred for better startup performance.
  :hook ((prog-mode . (lambda ()
                        (unless (derived-mode-p 'emacs-lisp-mode)
                          (lsp-deferred)))))
  :custom
  (lsp-completion-provider :capf) ; Use completion-at-point-functions for completions
  (lsp-eldoc-render-all nil)      ; Only show eldoc for symbol at point
  (lsp-idle-delay 0.500)          ; Delay before sending changes to server (in seconds)
  (lsp-signature-render-documentation t) ; Show function signature help with docs
  (lsp-headerline-breadcrumb-enable t)   ; Show file path and symbols in header line
  :config

  (add-to-list 'lsp-language-id-configuration '(enh-ruby-mode . "ruby"))
  (add-to-list 'lsp-disabled-clients 'rubocop-ls) ; Disable the standalone rubocop-ls
;;  (setq lsp-enabled-clients listp t)
  (setq lsp-warn-no-matched-clients t)

  (require 'lsp-headerline)
  (require 'lsp-modeline)
  (require 'lsp-lens)
  (add-hook 'lsp-mode-hook #'lsp-lens-mode)
  (add-hook 'lsp-mode-hook #'lsp-modeline-workspace-status-mode)
  (add-hook 'lsp-mode-hook #'lsp-headerline-breadcrumb-mode))

;; Provides richer UI elements like sidelines, documentation popups, etc.
;; Can be resource-intensive for some or visually busy; enable if you like it.
;; (use-package lsp-ui
;;   :ensure t
;;   :commands lsp-ui-mode
;;   :after lsp-mode
;;   :hook (lsp-mode . lsp-ui-mode) ; Hook to lsp-mode to enable automatically
;;   :custom
;;   (lsp-ui-doc-enable t)
;;   (lsp-ui-doc-position 'at-point)       ; or 'top, 'bottom, 'window
;;   (lsp-ui-sideline-show-diagnostics t)
;;   (lsp-ui-sideline-show-hover t)
;;   (lsp-ui-sideline-show-code-actions t)
;;   (lsp-ui-peek-enable t)              ; For peek definition/references
;;   (lsp-ui-imenu-enable t)             ; For imenu integration
;;   (lsp-ui-doc-header t)
;;   (lsp-ui-doc-include-signature t)
;;   (lsp-enable-markdown t)
;;   (lsp-markdown-renderer 'markdown))

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

(provide 'nh-autocompletion)
