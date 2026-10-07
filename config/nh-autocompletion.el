;;; nh-autocompletion.el --- autocomplete -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

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

(defun nh--configure-global-completion ()
  "Configure global file completion to respect our exclusion patterns."
  (setq completion-ignored-extensions
        (append completion-ignored-extensions nh/globally-ignored-file-extensions))

  (when (boundp 'ido-ignore-directories)
    (setq ido-ignore-directories
          (append ido-ignore-directories nh/globally-ignored-directories))))

(defun nh--build-fd-shell-command ()
  "Build fd shell command string for projectile."
  (mapconcat (lambda (dir) (format "--exclude %s" dir))
             nh/globally-ignored-directories " "))

(defun nh--build-ripgrep-shell-command ()
  "Build ripgrep shell command string for projectile."
  (mapconcat (lambda (dir) (format "--glob '!%s'" dir))
             nh/globally-ignored-directories " "))

(nh--configure-global-completion)

;; vertico: shows minibuffer completions as a vertical list.
;; GitHub: https://github.com/minad/vertico
(use-package vertico
  :ensure t
  :demand
  :init
  (setq completion-auto-select 'second-tab)
  (setq completion-show-help t)
  (setq completion-auto-help t)
  (defun  nh/vertico-insert ()
    (interactive)
    (let* ((mb (minibuffer-contents-no-properties))
           (lc (if (string= mb "") mb (substring mb -1))))
      (cond ((string-match-p "^[/~:]" lc) (self-insert-command 1 ?/))
            ((file-directory-p (vertico--candidate)) (vertico-insert))
            (t (self-insert-command 1 ?/)))))
  :config
  (setq vertico-cycle t)
  ;; vertico-preselect 'directory needs the MELPA version of vertico.
  (setq vertico-preselect 'directory)
  (setq vertico-count 15)
  (setq vertico-resize t)
  (vertico-mode)

  (require 'vertico-directory)
  (define-key vertico-map (kbd "DEL") #'vertico-directory-delete-char)
  (define-key vertico-map (kbd "M-DEL") #'vertico-directory-delete-word)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

  :bind (:map vertico-map
              ("/" . #'nh/vertico-insert)
              ("TAB" . #'vertico-next)
              ("S-TAB" . #'vertico-previous)
              ("C-j" . #'vertico-exit)
              ("RET" . #'vertico-directory-enter)
              ("M-p" . #'vertico-previous)
              ("M-n" . #'vertico-next)))

;; orderless: matches space-separated words in any order.
;; GitHub: https://github.com/oantolin/orderless
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless flex basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))))
  (orderless-smart-case t)
  (orderless-component-separator #'orderless-escapable-split-on-space))

;; marginalia: adds notes beside minibuffer items, like file sizes and docstrings.
;; GitHub: https://github.com/minad/marginalia
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode)
  :custom
  (marginalia-align 'right)
  (marginalia-max-relative-age 0))

;; Save minibuffer history across restarts.
(require 'savehist)
(setq history-length 1000)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history t)
(setq savehist-autosave-interval 300)

(setq savehist-additional-variables
      '(mark-ring
        global-mark-ring
        search-ring
        regexp-search-ring
        extended-command-history
        file-name-history
        buffer-name-history
        minibuffer-history
        query-replace-history
        read-expression-history
        org-read-date-history
        kill-ring))

(savehist-mode 1)

;; recentf: remembers recently opened files.
(use-package recentf
  :init
  (recentf-mode 1)
  (setq recentf-max-saved-items 1000
        recentf-auto-cleanup 'never))

;; embark: act on the item at point or the current completion.
;; GitHub: https://github.com/oantolin/embark
(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (setq embark-verbose-indicator-display-action
        '(display-buffer-at-bottom))

  (setq embark-action-indicator
        (lambda (map)
          (which-key--show-keymap "Embark Actions" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command))
  (setq embark-become-indicator embark-action-indicator))

;; embark-consult: Embark actions for Consult results.
;; GitHub: https://github.com/oantolin/embark
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; consult: search and navigation commands with live preview.
;; GitHub: https://github.com/minad/consult
(use-package consult
  :ensure t
  :bind (
         ("C-x b" . consult-buffer)
         ;; Not C-c r, which Ruby, Go and Python buffers use as a prefix.
         ("C-x C-r" . consult-recent-file)

         ("C-s" . consult-line)

         ("C-c f" . consult-find)
         ("C-c d" . consult-fd)
         ;; Not C-c l, which is the LSP prefix in LSP buffers.
         ("C-c L" . consult-locate)

         ("C-c k" . consult-ripgrep)
         ("C-c g" . consult-grep)
         ("C-c G" . consult-git-grep)

         ("C-M-l" . consult-imenu)
         ("M-y" . consult-yank-pop)
         ("C-c h" . consult-history)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ("C-c o" . consult-outline)
         ("C-c t" . consult-theme)

         ("C-c C-r" . consult-history)
         )

  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  (when (fboundp 'consult-xref)
    (setq xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref))

  :config
  (setq consult-find-args (nh--build-find-command))
  (setq consult-fd-args (nh--build-fd-command))
  (setq consult-ripgrep-args (nh--build-ripgrep-command))

  (setq consult-async-min-input 0)
  (setq consult-async-input-throttle 0.1)
  (setq consult-async-input-debounce 0.1)

  (setq consult-async-refresh-delay 0.0)
  (setq consult-preview-key 'any)

  (setq consult-project-function #'consult--default-project-function)

  (setq consult-async-split-style 'perl)

  (setq consult-line-start-from-top nil))

;; consult-dir: jump to a recent or bookmarked folder.
;; GitHub: https://github.com/karthink/consult-dir
(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-j" . consult-dir-jump-file)))

;; ag: search with The Silver Searcher.
;; GitHub: https://github.com/Wilfred/ag.el
(use-package ag
  :ensure t)

;; projectile: find files, search and run commands per project.
;; GitHub: https://github.com/bbatsov/projectile
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :commands (projectile-find-file projectile-switch-project projectile-ag projectile-mode)
  :demand t
  :init
  (setq projectile-track-known-projects-automatically t
        projectile-completion-system 'default
        projectile-enable-caching t
        projectile-indexing-method 'alien
        projectile-project-root-files '(".projectile" ".git" ".hg" ".svn" ".bzr" "_darcs"
                                         "package.json" "Gemfile" "requirements.txt"
                                         "setup.py" "pom.xml" "build.gradle" "Cargo.toml"
                                         "go.mod" "tsconfig.json" "jsconfig.json"
                                         "composer.json" "Makefile" "CMakeLists.txt"))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (dolist (dir nh/globally-ignored-directories)
    (add-to-list 'projectile-globally-ignored-directories dir))
  (dolist (pattern nh/globally-ignored-file-extensions)
    (add-to-list 'projectile-globally-ignored-files pattern))

  (when (executable-find "fd")
    (setq projectile-generic-command
          (concat "fd . -0 --type f --color=never " (nh--build-fd-shell-command))))
  (when (and (not (executable-find "fd")) (executable-find "rg"))
    (setq projectile-generic-command
          (concat "rg --files --null --color=never " (nh--build-ripgrep-shell-command))))

  (unless (executable-find "ag")
    (message "[Projectile] Warning: 'ag' (The Silver Searcher) is not installed."))

  (projectile-mode 1))

;; flycheck: checks code for errors as you edit.
;; GitHub: https://github.com/flycheck/flycheck
(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :hook ((prog-mode . flycheck-mode)
         (emacs-lisp-mode . flycheck-mode))
  :custom
  (flycheck-idle-change-delay 1)
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-disabled-checkers '())
  (flycheck-display-errors-delay 0.5)
  (flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  :bind (:map flycheck-mode-map
              ("M-n" . flycheck-next-error)
              ("M-p" . flycheck-previous-error)
              ("C-c ! l" . flycheck-list-errors)
              ("C-c ! c" . flycheck-buffer)
              ("C-c ! v" . flycheck-verify-setup))
  :config
  (setq flycheck-emacs-lisp-check-declare t)

  (add-to-list 'display-buffer-alist
               '("\\*Flycheck errors\\*" (display-buffer-pop-up-window)))
  (add-to-list 'display-buffer-alist
               '("\\*Warnings\\*" (display-buffer-pop-up-window)))

  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (setq-local flycheck-disabled-checkers '())
              (setq-local flycheck-idle-change-delay 0.5))))

;; flycheck-pos-tip: shows Flycheck errors in a tooltip.
;; GitHub: https://github.com/flycheck/flycheck-pos-tip
(use-package flycheck-pos-tip
  :ensure t
  :after flycheck
  :if (display-graphic-p)
  :config
  (flycheck-pos-tip-mode))

;; corfu: completion popup inside the buffer.
;; GitHub: https://github.com/minad/corfu
(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-preview-delay 0.2)
  (corfu-popupinfo-mode 1)
  (corfu-popupinfo-delay '(0.5 . 0.2))
  (corfu-separator ?\s)
  (corfu-quit-at-boundary 'separator)
  (corfu-scroll-margin 5)
  :init
  :config
  (global-corfu-mode)
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (add-to-list 'completion-at-point-functions #'cape-elisp-symbol))))

;; cape: extra completion sources such as file names and words from open buffers.
;; GitHub: https://github.com/minad/cape
(use-package cape
  :ensure t
  :defer t
  :init
  (add-hook 'completion-at-point-functions #'cape-file t)
  (add-hook 'completion-at-point-functions #'cape-dabbrev t)
  (add-hook 'completion-at-point-functions #'cape-keyword t)
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (add-hook 'completion-at-point-functions #'cape-elisp-symbol t t))))

;; kind-icon: icons in the Corfu popup for each kind of completion.
;; GitHub: https://github.com/jdtsmith/kind-icon
(use-package kind-icon
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; yasnippet: expands short keys into code templates.
;; GitHub: https://github.com/joaotavora/yasnippet
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1)
  ;; yasnippet-snippets: a community collection of snippets.
  ;; GitHub: https://github.com/AndreaCrotti/yasnippet-snippets
  (use-package yasnippet-snippets
    :ensure t
    :after yasnippet
    :config
    (yasnippet-snippets-initialize)))

;; lsp-mode: Language Server Protocol client.
;; GitHub: https://github.com/emacs-lsp/lsp-mode
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook ((prog-mode . (lambda ()
                        ;; Tide handles these modes. If ts-ls attaches too, the two break highlighting.
                        (unless (or (derived-mode-p 'emacs-lisp-mode)
                                    (derived-mode-p 'typescript-mode)
                                    (derived-mode-p 'js2-mode)
                                    (derived-mode-p 'rjsx-mode)
                                    (derived-mode-p 'web-mode))
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

  (setq lsp-auto-guess-root t)
  (setq lsp-prefer-workspace-root t)

  (setq lsp-project-root-files '(".git" ".projectile" "package.json" "Gemfile"
                                  "Cargo.toml" "go.mod" "pom.xml" "build.gradle"
                                  "tsconfig.json" "jsconfig.json" ".env"
                                  "Makefile" "CMakeLists.txt" ".gitignore"))

  (setq lsp-enable-file-watchers t)
  (setq lsp-file-watch-threshold 1000)

  (require 'lsp-headerline)
  (require 'lsp-modeline)
  (require 'lsp-lens)
  (add-hook 'lsp-mode-hook #'lsp-lens-mode)
  (add-hook 'lsp-mode-hook #'lsp-modeline-workspace-status-mode)
  (add-hook 'lsp-mode-hook #'lsp-headerline-breadcrumb-mode)

  (setq lsp-modeline-code-actions-enable t)
  (setq lsp-modeline-diagnostics-enable t)
  (setq lsp-signature-auto-activate t)
  (setq lsp-signature-render-documentation t)
  (setq lsp-hover-enable t)
  (setq lsp-eldoc-enable-hover t)

  (setq lsp-diagnostics-provider :flycheck)
  (setq lsp-flycheck-live-reporting t)

  ;; IO logging slows LSP down.
  (setq lsp-print-io nil)
  (setq lsp-log-io nil)

  ;; Some servers send text edits with nil positions; skip those instead of erroring.
  (defadvice lsp--apply-text-edit (around lsp-handle-nil-positions activate)
    "Handle nil positions in LSP text edits."
    (condition-case err
        ad-do-it
      (wrong-type-argument
       (message "LSP: Ignoring text edit with invalid position: %s" err)
       nil)))

  (with-eval-after-load 'lsp-mode
    (defun nh/safe-lsp-hover ()
      "Safe wrapper around lsp-hover that handles errors."
      (interactive)
      (condition-case err
          (lsp-hover)
        (error
         (message "LSP hover error: %s" (error-message-string err)))))

    (define-key lsp-mode-map [remap xref-find-definitions] 'lsp-find-definition)
    (define-key lsp-mode-map (kbd "C-c l h") 'nh/safe-lsp-hover))

  (defun nh/lsp-describe-workspace ()
    "Describe the current LSP workspace and project root."
    (interactive)
    (if (bound-and-true-p lsp-mode)
        (message "LSP servers: %s\nProjectile root: %s\nLSP root: %s"
                 (mapconcat #'lsp--workspace-print (lsp-workspaces) ", ")
                 (projectile-project-root)
                 (lsp-workspace-root))
      (message "LSP mode is not active in this buffer")))

  (defun nh/lsp-restart-workspace ()
    "Restart the LSP workspace for the current buffer."
    (interactive)
    (when (bound-and-true-p lsp-mode)
      (lsp-workspace-restart (lsp--read-workspace))
      (message "LSP workspace restarted"))))

;; lsp-ui: popups and sideline notes for LSP results.
;; GitHub: https://github.com/emacs-lsp/lsp-ui
(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-show-with-cursor nil)
  (lsp-ui-doc-show-with-mouse t)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-max-width 120)
  (lsp-ui-doc-max-height 15)
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-doc-use-webkit nil)
  (lsp-ui-doc-text-scale-level 0)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-alignment 'window)
  (lsp-ui-doc-position 'top)  ; Position above cursor
  (lsp-ui-doc-delay 0.2)  ; Small delay before showing
  (lsp-ui-doc-border (face-attribute 'vertical-border :foreground))
  (lsp-ui-doc-frame-parameters
   '((internal-border-width . 15)
     (left-fringe . 10)
     (right-fringe . 10)))

  :config
  (setq lsp-ui-doc-child-frame-border-width 3)  ; Add spacing around the frame

  ;; Push the doc popup away from point so it doesn't cover the current line.
  (defun nh/lsp-ui-doc-move-with-offset (orig-fun &rest args)
    "Add offset when moving the doc frame."
    (apply orig-fun args)
    (when (and (boundp 'lsp-ui-doc--frame)
               lsp-ui-doc--frame
               (frame-live-p lsp-ui-doc--frame))
      (let* ((frame-pos (frame-position lsp-ui-doc--frame))
             (x (car frame-pos))
             (y (cdr frame-pos))
             (offset (if (eq lsp-ui-doc-position 'top)
                        -80
                      80)))  ; Move down 80 pixels when on bottom
        (set-frame-position lsp-ui-doc--frame x (+ y offset)))))

  (with-eval-after-load 'lsp-ui-doc
    (advice-add 'lsp-ui-doc--move-frame :around #'nh/lsp-ui-doc-move-with-offset))

  (setq lsp-ui-flycheck-enable t)
  (setq lsp-ui-flycheck-list-position 'right)
  (setq lsp-ui-flycheck-live-reporting t)

  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-sideline-show-symbol t)
  (setq lsp-ui-sideline-ignore-duplicate t)
  (setq lsp-ui-sideline-delay 0.5)

  (setq lsp-ui-peek-enable t)
  (setq lsp-ui-peek-peek-height 20)
  (setq lsp-ui-peek-list-width 50)
  (setq lsp-ui-peek-fontify 'on-demand)

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
  (defun nh/configure-lsp-ui-faces ()
    "Configure lsp-ui faces to match the current theme."
    (let* ((default-bg (face-attribute 'default :background))
           (mode-line-bg (face-attribute 'mode-line :background))
           (mode-line-inactive-bg (face-attribute 'mode-line-inactive :background))
           (header-bg mode-line-inactive-bg)  ; Use darker background for header
           (doc-bg default-bg))  ; Use main background for documentation

      (custom-set-faces
       `(lsp-ui-doc-header ((t (:inherit font-lock-keyword-face
                               :background ,header-bg
                               :foreground ,(face-attribute 'font-lock-keyword-face :foreground)
                               :weight bold
                               :height 1.1  ; Slightly larger
                               :box (:line-width (4 . 4) :color ,header-bg)))))  ; Padding around text
       `(lsp-ui-doc-background ((t (:background ,doc-bg))))
       `(lsp-ui-doc-url ((t (:inherit link :background ,doc-bg))))
       `(lsp-ui-doc ((t (:background ,doc-bg))))
       `(lsp-ui-doc-markdown-code-block-face ((t (:background ,doc-bg)))))

      (setq lsp-ui-doc-border (face-attribute 'vertical-border :foreground))

      (message "LSP-UI colors set: header=%s, doc=%s" header-bg doc-bg))

    (when (and (fboundp 'lsp-ui-doc--delete-frame)
               (boundp 'lsp-ui-doc--frame)
               lsp-ui-doc--frame)
      (lsp-ui-doc--delete-frame)))

  (nh/configure-lsp-ui-faces)

  (add-hook 'after-load-theme-hook #'nh/configure-lsp-ui-faces)

  (when lsp-ui-doc-use-webkit
    (setq lsp-ui-doc-webkit-background-color
          (face-attribute 'mode-line-inactive :background))))

  (with-eval-after-load 'lsp-ui-doc
    (when lsp-ui-doc-use-webkit
      (setq lsp-ui-doc-webkit-background-color
            (face-attribute 'mode-line-inactive :background)))

    ;; Markdown rendering gives code blocks a white background; remove it.
    (defun nh/lsp-ui-doc-remove-code-background (orig-fn &rest args)
      "Remove white background from code blocks in lsp-ui-doc."
      (let ((result (apply orig-fn args)))
        (when (get-buffer " *lsp-ui-doc*")
          (with-current-buffer " *lsp-ui-doc*"
            (let ((inhibit-read-only t)
                  (doc-bg (face-attribute 'mode-line-inactive :background)))
              (save-excursion
                (goto-char (point-min))
                (while (re-search-forward "`[^`]+`" nil t)
                  (let ((start (match-beginning 0))
                        (end (match-end 0)))
                    (add-face-text-property start end
                                          `(:background ,doc-bg) t)))
                (goto-char (point-min))
                (while (re-search-forward "```[^`]*```" nil t)
                  (let ((start (match-beginning 0))
                        (end (match-end 0)))
                    (add-face-text-property start end
                                          `(:background ,doc-bg) t)))))))
        result))

    (advice-add 'lsp-ui-doc--render-buffer :around #'nh/lsp-ui-doc-remove-code-background))

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

      (cond
       ((null border-val) (setq lsp-ui-doc-border nil))
       ((eq border-val 'custom)
        (setq lsp-ui-doc-border (read-color "Border color: ")))
       ((symbolp border-val)
        (setq lsp-ui-doc-border (face-attribute border-val :foreground))))

      (when (eq padding-val 'custom)
        (setq padding-val (read-number "Padding (pixels): " 10)))

      (setq lsp-ui-doc-frame-parameters
            `((internal-border-width . ,padding-val)))

      (when (and (boundp 'lsp-ui-doc--frame) lsp-ui-doc--frame)
        (lsp-ui-doc--delete-frame))

      (message "Border: %s, Padding: %dpx. Hover to see changes."
               (or lsp-ui-doc-border "none") padding-val)))

;; whitespace: highlights trailing spaces and long lines.
(use-package whitespace
  :ensure nil
  :diminish whitespace-mode
  :init
  (add-hook 'prog-mode-hook #'whitespace-mode)
  :config
  (setq whitespace-style '(face trailing tabs lines-tail)))

;; Disabled: ws-butler trims trailing spaces only on lines you edited.
;; GitHub: https://github.com/lewang/ws-butler
;; (use-package ws-butler
;;   :diminish ws-butler-mode
;;   :ensure t
;;   :config
;;   (setq ws-butler-keep-whitespace-before-point nil)
;;   (ws-butler-global-mode))

(defun nh/consult-line-reverse ()
  "Search backwards using consult-line."
  (interactive)
  (let ((current-line (line-number-at-pos))
        (consult-line-start-from-top nil))
    (consult-line)
    ;; If consult-line didn't move point, search backward instead.
    (when (eq current-line (line-number-at-pos))
      (isearch-backward))))

(defun nh/consult-ripgrep-project ()
  "Run `consult-ripgrep` in the project root, or current directory if no project."
  (interactive)
  (if-let ((project (project-current)))
      (consult-ripgrep (project-root project))
    (consult-ripgrep default-directory)))

(global-set-key (kbd "C-r") #'nh/consult-line-reverse)
;;(global-set-key (kbd "C-c p s") #'nh/consult-ripgrep-project)

(define-key isearch-mode-map (kbd "M-s l") #'consult-line)
(define-key isearch-mode-map (kbd "M-s L") #'consult-line-multi)

(define-key minibuffer-local-map (kbd "M-r") #'consult-history)

(defun nh/show-command-history ()
  "Show the extended command history (M-x history)."
  (interactive)
  (let ((command (completing-read "Recent commands: " extended-command-history)))
    (command-execute (intern command))))

(defun nh/clear-command-history ()
  "Clear the M-x command history."
  (interactive)
  (when (yes-or-no-p "Clear M-x command history? ")
    (setq extended-command-history nil)
    (message "M-x command history cleared")))

(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)

(setq completion-show-inline-help nil
      completion-auto-help t
      completion-cycle-threshold 1
      completions-detailed t
      completion-show-help t
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t
      completion-auto-select nil
      completions-format 'one-column
      tab-always-indent t)


(provide 'nh-autocompletion)

;;; nh-autocompletion.el ends here
