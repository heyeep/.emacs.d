;;; nh-ruby.el --- Modern Ruby Development Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Modern Ruby development configuration for Emacs 30.1
;; Includes enhanced editing, navigation, Rails support, and LSP integration.

;;; Code:

;; Custom function to handle CocoaPods files
(defun nh/ruby-check-for-cocoapods ()
  "Check if the Ruby file is a CocoaPods file and disable flycheck if needed."
  (when (and buffer-file-name
             (string-match-p "Podfile\\'" (buffer-file-name)))
    (when (bound-and-true-p flycheck-mode)
      (flycheck-mode -1)
      (message "Flycheck disabled for CocoaPods file"))))

;; Enhanced Ruby mode settings
(defun nh/ruby-mode-setup ()
  "Setup function for Ruby mode enhancements."
  ;; Indentation preferences
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 2)
  (setq-local ruby-indent-level 2)

  ;; Better comment behavior
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+\\s-*")

  ;; Enable auto-pairing for Ruby
  (when (fboundp 'electric-pair-local-mode)
    (electric-pair-local-mode 1)))

;; Setup Ruby with LSP and Flycheck working together
(defun nh/setup-ruby-lsp-flycheck ()
  "Setup Ruby with LSP and Flycheck working together."
  (setq-local lsp-diagnostics-provider :flycheck)
  (setq-local flycheck-relevant-checkers '(lsp))
  (flycheck-mode 1)
  (lsp-deferred))

;; Configure LSP for Ruby modes
(with-eval-after-load 'lsp-mode
  ;; This tells lsp-mode to use the ruby-lsp executable when it detects
  ;; a ruby project. It will automatically run it via `bundle exec` if
  ;; a Gemfile is present, which is the correct behavior.
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("ruby-lsp"))
    :major-modes '(ruby-mode enh-ruby-mode)
    :server-id 'ruby-lsp-ls
    ;; Enable all features, including Rails support.
    :initialization-options
    '((:featuresConfiguration (:textDocument (:codeAction (:enabled t)
                                               :completion (:enabled t)
                                               :definition (:enabled t)
                                               :documentHighlight (:enabled t)
                                               :documentLink (:enabled t)
                                               :documentSymbol (:enabled t)
                                               :foldingRange (:enabled t)
                                               :formatting (:enabled t)
                                               :hover (:enabled t)
                                               :inlayHint (:enabled t)
                                               :onTypeFormatting (:enabled t)
                                               :references (:enabled t)
                                               :rename (:enabled t)
                                               :selectionRange (:enabled t)
                                               :semanticHighlighting (:enabled t)
                                               :signatureHelp (:enabled t)
                                               :workspaceSymbol (:enabled t))
                             :diagnostics (:enabled t)
                             :workspace (:didChangeWatchedFiles (:enabled t))
                             :experimental (:enabled t)
                             :rails (:enabled t))))
    :priority 20)))

;; (with-eval-after-load 'flycheck
;;   (add-hook 'flycheck-mode-hook #'flycheck-lsp-setup))

;; Ruby Mode: Major mode for editing Ruby files
;; Built-in Ruby major mode providing syntax highlighting, indentation, and
;; basic editing features for Ruby programming with support for various Ruby file types.
(use-package ruby-mode
  :ensure t
  :mode (("\\.rb\\'" . ruby-mode)
         ("Rakefile\\'" . ruby-mode)
         ("\\.rake\\'" . ruby-mode)
         ("\\.gemspec\\'" . ruby-mode)
         ("\\.ru\\'" . ruby-mode)
         ("\\.cap\\'" . ruby-mode)
         ("\\.thor\\'" . ruby-mode)
         ("\\.jbuilder\\'" . ruby-mode)
         ("\\.prawn\\'" . ruby-mode)
         ("\\.builder\\'" . ruby-mode)
         ("\\.rabl\\'" . ruby-mode)
         ("\\.rjs\\'" . ruby-mode)
         ("\\.rxml\\'" . ruby-mode)
         ("Gemfile\\'" . ruby-mode)
         ("Guardfile\\'" . ruby-mode)
         ("Capfile\\'" . ruby-mode)
         ("Vagrantfile\\'" . ruby-mode)
         ("Berksfile\\'" . ruby-mode)
         ("Cheffile\\'" . ruby-mode)
         ("Fastfile\\'" . ruby-mode)
         ("Appraisals\\'" . ruby-mode)
         ("\\.podspec\\'" . ruby-mode)
         ("Podfile\\'" . ruby-mode))
  :interpreter ("ruby" . ruby-mode)
  :hook ((ruby-mode . nh/ruby-check-for-cocoapods)
         (ruby-mode . nh/ruby-mode-setup)
         (ruby-mode . nh/setup-ruby-lsp-flycheck))
  :bind (:map ruby-mode-map
              ("C-c r u c" . nh/ruby-rubocop-check-current-file)
              ("C-c r u a" . nh/ruby-rubocop-autocorrect-current-file)
              ("C-c r t r" . nh/rails-routes)
              ("C-c r t d" . nh/rails-dbconsole)
              ("C-c r t g" . nh/rails-routes-grep)
              ("C-c r f n" . flycheck-next-error)
              ("C-c r f p" . flycheck-previous-error)
              ("C-c r f l" . flycheck-list-errors))
  :config
  ;; Do not insert encoding magic comment in new Ruby files
  (setq ruby-insert-encoding-magic-comment nil)

  ;; Better alignment for Ruby method calls
  (setq ruby-align-to-stmt-keywords '(begin if unless while until case for def class module))
  (setq ruby-align-chained-calls t))

;; Enhanced Ruby Mode: More features for Ruby editing
;; Advanced Ruby major mode with enhanced syntax highlighting, better
;; indentation, and additional features beyond the built-in ruby-mode.
;; GitHub: https://github.com/zenspider/enhanced-ruby-mode
(use-package enh-ruby-mode
  :ensure t
  :mode (("\\.rb\\'" . enh-ruby-mode)
         ("Rakefile\\'" . enh-ruby-mode)
         ("\\.rake\\'" . enh-ruby-mode)
         ("\\.gemspec\\'" . enh-ruby-mode)
         ("\\.ru\\'" . enh-ruby-mode)
         ("\\.cap\\'" . enh-ruby-mode)
         ("\\.thor\\'" . enh-ruby-mode)
         ("\\.jbuilder\\'" . enh-ruby-mode)
         ("\\.prawn\\'" . enh-ruby-mode)
         ("\\.builder\\'" . enh-ruby-mode)
         ("\\.rabl\\'" . enh-ruby-mode)
         ("\\.rjs\\'" . enh-ruby-mode)
         ("\\.rxml\\'" . enh-ruby-mode)
         ("Gemfile\\'" . enh-ruby-mode)
         ("Guardfile\\'" . enh-ruby-mode)
         ("Capfile\\'" . enh-ruby-mode)
         ("Vagrantfile\\'" . enh-ruby-mode)
         ("Berksfile\\'" . enh-ruby-mode)
         ("Cheffile\\'" . enh-ruby-mode)
         ("Fastfile\\'" . enh-ruby-mode)
         ("Appraisals\\'" . enh-ruby-mode)
         ("\\.podspec\\'" . enh-ruby-mode)
         ("Podfile\\'" . enh-ruby-mode))
  :interpreter ("ruby" . enh-ruby-mode)
  :hook ((enh-ruby-mode . nh/ruby-check-for-cocoapods)
         (enh-ruby-mode . nh/ruby-mode-setup)
         (enh-ruby-mode . nh/setup-ruby-lsp-flycheck))
  :bind (:map enh-ruby-mode-map
              ("C-c r u c" . nh/ruby-rubocop-check-current-file)
              ("C-c r u a" . nh/ruby-rubocop-autocorrect-current-file)
              ("C-c r t r" . nh/rails-routes)
              ("C-c r t d" . nh/rails-dbconsole)
              ("C-c r t g" . nh/rails-routes-grep)
              ("C-c r f n" . flycheck-next-error)
              ("C-c r f p" . flycheck-previous-error)
              ("C-c r f l" . flycheck-list-errors))
  :config
  ;; Enhanced Ruby mode specific settings
  (setq enh-ruby-add-encoding-comment-on-save nil)
  (setq enh-ruby-deep-indent-paren nil)
  (setq enh-ruby-hanging-brace-indent-level 2))

;; Auto-detect Rails console function
(defun nh/inf-ruby-console-auto ()
  "Automatically start appropriate Ruby console."
  (interactive)
  (cond
   ((file-exists-p "Gemfile") (inf-ruby-console-auto))
   (t (inf-ruby))))

;; Inf Ruby: Interactive Ruby REPL
;; Provides an interactive Ruby REPL within Emacs, supporting various Ruby
;; implementations and automatic detection of Rails projects for console access.
;; GitHub: https://github.com/nonsequitur/inf-ruby
(use-package inf-ruby
  :ensure t
  :hook ((ruby-mode enh-ruby-mode) . inf-ruby-minor-mode)
  :config
  ;; Better REPL setup
  (setq inf-ruby-default-implementation "pry")
  (setq inf-ruby-eval-binding "Pry.toplevel_binding")

  :bind (:map ruby-mode-map
              ("C-c r r" . nh/inf-ruby-console-auto)
              ("C-c r l" . inf-ruby-console-auto)
              ("C-c r e" . ruby-send-line)
              ("C-c r b" . ruby-send-buffer)
              ("C-c r R" . ruby-send-region)))

;; Robe: IDE-like code navigation and documentation for Ruby
;; Provides intelligent code completion, navigation, and documentation for Ruby
;; development with REPL-based introspection and method lookup capabilities.
;; GitHub: https://github.com/dgutov/robe
(use-package robe
  :ensure t
  :hook (((ruby-mode enh-ruby-mode) . robe-mode))
  :config
  ;; Custom function to ensure Robe starts correctly with project environment
  (defun nh/robe-safe-start ()
    "Start Robe safely with appropriate environment settings."
    (interactive)
    (let ((default-directory (or (projectile-project-root)
                                 default-directory)))
      ;; Ensure asdf shims are in the PATH for this process
      (when (file-exists-p "~/.asdf/shims")
        (setenv "PATH" (concat (getenv "PATH") ":" (expand-file-name "~/.asdf/shims")))
        (setq exec-path (append exec-path (list (expand-file-name "~/.asdf/shims")))))
      
      ;; Set environment variables to help Robe find the gems
      (setenv "RUBYOPT" "-rpry -rpry-doc -rreadline")
      
      ;; Start inf-ruby first if not running
      (unless (comint-check-proc inf-ruby-buffer)
        (condition-case nil
            (inf-ruby-console-auto)
          (error (inf-ruby))))
      
      ;; Now start Robe
      (condition-case err
          (progn
            (call-interactively 'robe-start)
            (message "Robe started successfully"))
        (error
         (message "Robe start error: %s" (error-message-string err))
         (display-warning 'robe (format "Failed to start Robe: %s" (error-message-string err)))))
      
      ;; Reset environment after starting Robe
      (setenv "RUBYOPT" nil)))
  
  (advice-add 'robe-start :override #'nh/robe-safe-start)
  
  (add-to-list 'completion-at-point-functions #'robe-complete-at-point)
  :bind (:map robe-mode-map
              ("C-c r d" . robe-doc)
              ("C-c r j" . robe-jump)
              ("C-c r J" . robe-jump-to-module)))

;; Projectile Rails: Rails-specific project navigation
;; Enhances Projectile with Rails-specific navigation commands for quickly
;; jumping between models, views, controllers, and other Rails components.
;; GitHub: https://github.com/asok/projectile-rails
(use-package projectile-rails
  :ensure t
  :diminish projectile-rails-mode
  :hook ((ruby-mode enh-ruby-mode web-mode) . projectile-rails-on)
  :config
  ;; Enhanced Rails navigation
  (setq projectile-rails-add-keywords t)
  (setq projectile-rails-discover-bind "C-c r f")

  :bind (:map projectile-rails-mode-map
              ("C-c r m" . projectile-rails-find-model)
              ("C-c r c" . projectile-rails-find-controller)
              ("C-c r v" . projectile-rails-find-view)
              ("C-c r h" . projectile-rails-find-helper)
              ("C-c r s" . projectile-rails-find-spec)
              ("C-c r t" . projectile-rails-find-test)
              ("C-c r M" . projectile-rails-find-migration)
              ("C-c r n" . projectile-rails-find-initializer)
              ("C-c r g" . projectile-rails-goto-file-at-point)
              ("C-c r R" . projectile-rails-console)))

(with-eval-after-load 'lsp-ui
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-doc-enable t))

(defun nh/ruby-bundle-exec (command)
  "Run COMMAND with bundle exec in the project root."
  (interactive "sBundle exec: ")
  (let ((default-directory (or (projectile-project-root)
                               default-directory)))
    (async-shell-command (format "bundle exec %s" command))))

;; RuboCop integration
(defun nh/ruby-rubocop-check-current-file ()
  "Run RuboCop on the current file."
  (interactive)
  (when buffer-file-name
    (nh/ruby-bundle-exec (format "rubocop %s" (shell-quote-argument buffer-file-name)))))

(defun nh/ruby-rubocop-autocorrect-current-file ()
  "Run RuboCop with auto-correct on the current file."
  (interactive)
  (when buffer-file-name
    (nh/ruby-bundle-exec (format "rubocop -a %s" (shell-quote-argument buffer-file-name)))
    (revert-buffer t t)))

;; Rails-specific functions
(defun nh/rails-routes ()
  "Show Rails routes."
  (interactive)
  (nh/ruby-bundle-exec "rails routes"))

(defun nh/rails-dbconsole ()
  "Open Rails database console."
  (interactive)
  (nh/ruby-bundle-exec "rails dbconsole"))

(defun nh/rails-routes-grep ()
  "Grep through Rails routes output."
  (interactive)
  (let ((search-term (read-string "Search routes for: ")))
    (when (and search-term (not (string-empty-p search-term)))
      (let ((buffer-name "*Rails Routes Search*"))
        (with-current-buffer (get-buffer-create buffer-name)
          (erase-buffer)
          (insert (format "Rails Routes matching '%s':\n\n" search-term))
          (insert (shell-command-to-string
                   (format "cd %s && bundle exec rails routes | grep -i '%s'"
                           (or (projectile-project-root) default-directory)
                           search-term)))
          (goto-char (point-min))
          (view-mode 1))
        (switch-to-buffer-other-window buffer-name)))))


;;;###autoload
(defun nh/ruby-mode ()
  "Bootstrap Ruby mode configuration."
  ;; Update auto-mode-alist entries that point to this function
  (dolist (alist auto-mode-alist)
    (when (eq (cdr alist) 'nh/ruby-mode)
      (setf (cdr alist) 'ruby-mode)))
  (ruby-mode))

(provide 'nh-ruby)

;;; nh-ruby.el ends here
