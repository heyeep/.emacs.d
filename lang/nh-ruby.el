;;; nh-ruby.el --- ruby -*- lexical-binding: t; -*-

;;; Commentary:

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
  ;; Explicitly disable all Ruby-related checkers except LSP
  (setq-local flycheck-disabled-checkers '(ruby-rubocop ruby-reek ruby-rubylint ruby))
  (flycheck-mode 1)
  (lsp-deferred))

;; Configure LSP for Ruby modes - using ruby-lsp only
(with-eval-after-load 'lsp-mode
  ;; Disable Solargraph and other Ruby servers completely
  (setq lsp-disabled-clients '(solargraph rubocop-ls steep-ls typeprof-ls))

  ;; Disable RuboCop diagnostics globally
  (setq lsp-rubocop-use-bundler nil)
  (setq lsp-ruby-lsp-use-bundler nil)

  ;; Custom handler for ruby-lsp code lens actions (Run/Debug buttons)
  (defun nh/ruby-lsp-execute-command-handler (command)
    "Custom handler for ruby-lsp commands to fix RSpec execution."
    ;; Debug output
    (message "=== Ruby LSP Command Handler ===")
    (message "Raw command: %S" command)
    (message "Command type: %s" (type-of command))

    ;; If command is nil or empty, and we're in a spec file, just run the spec
    (if (and (or (null command)
                 (and (stringp command) (string= command ""))
                 (and (hash-table-p command) (null (gethash "command" command))))
             (derived-mode-p 'ruby-mode 'enh-ruby-mode)
             (string-match-p "_spec\\.rb\\'" (or buffer-file-name "")))
        (progn
          (message "Command is nil/empty, running spec at point")
          (nh/run-rspec-at-point))
      ;; Otherwise continue with normal processing

      ;; Handle the case where command might be wrapped
      (when (and (listp command) (= (length command) 1))
        (setq command (car command)))

      (let* ((command-id (cond
                         ((stringp command) command)
                         ((hash-table-p command) (gethash "command" command))
                         (t nil)))
             (arguments (when (hash-table-p command)
                         (gethash "arguments" command))))

        (message "Extracted command-id: %s" command-id)
        (message "Arguments: %S" arguments)

        ;; Check if this is a shell command that ruby-lsp wants us to run
        (cond
         ;; If still nil but in spec file, run spec
         ((and (null command-id)
               (derived-mode-p 'ruby-mode 'enh-ruby-mode)
               (string-match-p "_spec\\.rb\\'" (or buffer-file-name "")))
          (message "No command ID, running spec at point")
          (nh/run-rspec-at-point))

         ;; Direct shell command from ruby-lsp
         ((and (stringp command-id)
               (or (string-match-p "rspec" command-id)
                   (string-match-p "ruby.*spec" command-id)
                   (string-match-p "bundle.*rspec" command-id)))
          (message "Detected direct RSpec command: %s" command-id)
          (let* ((project-root (or (locate-dominating-file default-directory "Gemfile")
                                  (locate-dominating-file default-directory ".git")
                                  default-directory))
                 (default-directory project-root))
            ;; Fix the command if it's trying to run ./rspec
            (when (string-match "^\\./rspec" command-id)
              (setq command-id (replace-regexp-in-string "^\\./rspec" "bundle exec rspec" command-id)))
            (message "Running command: %s from %s" command-id default-directory)
            (compile command-id)))

         ;; Ruby LSP test commands
         ((and command-id (or (string-match-p "test" command-id)
                             (string-match-p "Test" command-id)
                             (string-match-p "spec" command-id)))
          (message "Intercepting test command: %s" command-id)
          ;; Try to extract file and line from arguments
          (let* ((file-path buffer-file-name)
                 (line-number (line-number-at-pos))
                 (project-root (or (locate-dominating-file file-path "Gemfile")
                                 (locate-dominating-file file-path ".git")))
                 (default-directory (or project-root default-directory)))
            (when file-path
              (let* ((relative-path (if project-root
                                       (file-relative-name file-path project-root)
                                     file-path))
                     (rspec-command (format "bundle exec rspec %s:%s" relative-path line-number)))
                (message "Running: %s" rspec-command)
                (compile rspec-command)))))

         ;; Pass through other commands
         (t
          (message "Unknown command format, passing through: %s" command-id)
          (when command-id
            (lsp-execute-command command-id arguments)))))))

  ;; Override the LSP execute command for ruby-lsp
  (advice-add 'lsp-execute-command :around
              (lambda (orig-fun command &rest args)
                (when (and lsp-mode (lsp-workspaces))
                  (message "LSP execute-command called with: %S args: %S" command args))
                (if (and lsp-mode
                        (lsp-workspaces)
                        (equal (lsp--workspace-server-id (car (lsp-workspaces))) 'ruby-lsp-ls))
                    (nh/ruby-lsp-execute-command-handler (or command (car args)))
                  (apply orig-fun command args))))

  ;; Also intercept code lens action execution
  (advice-add 'lsp-execute-code-action :around
              (lambda (orig-fun action &rest args)
                (when (and lsp-mode (lsp-workspaces))
                  (message "LSP code action called with: %S" action))
                (if (and lsp-mode
                        (lsp-workspaces)
                        (equal (lsp--workspace-server-id (car (lsp-workspaces))) 'ruby-lsp-ls)
                        (hash-table-p action))
                    (let ((command (gethash "command" action)))
                      (when command
                        (nh/ruby-lsp-execute-command-handler command)))
                  (apply orig-fun action args))))

  ;; Intercept shell-command to fix rspec execution
  (defun nh/fix-rspec-shell-command (orig-fun command &rest args)
    "Fix RSpec commands that try to run ./rspec."
    (when (string-match-p "ruby.*rspec\\|^\\.?/?rspec" command)
      (message "Intercepted shell command: %s" command)
      ;; Fix various forms of rspec commands
      (setq command (replace-regexp-in-string "^\\./rspec" "bundle exec rspec" command))
      (setq command (replace-regexp-in-string "^rspec" "bundle exec rspec" command))
      (setq command (replace-regexp-in-string "ruby .*rspec" "bundle exec rspec" command))
      ;; Also fix the path if it's running from wrong directory
      (when (string-match "rspec \\(.*\\)" command)
        (let* ((spec-file (match-string 1 command))
               (project-root (or (locate-dominating-file default-directory "Gemfile")
                                (locate-dominating-file default-directory ".git"))))
          (when project-root
            (setq default-directory project-root))))
      (message "Fixed command: %s from directory: %s" command default-directory))
    (apply orig-fun command args))

  ;; Apply advice to shell-command functions
  (advice-add 'shell-command :around #'nh/fix-rspec-shell-command)
  (advice-add 'async-shell-command :around #'nh/fix-rspec-shell-command)
  (advice-add 'compile :around #'nh/fix-rspec-shell-command)

  ;; Also intercept the terminal/vterm commands that LSP might use
  (defun nh/fix-terminal-rspec-command (orig-fun command &rest args)
    "Fix RSpec commands in terminal."
    (when (and (stringp command) (string-match-p "rspec" command))
      (setq command (replace-regexp-in-string "^\\./rspec" "bundle exec rspec" command))
      (setq command (replace-regexp-in-string "^rspec" "bundle exec rspec" command)))
    (apply orig-fun command args))

  ;; Intercept various terminal commands
  (with-eval-after-load 'vterm
    (advice-add 'vterm-send-string :around #'nh/fix-terminal-rspec-command))
  (with-eval-after-load 'term
    (advice-add 'term-send-raw-string :around #'nh/fix-terminal-rspec-command))

  ;; Create rspec binstub if needed
  (defun nh/ensure-rspec-binstub ()
    "Ensure rspec binstub exists in current project."
    (interactive)
    (let* ((project-root (or (locate-dominating-file default-directory "Gemfile")
                            (locate-dominating-file default-directory ".git")))
           (bin-dir (when project-root (expand-file-name "bin" project-root)))
           (rspec-bin (when bin-dir (expand-file-name "rspec" bin-dir))))
      (when (and project-root (not (file-exists-p rspec-bin)))
        (when (y-or-n-p "Create bin/rspec stub for LSP code lens? ")
          (unless (file-exists-p bin-dir)
            (make-directory bin-dir t))
          (with-temp-file rspec-bin
            (insert "#!/usr/bin/env bash\n")
            (insert "# This is a wrapper for LSP code lens compatibility\n")
            (insert "bundle exec rspec \"$@\"\n"))
          (set-file-modes rspec-bin #o755)
          (message "Created %s" rspec-bin)))))

  ;; Intercept lsp--send-execute-command to see what's being sent
  (advice-add 'lsp--send-execute-command :before
              (lambda (command &optional args)
                (message "LSP sending execute command: %s with args: %S" command args)))

  ;; Also check lsp-perform-code-action
  (advice-add 'lsp-perform-code-action :around
              (lambda (orig-fun action &rest args)
                (message "Performing code action: %S" action)
                (apply orig-fun action args)))

  ;; Register ruby-lsp as the primary Ruby language server
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("ruby-lsp"))
    :major-modes '(ruby-mode enh-ruby-mode)
    :server-id 'ruby-lsp-ls
    ;; Enable all features, including Rails support, but disable RuboCop
    :initialization-options
    (lambda ()
      (list :initializationOptions
            (list :enabledFeatures ["codeActions" "diagnostics" "documentHighlights"
                                   "documentLink" "documentSymbols" "foldingRanges"
                                   "formatting" "hover" "inlayHint" "onTypeFormatting"
                                   "selectionRanges" "semanticHighlighting" "completion"
                                   "codeLens" "definition" "documentLink" "references"
                                   "signatureHelp" "typeHierarchy" "workspaceSymbol"]
                  :experimentalFeaturesEnabled t
                  :formatter "auto"
                  :linters (list :rubocop :json-false)
                  :testFramework "rspec")))
    :priority 100)))

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
              ("C-c r f l" . flycheck-list-errors)
              ;; RSpec bindings
              ("C-c r p f" . nh/rspec-run-current-file)
              ("C-c r p p" . nh/rspec-run-at-point)
              ("C-c r p r" . nh/rspec-rerun-last)
              ("C-c r p a" . nh/rspec-run-all))
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
              ("C-c r f l" . flycheck-list-errors)
              ("C-c r r" . nh/inf-ruby-console-auto)
              ("C-c r C" . nh/inf-ruby-rails-console)
              ("C-c r N" . nh/inf-ruby-rails-console-no-spring)
              ("C-c r d" . nh/rails-console-development)
              ("C-c r D" . nh/debug-rails-directory)
              ;; RSpec bindings
              ("C-c r p f" . nh/rspec-run-current-file)
              ("C-c r p p" . nh/rspec-run-at-point)
              ("C-c r p r" . nh/rspec-rerun-last)
              ("C-c r p a" . nh/rspec-run-all))
  :config
  ;; Enhanced Ruby mode specific settings
  (setq enh-ruby-add-encoding-comment-on-save nil)
  (setq enh-ruby-deep-indent-paren nil)
  (setq enh-ruby-hanging-brace-indent-level 2))

;; Better Rails console auto-detection that bypasses inf-ruby prompts
(defun nh/inf-ruby-console-auto ()
  "Start appropriate Ruby console WITHOUT any environment prompts."
  (interactive)
  (require 'inf-ruby nil t)
  ;; First, find the project root
  (let* ((buffer-dir (if buffer-file-name
                        (file-name-directory buffer-file-name)
                      default-directory))
         ;; Try multiple methods to find project root
         (project-root (or
                       ;; Try projectile first
                       (and (fboundp 'projectile-project-root)
                            (ignore-errors (projectile-project-root)))
                       ;; Try finding Gemfile
                       (locate-dominating-file buffer-dir "Gemfile")
                       ;; Try finding .git
                       (locate-dominating-file buffer-dir ".git")))
         ;; Set working directory
         (default-directory (if project-root
                              (file-name-as-directory project-root)
                            buffer-dir)))

    (message "Console directory: %s" default-directory)

    ;; Determine project type and start appropriate console
    (cond
     ;; Rails project detection - check multiple indicators
     ((and (file-exists-p (expand-file-name "Gemfile" default-directory))
           (or (file-exists-p (expand-file-name "config/application.rb" default-directory))
               (file-exists-p (expand-file-name "config/environment.rb" default-directory))
               (file-exists-p (expand-file-name "config/boot.rb" default-directory))
               (file-exists-p (expand-file-name "config.ru" default-directory))
               (file-exists-p (expand-file-name "app/controllers" default-directory))))
      (message "Rails project detected - starting Rails console in development mode...")
      ;; Set environment and start console directly - no prompting!
      (let ((process-environment (cons "RAILS_ENV=development"
                                     (cons "DISABLE_SPRING=1" process-environment)))
            ;; Ensure we use the inf-ruby buffer name format
            (inf-ruby-buffer-name "rails"))
        ;; Start the console directly with run-ruby
        (run-ruby "bundle exec rails console" "rails")))

     ;; Ruby project with Gemfile (non-Rails)
     ((file-exists-p (expand-file-name "Gemfile" default-directory))
      (message "Ruby project detected - starting IRB with bundler...")
      (run-ruby "bundle exec irb" "ruby"))

     ;; Plain Ruby project
     (t
      (message "Starting plain IRB...")
      (run-ruby "irb" "ruby")))))

;; Debug function to check project detection
(defun nh/ruby-debug-project-root ()
  "Debug function to show current directory and detected project root."
  (interactive)
  (let* ((current-dir default-directory)
         (projectile-root (when (fboundp 'projectile-project-root)
                           (projectile-project-root)))
         (gemfile-root (locate-dominating-file default-directory "Gemfile"))
         (rails-root (or (locate-dominating-file default-directory "config/application.rb")
                        (locate-dominating-file default-directory "config/environment.rb"))))
    (message "Current dir: %s\nProjectile root: %s\nGemfile root: %s\nRails root: %s"
             current-dir
             (or projectile-root "not found")
             (or gemfile-root "not found")
             (or rails-root "not found"))))

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

  ;; Make inf-ruby buffer read-only except at the prompt
  (add-hook 'inf-ruby-mode-hook
            (lambda ()
              ;; Make the buffer read-only except for the input area
              (setq-local comint-prompt-read-only t)
              ;; Prevent insertion before the process mark
              (setq-local comint-insert-mode t)
              ;; Move to end of buffer on input
              (setq-local comint-scroll-to-bottom-on-input t)
              ;; Keep the prompt at the bottom
              (setq-local comint-scroll-to-bottom-on-output t)
              ;; Highlight the prompt
              (setq-local comint-highlight-prompt t)

              ;; Add local keybinding to jump to prompt
              (local-set-key (kbd "C-c C-a") 'comint-bol)
              (local-set-key (kbd "C-c C-u") 'comint-kill-input)
              (local-set-key (kbd "C-c M-o") 'comint-clear-buffer)

              ;; Function to move to end of buffer if trying to type in read-only area
              (defun nh/inf-ruby-send-input-or-goto-end ()
                "If at prompt, send input. Otherwise, go to end of buffer."
                (interactive)
                (if (>= (point) (marker-position (process-mark (get-buffer-process (current-buffer)))))
                    (comint-send-input)
                  (goto-char (point-max))))

              ;; Override RET to use our function
              (local-set-key (kbd "RET") 'nh/inf-ruby-send-input-or-goto-end)))

  ;; REMOVED: advice-add was causing environment prompts - we'll use our own function instead

  ;; Modern advice to ensure inf-ruby runs from project root
  (advice-add 'inf-ruby-console-rails :around
              (lambda (orig-fun &rest args)
                "Run Rails console from project root or current directory."
                (let* ((project-root (or (and (fboundp 'projectile-project-root)
                                             (projectile-project-root))
                                        (locate-dominating-file default-directory "Gemfile")))
                       (default-directory (or project-root default-directory)))
                  (message "Running Rails console from: %s%s"
                           default-directory
                           (if project-root "" " (no project root found)"))
                  (apply orig-fun args))))

  ;; Also ensure regular inf-ruby runs from project root
  (advice-add 'inf-ruby :around
              (lambda (orig-fun &rest args)
                "Run inf-ruby from project root if in a project, otherwise current directory."
                (let* ((project-root (or (and (fboundp 'projectile-project-root)
                                             (projectile-project-root))
                                        (locate-dominating-file default-directory "Gemfile")))
                       (default-directory (or project-root default-directory)))
                  (apply orig-fun args))))

  ;; Force Rails console to use project root
  (defun nh/inf-ruby-rails-console ()
    "Start Rails console from project root (or current dir) in development environment."
    (interactive)
    (require 'inf-ruby nil t)
    (let* ((project-root (or (and (fboundp 'projectile-project-root)
                                 (projectile-project-root))
                            (locate-dominating-file default-directory "Gemfile")))
           (default-directory (or project-root default-directory))
           (process-environment (cons "RAILS_ENV=development" process-environment)))
      (message "Starting Rails console (development) from: %s%s"
               default-directory
               (if project-root "" " (no project root found)"))
      (if (fboundp 'run-ruby)
          (run-ruby "bundle exec rails console development" "rails")
        (async-shell-command "bundle exec rails console development"))))

  ;; Rails console without Spring (to avoid fork issues on macOS)
  (defun nh/inf-ruby-rails-console-no-spring ()
    "Start Rails console without Spring from project root (or current dir) in development environment."
    (interactive)
    (require 'inf-ruby nil t)
    (let* ((project-root (or (and (fboundp 'projectile-project-root)
                                 (projectile-project-root))
                            (locate-dominating-file default-directory "Gemfile")))
           (default-directory (or project-root default-directory))
           (process-environment (append '("DISABLE_SPRING=1" "RAILS_ENV=development")
                                       process-environment)))
      (message "Starting Rails console without Spring (development) from: %s%s"
               default-directory
               (if project-root "" " (no project root found)"))
      (if (fboundp 'run-ruby)
          (run-ruby "bundle exec rails console development" "rails-no-spring")
        (async-shell-command "bundle exec rails console development"))))

  ;; Debug what directory inf-ruby-console-auto actually uses
  (defun nh/debug-rails-directory ()
    "Debug function to show what directories are being detected."
    (interactive)
    (require 'inf-ruby nil t)
    (let* ((buffer-dir (if buffer-file-name
                          (file-name-directory buffer-file-name)
                        default-directory))
           (projectile-root (when (fboundp 'projectile-project-root)
                             (projectile-project-root)))
           (gemfile-root (locate-dominating-file buffer-dir "Gemfile"))
           (rails-indicators '("config/application.rb"
                              "config/environment.rb"
                              "config/boot.rb"
                              "app/controllers"))
           (rails-root nil))
      ;; Find Rails root
      (dolist (indicator rails-indicators)
        (when (not rails-root)
          (let ((found (locate-dominating-file buffer-dir indicator)))
            (when found (setq rails-root found)))))

      (message "=== Rails Directory Debug ===
Buffer directory: %s
Projectile root: %s
Gemfile root: %s
Rails root: %s
default-directory: %s
Gemfile exists at %s: %s
Rails files exist: app.rb=%s env.rb=%s boot.rb=%s"
               buffer-dir
               (or projectile-root "NOT FOUND")
               (or gemfile-root "NOT FOUND")
               (or rails-root "NOT FOUND")
               default-directory
               (or gemfile-root default-directory)
               (if gemfile-root
                   (file-exists-p (expand-file-name "Gemfile" gemfile-root))
                 "NO ROOT")
               (and gemfile-root (file-exists-p (expand-file-name "config/application.rb" gemfile-root)))
               (and gemfile-root (file-exists-p (expand-file-name "config/environment.rb" gemfile-root)))
               (and gemfile-root (file-exists-p (expand-file-name "config/boot.rb" gemfile-root)))))

    ;; Bind to C-c r D for easy access
    (global-set-key (kbd "C-c r D") 'nh/debug-rails-directory))

  ;; Direct Rails console function that works
  (defun nh/rails-console-development ()
    "Start Rails console in development mode - no prompts, just works."
    (interactive)
    (require 'inf-ruby nil t)
    ;; Get proper directory for file
    (let* ((buffer-dir (if buffer-file-name
                          (file-name-directory buffer-file-name)
                        default-directory))
           ;; Find project root
           (project-root (or
                         ;; Try projectile
                         (and (fboundp 'projectile-project-root)
                              (ignore-errors (projectile-project-root)))
                         ;; Try Gemfile
                         (locate-dominating-file buffer-dir "Gemfile")
                         ;; Last resort
                         buffer-dir))
           ;; Set directory
           (default-directory (file-name-as-directory project-root))
           ;; Set environment
           (process-environment (cons "RAILS_ENV=development"
                                    (cons "DISABLE_SPRING=1" process-environment))))

      (message "Rails console starting in: %s" default-directory)

      ;; Verify it's a Rails project
      (if (and (file-exists-p (expand-file-name "Gemfile" default-directory))
               (or (file-exists-p (expand-file-name "config/application.rb" default-directory))
                   (file-exists-p (expand-file-name "config/environment.rb" default-directory))
                   (file-exists-p (expand-file-name "config/boot.rb" default-directory))))
          ;; Start console directly
          (run-ruby "bundle exec rails console" "rails-dev")
        (error "Not in a Rails project! Current directory: %s" default-directory))))

  :bind (:map ruby-mode-map
              ("C-c r r" . nh/inf-ruby-console-auto)
              ("C-c r l" . inf-ruby-console-auto)
              ("C-c r C" . nh/inf-ruby-rails-console)
              ("C-c r N" . nh/inf-ruby-rails-console-no-spring)
              ("C-c r d" . nh/rails-console-development)
              ("C-c r D" . nh/debug-rails-directory)
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
  :diminish " Ⓡ"  ;; Show Ⓡ in modeline when Robe is active
  :config
  ;; Visual indicator when Robe is running
  (defun nh/robe-update-modeline ()
    "Update modeline to show Robe status."
    (setq robe-mode-string
          (if (robe-running-p)
              " Ⓡ✓"  ;; Green checkmark when running
            " Ⓡ⚠"))  ;; Warning when not running
    (force-mode-line-update))

  ;; Update modeline when Robe starts/stops
  (add-hook 'robe-mode-hook #'nh/robe-update-modeline)
  (advice-add 'robe-start :after (lambda (&rest _) (nh/robe-update-modeline)))

  ;; Enable company-mode for better completion UI if available
  (when (fboundp 'company-mode)
    (add-hook 'robe-mode-hook 'company-mode))
  ;; Better Robe start with environment setup
  (defun nh/robe-start-with-env (orig-fun &rest args)
    "Advice to ensure Robe starts with proper environment."
    (let ((default-directory (or (and (fboundp 'projectile-project-root)
                                     (ignore-errors (projectile-project-root)))
                                 (locate-dominating-file default-directory "Gemfile")
                                 default-directory))
          (orig-path (getenv "PATH"))
          (orig-rubyopt (getenv "RUBYOPT")))

      ;; Ensure asdf shims are in the PATH
      (when (file-exists-p "~/.asdf/shims")
        (setenv "PATH" (concat orig-path ":" (expand-file-name "~/.asdf/shims")))
        (setq exec-path (append exec-path (list (expand-file-name "~/.asdf/shims")))))

      ;; Set environment variables to help Robe find gems
      (setenv "RUBYOPT" "-rpry -rpry-doc")

      ;; Start inf-ruby first if not running
      (unless (and (boundp 'inf-ruby-buffer)
                   inf-ruby-buffer
                   (comint-check-proc inf-ruby-buffer))
        (condition-case nil
            (nh/inf-ruby-console-auto)
          (error
           (message "Starting fallback Ruby REPL for Robe...")
           (inf-ruby))))

      ;; Now call the original robe-start
      (unwind-protect
          (condition-case err
              (progn
                (apply orig-fun args)
                (message "Robe started successfully"))
            (error
             (message "Robe error: %s" (error-message-string err))
             (display-warning 'robe (format "Failed to start Robe: %s" (error-message-string err)))))
        ;; Always restore environment
        (setenv "PATH" orig-path)
        (setenv "RUBYOPT" orig-rubyopt))))

  ;; Use :around advice instead of :override to avoid recursion
  (advice-add 'robe-start :around #'nh/robe-start-with-env)

  (add-to-list 'completion-at-point-functions #'robe-complete-at-point)
  ;; Add a function to check Robe status
  (defun nh/robe-check-status ()
    "Check if Robe is running and show status."
    (interactive)
    (if (and (boundp 'robe-mode) robe-mode)
        (if (robe-running-p)
            (message "✓ Robe is ACTIVE and running! Try:
- Type 'Array.' and wait for completions
- C-c r j on a method to jump to definition
- C-c r d on a method for documentation
- M-. on any Ruby method/class to navigate")
          (message "✗ Robe mode is enabled but NOT running. Run M-x robe-start or C-c r S"))
      (message "✗ Robe mode is NOT enabled in this buffer")))

  ;; Test Robe functionality
  (defun nh/robe-test ()
    "Test Robe is working by checking completion."
    (interactive)
    (if (robe-running-p)
        (progn
          (message "Testing Robe...")
          ;; Try a simple completion test
          (let ((completions (robe-complete-thing "Array" nil)))
            (if completions
                (message "✓ Robe works! Found %d completions for 'Array'. Try typing 'Array.' in a Ruby buffer!"
                         (length completions))
              (message "✗ Robe might be having issues - try restarting with C-c r S"))))
      (message "✗ Robe not running! Start it with M-x robe-start")))

  ;; Simple visual test
  (defun nh/robe-quick-test ()
    "Quick visual test - insert Array. and trigger completion."
    (interactive)
    (when (derived-mode-p 'ruby-mode 'enh-ruby-mode)
      (insert "Array.")
      (if (featurep 'company)
          (company-complete)
        (completion-at-point))
      (message "If you see completions popup, Robe is working!")))

  ;; Diagnostic info
  (defun nh/robe-diagnostics ()
    "Show diagnostic information about Robe setup."
    (interactive)
    (let ((buffer (get-buffer "*robe-diagnostics*")))
      (when buffer (kill-buffer buffer))
      (with-current-buffer (get-buffer-create "*robe-diagnostics*")
        (erase-buffer)
        (insert "=== Robe Diagnostics ===\n\n")
        (insert (format "Robe mode enabled: %s\n" (if (and (boundp 'robe-mode) robe-mode) "YES" "NO")))
        (insert (format "Robe running: %s\n" (if (robe-running-p) "YES" "NO")))
        (insert (format "Ruby REPL buffer: %s\n" (if (and (boundp 'inf-ruby-buffer) inf-ruby-buffer) inf-ruby-buffer "None")))
        (insert (format "REPL process alive: %s\n"
                       (if (and (boundp 'inf-ruby-buffer) inf-ruby-buffer
                               (comint-check-proc inf-ruby-buffer))
                           "YES" "NO")))
        (insert "\n=== How to use Robe ===\n\n")
        (insert "1. Start a Ruby REPL: C-c r r\n")
        (insert "2. Start Robe: C-c r S (or M-x robe-start)\n")
        (insert "3. Test it:\n")
        (insert "   - Type 'String.' and see method completions\n")
        (insert "   - Put cursor on a method and press C-c r d for docs\n")
        (insert "   - Put cursor on a method and press C-c r j to jump to definition\n")
        (insert "   - Use M-. as alternative to jump to definition\n")
        (display-buffer (current-buffer)))))

  :bind (:map robe-mode-map
              ("C-c r d" . robe-doc)
              ("C-c r j" . robe-jump)
              ("C-c r J" . robe-jump-to-module)
              ("C-c r s" . nh/robe-check-status)
              ("C-c r S" . robe-start)
              ("C-c r T" . nh/robe-test)
              ("C-c r Q" . nh/robe-quick-test)
              ("C-c r ?" . nh/robe-diagnostics)))

;; LSP diagnostic functions
(defun nh/check-ruby-lsp-status ()
  "Check which Ruby LSP server is active and its configuration."
  (interactive)
  (if (bound-and-true-p lsp-mode)
      (let* ((workspace (lsp-workspaces))
             (server-id (when workspace
                         (lsp--workspace-server-id (car workspace))))
             (disabled-clients lsp-disabled-clients))
        (message "LSP Status:
Active server: %s
Disabled clients: %s
Diagnostics provider: %s
Flycheck checkers: %s"
                 (or server-id "None")
                 disabled-clients
                 lsp-diagnostics-provider
                 (when (bound-and-true-p flycheck-mode)
                   flycheck-enabled-checkers)))
    (message "LSP mode not active in this buffer")))

(defun nh/flycheck-diagnose-ruby ()
  "Diagnose Flycheck configuration for Ruby."
  (interactive)
  (if (bound-and-true-p flycheck-mode)
      (let ((checkers flycheck-enabled-checkers)
            (disabled flycheck-disabled-checkers)
            (relevant flycheck-relevant-checkers))
        (message "Flycheck Ruby Diagnostics:
Enabled checkers: %s
Disabled checkers: %s
Relevant checkers: %s
Current checker: %s"
                 checkers
                 disabled
                 relevant
                 flycheck-checker))
    (message "Flycheck not active")))

(defun nh/ruby-lsp-show-config ()
  "Show ruby-lsp configuration from the server."
  (interactive)
  (when (and (bound-and-true-p lsp-mode)
             (lsp-workspaces))
    (lsp-request-async
     "workspace/configuration"
     '(:items [(:section "ruby-lsp")])
     (lambda (config)
       (message "Ruby-LSP config: %s" config))
     :mode 'detached)))

;; Function to set up keybindings for Ruby modes
(defun nh/setup-ruby-keybindings (mode-map)
  "Set up keybindings for ruby MODE-MAP."
  (define-key mode-map (kbd "C-c l s") 'nh/check-ruby-lsp-status)
  (define-key mode-map (kbd "C-c l f") 'nh/flycheck-diagnose-ruby)
  (define-key mode-map (kbd "C-c l c") 'nh/ruby-lsp-show-config)
  (define-key mode-map (kbd "C-c l d") 'nh/ruby-check-diagnostics-source)
  (define-key mode-map (kbd "C-c l R") 'nh/ruby-lsp-disable-rubocop)
  (define-key mode-map (kbd "C-c l E") 'nh/ruby-lsp-env-disable-rubocop))

;; Apply keybindings to both Ruby modes
(with-eval-after-load 'ruby-mode
  (nh/setup-ruby-keybindings ruby-mode-map))

(with-eval-after-load 'enh-ruby-mode
  (nh/setup-ruby-keybindings enh-ruby-mode-map))

;; Function to check what's actually providing diagnostics
(defun nh/ruby-check-diagnostics-source ()
  "Check what's providing diagnostics in the current Ruby buffer."
  (interactive)
  (let ((messages '()))
    ;; Check LSP
    (when (bound-and-true-p lsp-mode)
      (push (format "LSP Mode: Active (Server: %s)"
                    (or (when (lsp-workspaces)
                          (lsp--workspace-server-id (car (lsp-workspaces))))
                        "None"))
            messages))

    ;; Check Flycheck
    (when (bound-and-true-p flycheck-mode)
      (push (format "Flycheck: Active (Checker: %s)" (or flycheck-checker "auto"))
            messages)
      (when flycheck-enabled-checkers
        (push (format "  Enabled: %s" flycheck-enabled-checkers) messages))
      (when flycheck-disabled-checkers
        (push (format "  Disabled: %s" flycheck-disabled-checkers) messages)))

    ;; Check for rubocop process
    (let ((procs (process-list)))
      (dolist (proc procs)
        (when (string-match-p "rubocop" (process-name proc))
          (push (format "RuboCop process found: %s" (process-name proc)) messages))))

    ;; Check ruby-lsp settings
    (when (and (bound-and-true-p lsp-mode) (lsp-workspaces))
      (let ((workspace (car (lsp-workspaces))))
        (when workspace
          (let ((settings (lsp--workspace-settings workspace)))
            (when settings
              (push (format "LSP Settings: %s" settings) messages))))))

    (message "%s" (string-join (reverse messages) "\n"))))


;; Function to create .ruby-lsp.yml to disable RuboCop
(defun nh/ruby-lsp-disable-rubocop ()
  "Create or update .ruby-lsp.yml in project root to disable RuboCop."
  (interactive)
  (let* ((project-root (or (and (fboundp 'projectile-project-root)
                               (projectile-project-root))
                          (locate-dominating-file default-directory "Gemfile")
                          default-directory))
         (config-file (expand-file-name ".ruby-lsp.yml" project-root))
         (config-content "# Ruby LSP configuration
# Disable RuboCop to use only ruby-lsp's built-in diagnostics
linters:
  rubocop: false

# Optional: Configure formatter
formatter: \"auto\"

# Optional: Configure other features
features:
  diagnostics: true
  formatting: true
"))
    (when (y-or-n-p (format "Create/update .ruby-lsp.yml in %s to disable RuboCop? " project-root))
      (write-region config-content nil config-file)
      (message "Created %s - Please restart LSP with M-x lsp-restart-workspace" config-file))))

;; Alternative: Set environment variable to disable RuboCop
(defun nh/ruby-lsp-env-disable-rubocop ()
  "Set environment variable to disable RuboCop in ruby-lsp."
  (interactive)
  (setenv "RUBY_LSP_USE_BUNDLER_COMPOSE" "false")
  (setenv "DISABLE_RUBOCOP" "true")
  (message "Set environment to disable RuboCop. Restart LSP with M-x lsp-restart-workspace"))


;; Configure compilation mode to handle ANSI color codes
(require 'ansi-color)
(defun nh/colorize-compilation-buffer ()
  "Colorize ANSI escape sequences in compilation buffer."
  (ansi-color-apply-on-region compilation-filter-start (point-max)))

(add-hook 'compilation-filter-hook 'nh/colorize-compilation-buffer)

;; Create a simple RSpec runner function
(defun nh/run-rspec-at-point ()
  "Run RSpec test at point using code lens."
  (interactive)
  (let* ((file-path buffer-file-name)
         (line-number (line-number-at-pos))
         (project-root (or (locate-dominating-file file-path "Gemfile")
                          (locate-dominating-file file-path ".git")
                          default-directory))
         (default-directory project-root)
         (relative-path (file-relative-name file-path project-root)))
    (compile (format "bundle exec rspec %s:%d" relative-path line-number))))

;; Override code lens mouse click handler
(with-eval-after-load 'lsp-mode
  ;; Fix the actual click handler for code lens
  (defun nh/lsp-code-lens--action-fix (orig-fun action)
    "Intercept code lens actions and fix RSpec commands."
    (if (and (derived-mode-p 'ruby-mode 'enh-ruby-mode)
             (string-match-p "_spec\\.rb\\'" (or buffer-file-name "")))
        ;; For Ruby spec files, check if it's a test action
        (let ((command (when (hash-table-p action)
                        (gethash "command" action))))
          (if (and command (hash-table-p command))
              (let ((cmd-string (gethash "command" command))
                    (args (gethash "arguments" command)))
                (message "Code lens command: %s" cmd-string)
                ;; If it looks like a test command, run our version
                (if (or (string-match-p "test" cmd-string)
                       (string-match-p "Test" cmd-string)
                       (string-match-p "spec" cmd-string))
                    (nh/run-rspec-at-point)
                  ;; Otherwise let it through
                  (funcall orig-fun action)))
            ;; Not a command we recognize
            (funcall orig-fun action)))
      ;; Not a Ruby spec file
      (funcall orig-fun action)))

  ;; Apply advice to the code lens action handler
  (advice-add 'lsp-code-lens--action :around #'nh/lsp-code-lens--action-fix))

;; Debug function to see what command is being sent
(defun nh/debug-lsp-command ()
  "Debug the last LSP command."
  (interactive)
  (message "Debug: Check *Messages* buffer for LSP command details"))

;; Add debugging to the command handler
(defun nh/ruby-lsp-execute-command-debug (command)
  "Debug version of the command handler."
  (message "LSP Command Debug:")
  (message "  Command: %s" command)
  (when (hash-table-p command)
    (message "  Command ID: %s" (gethash "command" command))
    (message "  Arguments: %s" (gethash "arguments" command))))

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
  (let ((default-directory (or (and (fboundp 'projectile-project-root)
                                   (projectile-project-root))
                               (locate-dominating-file default-directory "Gemfile")
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
                           (or (and (fboundp 'projectile-project-root)
                                   (projectile-project-root))
                               (locate-dominating-file default-directory "Gemfile")
                               default-directory)
                           search-term)))
          (goto-char (point-min))
          (view-mode 1))
        (switch-to-buffer-other-window buffer-name)))))

;; RSpec functions
(defun nh/rspec-run-current-file ()
  "Run RSpec on the current file."
  (interactive)
  (when (and buffer-file-name
             (string-match-p "_spec\\.rb\\'" buffer-file-name))
    (let* ((project-root (or (and (fboundp 'projectile-project-root)
                                 (projectile-project-root))
                            (locate-dominating-file default-directory "Gemfile")
                            default-directory))
           (default-directory project-root)
           (spec-file (file-relative-name buffer-file-name project-root)))
      (compile (format "bundle exec rspec %s" (shell-quote-argument spec-file))))))

(defun nh/rspec-run-at-point ()
  "Run RSpec at the current line."
  (interactive)
  (when (and buffer-file-name
             (string-match-p "_spec\\.rb\\'" buffer-file-name))
    (let* ((project-root (or (and (fboundp 'projectile-project-root)
                                 (projectile-project-root))
                            (locate-dominating-file default-directory "Gemfile")
                            default-directory))
           (default-directory project-root)
           (spec-file (file-relative-name buffer-file-name project-root))
           (line-number (line-number-at-pos)))
      (compile (format "bundle exec rspec %s:%d"
                      (shell-quote-argument spec-file)
                      line-number)))))

(defun nh/rspec-rerun-last ()
  "Rerun the last RSpec command."
  (interactive)
  (when (boundp 'compile-command)
    (compile compile-command)))

(defun nh/rspec-run-all ()
  "Run all RSpec tests."
  (interactive)
  (let* ((project-root (or (and (fboundp 'projectile-project-root)
                               (projectile-project-root))
                          (locate-dominating-file default-directory "Gemfile")
                          default-directory))
         (default-directory project-root))
    (compile "bundle exec rspec")))


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
