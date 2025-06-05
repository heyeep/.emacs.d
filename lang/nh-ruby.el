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

;; Setup ruby-lsp as the primary LSP client for Ruby
(defun nh/setup-ruby-lsp ()
  "Setup ruby-lsp as the primary LSP client for Ruby files."
  (when (or (derived-mode-p 'ruby-mode) (derived-mode-p 'enh-ruby-mode))
    ;; Disable other Ruby language servers
    (setq-local lsp-disabled-clients '(rubocop-ls sorbet-ls typeprof-ls steep-ls ruby-syntax-tree-ls semgrep-ls))
    ;; Enable ruby-lsp
    (setq-local lsp-enabled-clients '(ruby-lsp-ls))))

;; Ruby Mode: Major mode for editing Ruby files (built-in)
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
         (ruby-mode . nh/setup-ruby-lsp)
         (ruby-mode . lsp-deferred)
         (ruby-mode . flycheck-mode))
  :bind (:map ruby-mode-map
              ("C-c r u c" . nh/ruby-rubocop-check-current-file)
              ("C-c r u a" . nh/ruby-rubocop-autocorrect-current-file)
              ("C-c r t r" . nh/rails-routes)
              ("C-c r t d" . nh/rails-dbconsole)
              ("C-c r t g" . nh/rails-routes-grep))
  :config
  ;; Do not insert encoding magic comment in new Ruby files
  (setq ruby-insert-encoding-magic-comment nil)

  ;; Better alignment for Ruby method calls
  (setq ruby-align-to-stmt-keywords '(begin if unless while until case for def class module))
  (setq ruby-align-chained-calls t))

;; Enhanced Ruby Mode: More features for Ruby editing
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
         (enh-ruby-mode . nh/setup-ruby-lsp)
         (enh-ruby-mode . lsp-deferred)
         (enh-ruby-mode . flycheck-mode))
  :bind (:map enh-ruby-mode-map
              ("C-c r u c" . nh/ruby-rubocop-check-current-file)
              ("C-c r u a" . nh/ruby-rubocop-autocorrect-current-file)
              ("C-c r t r" . nh/rails-routes)
              ("C-c r t d" . nh/rails-dbconsole)
              ("C-c r t g" . nh/rails-routes-grep))
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

;; Inf-Ruby: Interactive Ruby REPL
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
(use-package robe
  :ensure t
  :hook ((ruby-mode enh-ruby-mode) . robe-mode)
  :config
  ;; Add robe completion to corfu
  (with-eval-after-load 'cape
    (add-hook 'ruby-mode-hook
              (lambda ()
                (add-to-list 'completion-at-point-functions #'robe-complete-at-point)))
    (add-hook 'enh-ruby-mode-hook
              (lambda ()
                (add-to-list 'completion-at-point-functions #'robe-complete-at-point))))

  :bind (:map robe-mode-map
              ("C-c r d" . robe-doc)
              ("C-c r j" . robe-jump)
              ("C-c r J" . robe-jump-to-module)))

;; Projectile Rails: Rails-specific project navigation
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

;;;###autoload
(defun nh/restart-lsp-ruby ()
  "Restart LSP and ensure ruby-lsp is used for Ruby files."
  (interactive)
  (when (bound-and-true-p lsp-mode)
    (if (fboundp 'lsp-workspace-shutdown)
        (lsp-workspace-shutdown (lsp-find-workspace))
      (when (fboundp 'lsp-disconnect)
        (lsp-disconnect)))
    (when (fboundp 'lsp-workspace-restart)
      (lsp-workspace-restart (lsp-find-workspace))))
  ;; Ensure our Ruby-specific settings are applied
  (when (or (derived-mode-p 'ruby-mode) (derived-mode-p 'enh-ruby-mode))
    (nh/setup-ruby-lsp)
    (lsp-deferred))
  (message "LSP restarted - ruby-lsp should now be active for Ruby files"))

(provide 'nh-ruby)

;;; nh-ruby.el ends here
