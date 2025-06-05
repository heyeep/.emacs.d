;;; nh-python.el --- Python development configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Modern Python development configuration for Emacs 30.1
;; Includes intelligent completion, debugging, and interactive development features.

;;; Code:

(require 'nh-env)

;; Python Mode: Built-in Python mode enhancements
;; Provides syntax highlighting, indentation, and core Python editing features
;; with enhanced shell integration and automatic interpreter detection.
(use-package python
  :ensure nil
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :init
  ;; Enhanced Python shell setup with automatic interpreter detection
  (defun nh/setup-python-interpreter ()
    "Set up Python interpreter based on available versions and OS."
    (cond
     ;; On macOS, prefer python3 but fall back to python2 if needed
     (nh-env/is-mac
      (cond
       ((executable-find "python3") (setq python-shell-interpreter "python3"))
       ((executable-find "python2") (setq python-shell-interpreter "python2"))
       (t (setq python-shell-interpreter "python"))))
     ;; On other systems, prefer python3
     (t
      (cond
       ((executable-find "python3") (setq python-shell-interpreter "python3"))
       (t (setq python-shell-interpreter "python"))))))

  ;; Launch Python shell in background for interactive development
  (defun nh/setup-inferior-python ()
    "Launch Python shell in background for immediate availability."
    (nh/setup-python-interpreter)
    (unless (python-shell-get-buffer)
      (save-selected-window
        (run-python (python-shell-calculate-command) nil nil))))

  :config
  ;; Enhanced Python development settings
  (setq python-indent-offset 4                    ;; Standard Python indentation
        python-indent-guess-indent-offset t       ;; Auto-detect indentation
        python-shell-completion-native-enable t   ;; Use native completion
        python-shell-prompt-detect-enabled t      ;; Auto-detect prompts
        python-shell-prompt-detect-failure-warning nil) ;; Reduce noise

  ;; Fix for native completion issues
  ;; Addresses completion problems in some Python environments
  (defun python-shell-completion-native-try ()
    "Return non-nil if can trigger native completion."
    (let ((python-shell-completion-native-enable t)
          (python-shell-completion-native-output-timeout
           python-shell-completion-native-try-output-timeout))
      (python-shell-completion-native-get-completions
       (get-buffer-process (current-buffer))
       nil "_")))

  :hook ((python-mode . (lambda ()
                          ;; Enhanced display and editing settings
                          (setq-local tab-width 4
                                      indent-tabs-mode nil
                                      fill-column 88)  ;; Black formatter standard

                          ;; Enable useful minor modes
                          (when (fboundp 'eldoc-mode)
                            (eldoc-mode 1))
                          (when (fboundp 'electric-pair-local-mode)
                            (electric-pair-local-mode 1))

                          ;; Set up Python shell
                          (nh/setup-inferior-python)

                          ;; Enhanced font-lock for Python development
                          (font-lock-add-keywords
                           nil
                           '(("\\<\\(TODO\\|FIXME\\|BUG\\|HACK\\|NOTE\\|XXX\\):"
                              1 'font-lock-warning-face t)
                             ;; Highlight common Python decorators
                             ("@\\(\\sw\\|\\s_\\)+" . 'font-lock-preprocessor-face))))))

  :bind (:map python-mode-map
              ;; Evaluation commands - execute Python code interactively
              ("C-c e e" . python-shell-send-statement)     ;; Send current statement
              ("C-c e b" . python-shell-send-buffer)        ;; Send entire buffer
              ("C-c e f" . python-shell-send-defun)         ;; Send current function
              ("C-c e s" . python-shell-send-string)        ;; Send custom string
              ("C-c e R" . python-shell-send-region)        ;; Send selected region

              ;; Shell and REPL management
              ("C-c C-z" . python-shell-switch-to-shell)    ;; Switch to Python shell
              ("C-c C-c" . python-shell-send-buffer)        ;; Quick buffer execution

              ;; Navigation and documentation
              ("C-c d d" . python-describe-at-point)        ;; Describe symbol at point

              ;; Debugging
              ("C-c d b" . pdb)                             ;; Start Python debugger
              ("C-c d t" . python-shell-send-file)))       ;; Send file to shell

;; Anaconda Mode: Advanced Python development environment
;; Provides intelligent code completion, navigation, and documentation using
;; Jedi for static analysis with IDE-like features for Python development.
;; GitHub: https://github.com/pythonic-emacs/anaconda-mode
(use-package anaconda-mode
  :ensure t
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode))   ;; Enable eldoc integration
  :config
  ;; Configure anaconda server installation directory
  (setq anaconda-mode-installation-directory
        (expand-file-name (format "anaconda-mode/%s" emacs-major-version)
                          user-emacs-directory))

  ;; Enhanced server settings for better performance
  (setq anaconda-mode-eldoc-as-single-line t     ;; Cleaner eldoc display
        anaconda-mode-server-command "python")   ;; Use system Python

  :bind (:map anaconda-mode-map
              ;; Navigation commands - jump to definitions and references
              ("M-." . anaconda-mode-find-definitions)      ;; Go to definition
              ("M-," . anaconda-mode-go-back)               ;; Return from definition
              ("C-c f r" . anaconda-mode-find-references)   ;; Find references
              ("C-c f a" . anaconda-mode-find-assignments)  ;; Find assignments
              ("C-c f f" . anaconda-mode-find-file)         ;; Find file

              ;; Documentation commands
              ("C-c d s" . anaconda-mode-show-doc)          ;; Show documentation

              ;; Code completion
              ("C-c c c" . anaconda-mode-complete)))

;; Python debugging utilities for enhanced development workflow
(defun nh/python-insert-breakpoint ()
  "Insert a Python breakpoint at current line."
  (interactive)
  (beginning-of-line)
  (open-line 1)
  (insert "import pdb; pdb.set_trace()"))

(defun nh/python-insert-ipdb-breakpoint ()
  "Insert an IPython debugger breakpoint at current line."
  (interactive)
  (beginning-of-line)
  (open-line 1)
  (insert "import ipdb; ipdb.set_trace()"))

(defun nh/python-remove-breakpoints ()
  "Remove all pdb breakpoints from current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^.*import i?pdb; i?pdb.set_trace().*$" nil t)
      (beginning-of-line)
      (kill-whole-line))))

(defun nh/python-run-file ()
  "Run current Python file in shell."
  (interactive)
  (when buffer-file-name
    (python-shell-send-file buffer-file-name)
    (message "Executed %s" (file-name-nondirectory buffer-file-name))))

;; Additional Python development keybindings
(with-eval-after-load 'python
  (define-key python-mode-map (kbd "C-c d p") #'nh/python-insert-breakpoint)
  (define-key python-mode-map (kbd "C-c d i") #'nh/python-insert-ipdb-breakpoint)
  (define-key python-mode-map (kbd "C-c d r") #'nh/python-remove-breakpoints)
  (define-key python-mode-map (kbd "C-c r r") #'nh/python-run-file))

;; Legacy compatibility function
(defun +python-mode ()
  "Bootstrap Python mode - maintained for compatibility."
  (setq auto-mode-alist (rassq-delete-all #'+python-mode auto-mode-alist))
  (python-mode))

(provide 'nh-python)
;;; nh-python.el ends here
