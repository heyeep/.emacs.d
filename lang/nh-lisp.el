;;; nh-lisp.el --- Common Lisp development configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Modern Common Lisp development configuration for Emacs 30.1
;; Focused on essential SLIME IDE features with proper syntax and indentation.

;;; Code:

;; Required dependencies
(require 'nh-env)

(defcustom nh/lisp-implementation-paths
  `((mac . ,(list "/opt/homebrew/bin/sbcl"    ;; Apple Silicon Homebrew
                  "/usr/local/bin/sbcl"       ;; Intel Homebrew
                  "/opt/local/bin/sbcl"))     ;; MacPorts
    (linux . ,(list "/usr/bin/sbcl"           ;; Ubuntu/Debian
                     "/usr/local/bin/sbcl"))  ;; Manual install
    (windows . ,(list "C:/sbcl/sbcl.exe"      ;; Common Windows location
                      "sbcl.exe")))           ;; PATH lookup
  "Paths to search for Common Lisp implementations by system type."
  :type '(alist :key-type symbol :value-type (repeat string))
  :group 'nh-lisp)

(defun nh/find-lisp-implementation ()
  "Find the first available Common Lisp implementation for this system."
  (let ((paths (alist-get nh-env/system nh/lisp-implementation-paths)))
    (or (cl-find-if #'executable-find paths)
        "sbcl")))  ;; Final fallback

;; Built-in Lisp mode enhancements for Common Lisp development
;; Provides syntax highlighting, indentation, and basic editing features
(use-package lisp-mode
  :ensure nil
  :config
  ;; Enhanced indentation for Common Lisp - uses proper CL indentation rules
  ;; instead of Emacs Lisp indentation which differs in some cases
  (setq lisp-indent-function #'common-lisp-indent-function)

  ;; Improved electric features for Emacs 30.1
  ;; Electric quote mode can interfere with Lisp symbols, so disable it
  ;; Electric pair mode helps with balanced parentheses
  (when (boundp 'electric-quote-mode)
    (add-hook 'lisp-mode-hook
              (lambda ()
                (electric-quote-local-mode -1)  ;; Disable smart quotes in Lisp
                (electric-pair-local-mode 1))))  ;; Enable smart parens

  :hook ((lisp-mode . (lambda ()
                        ;; Enhanced display settings for better parentheses visibility
                        (setq-local show-paren-delay 0)        ;; Instant paren highlighting
                        (setq-local show-paren-style 'expression) ;; Highlight entire expression

                        ;; Enhanced font-lock for Common Lisp specific features
                        (font-lock-add-keywords
                         nil
                         '(;; Display lambda as λ symbol for better readability
                           ("\\<lambda\\>" 0 (prog1 ()
                                               (compose-region (match-beginning 0)
                                                               (match-end 0) "λ")))
                           ;; Highlight CLOS (Common Lisp Object System) keywords
                           ("\\<\\(defclass\\|defgeneric\\|defmethod\\|defpackage\\)\\>"
                            1 font-lock-keyword-face)))))))

;; SLIME: Superior Lisp Interaction Mode for Emacs
;; The premier Common Lisp development environment for Emacs
;; Provides REPL, debugger, inspector, cross-references, and much more
;; https://github.com/slime/slime
(use-package slime
  :ensure t
  :init
  ;; Set up the Lisp implementation - automatically detected based on OS
  (setq inferior-lisp-program (nh/find-lisp-implementation))

  ;; Enhanced startup settings for better user experience
  (setq slime-startup-animation nil)           ;; Disable startup animation for faster loading
  (setq slime-kill-without-query-p t)          ;; Don't prompt when killing SLIME process
  (setq slime-description-autofocus t)         ;; Auto-focus help/description windows

  ;; Use UTF-8 encoding for proper Unicode support in REPL
  (setq slime-net-coding-system 'utf-8-unix)

  :config
  ;; Configure essential SLIME contribs (contributions/extensions)
  (setq slime-contribs '(slime-fancy           ;; Enhanced REPL with syntax highlighting
                         slime-indentation     ;; Proper Common Lisp indentation
                         slime-sbcl-exts       ;; SBCL-specific debugging extensions
                         slime-asdf))          ;; ASDF (build system) integration

  ;; Initialize SLIME with the selected contributions
  (slime-setup slime-contribs)

  ;; Enhanced completion settings for better productivity
  (setq slime-complete-symbol*-fancy t)        ;; Use fancy completion with descriptions
  (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol) ;; Fuzzy matching

  ;; REPL history settings for persistent command history
  (setq slime-repl-history-file "~/.slime-history")      ;; Save history to file
  (setq slime-repl-history-size 1000)                   ;; Keep 1000 commands in history
  (setq slime-repl-history-remove-duplicates t)         ;; Remove duplicate entries

  :bind (:map slime-mode-map
              ;; Evaluation commands - execute Lisp code interactively
              ("C-c e e" . slime-eval-last-expression)  ;; Eval expression before point
              ("C-c e r" . slime-eval-region)           ;; Eval selected region
              ("C-c e b" . slime-eval-buffer)           ;; Eval entire buffer
              ("C-c e f" . slime-eval-defun)            ;; Eval current function

              ;; Navigation commands - jump to definitions and back
              ("M-." . slime-edit-definition)           ;; Jump to definition
              ("M-," . slime-pop-find-definition-stack) ;; Return from definition

              ;; Documentation commands - get help on symbols
              ("C-c d d" . slime-describe-symbol)       ;; Describe symbol at point
              ("C-c d a" . slime-apropos)               ;; Search for symbols by name

              ;; Compilation commands - compile Lisp code
              ("C-c k" . slime-compile-defun)           ;; Compile current function
              ("C-c c" . slime-compile-file)            ;; Compile current file
              ("C-c l" . slime-load-file)               ;; Load file into Lisp image

              ;; REPL access - switch to interactive Lisp environment
              ("C-c C-z" . slime-repl)))

;; Geiser: Interactive development environment for Scheme
;; Provides REPL, evaluation, documentation, and debugging for Scheme dialects
;; Supports multiple Scheme implementations (Guile, Chicken, Racket, etc.)
;; https://github.com/jaor/geiser
(use-package geiser
  :ensure t

  :config
  ;; Configure supported Scheme implementations
  (setq geiser-active-implementations '(guile chicken)) ;; Enable Guile and Chicken Scheme
  (setq geiser-default-implementation 'guile))          ;; Use Guile as default

(defun +commonlisp-mode ()
  "Bootstrap Common Lisp mode - maintained for compatibility."
  (setq auto-mode-alist (rassq-delete-all #'+commonlisp-mode auto-mode-alist))
  (lisp-mode))

(provide 'nh-lisp)
;;; nh-lisp.el ends here

