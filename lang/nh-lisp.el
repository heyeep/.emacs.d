;;; nh-lisp.el --- Common Lisp development configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Common Lisp editing with SLIME, plus Geiser for Scheme.

;;; Code:

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
        "sbcl")))

;; lisp-mode: Emacs's built-in mode for Common Lisp files.
(use-package lisp-mode
  :ensure nil
  :config
  ;; Common Lisp indents some forms differently from Emacs Lisp.
  (setq lisp-indent-function #'common-lisp-indent-function)

  ;; Smart quotes turn ' into a curly quote, which breaks quoted symbols.
  (when (boundp 'electric-quote-mode)
    (add-hook 'lisp-mode-hook
              (lambda ()
                (electric-quote-local-mode -1)
                (electric-pair-local-mode 1))))

  :hook ((lisp-mode . (lambda ()
                        (setq-local show-paren-delay 0)
                        (setq-local show-paren-style 'expression)

                        (font-lock-add-keywords
                         nil
                         '(;; Show lambda as λ.
                           ("\\<lambda\\>" 0 (prog1 ()
                                               (compose-region (match-beginning 0)
                                                               (match-end 0) "λ")))
                           ;; Highlight Common Lisp Object System (CLOS) definitions.
                           ("\\<\\(defclass\\|defgeneric\\|defmethod\\|defpackage\\)\\>"
                            1 font-lock-keyword-face)))))))

;; SLIME: REPL, debugger and inspector for Common Lisp.
;; GitHub: https://github.com/slime/slime
(use-package slime
  :ensure t
  :init
  (setq inferior-lisp-program (nh/find-lisp-implementation))

  (setq slime-startup-animation nil)
  (setq slime-kill-without-query-p t)
  (setq slime-description-autofocus t)

  (setq slime-net-coding-system 'utf-8-unix)

  :config
  (setq slime-contribs '(slime-fancy
                         slime-indentation
                         slime-sbcl-exts
                         slime-asdf))

  (slime-setup slime-contribs)

  (setq slime-complete-symbol*-fancy t)
  (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)

  (setq slime-repl-history-file "~/.slime-history")
  (setq slime-repl-history-size 1000)
  (setq slime-repl-history-remove-duplicates t)

  :bind (:map slime-mode-map
              ("C-c e e" . slime-eval-last-expression)
              ("C-c e r" . slime-eval-region)
              ("C-c e b" . slime-eval-buffer)
              ("C-c e f" . slime-eval-defun)

              ("M-." . slime-edit-definition)
              ("M-," . slime-pop-find-definition-stack)

              ("C-c D d" . slime-describe-symbol)
              ("C-c D a" . slime-apropos)

              ("C-c k" . slime-compile-defun)
              ("C-c c" . slime-compile-file)
              ("C-c l" . slime-load-file)

              ("C-c C-z" . slime-repl)))

;; Geiser: REPL and docs for Scheme, using Guile by default.
;; GitHub: https://github.com/jaor/geiser
(use-package geiser
  :ensure t

  :config
  (setq geiser-active-implementations '(guile chicken))
  (setq geiser-default-implementation 'guile))

(defun +commonlisp-mode ()
  "Bootstrap Common Lisp mode - maintained for compatibility."
  (setq auto-mode-alist (rassq-delete-all #'+commonlisp-mode auto-mode-alist))
  (lisp-mode))

(provide 'nh-lisp)
;;; nh-lisp.el ends here

