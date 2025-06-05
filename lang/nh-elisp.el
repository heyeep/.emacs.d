;;; nh-elisp.el --- Enhanced Emacs Lisp development configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Modern Emacs Lisp development configuration for Emacs 30.1
;; Includes enhanced editing, debugging, navigation, and evaluation features.

;;; Code:

(require 'bind-key)

;; Built-in Emacs Lisp mode enhancements
;; Provides syntax highlighting, indentation, and core editing features
(use-package elisp-mode
  :ensure nil
  :config
  ;; Enhanced evaluation and debugging settings for better development experience
  (setq load-prefer-newer t                        ;; Prefer newer .el over .elc files
        edebug-trace nil                           ;; Don't trace by default
        edebug-print-length 80                     ;; Longer print length for debugging
        eval-expression-print-length 50            ;; More generous printing in eval
        eval-expression-print-level 10)            ;; Allow deeper nesting in eval output

  ;; Enhanced debugger settings for Emacs 30.1
  (when (boundp 'debugger-stack-frame-as-list)
    (setq debugger-stack-frame-as-list t))

  ;; Auto-recompile .elc files when .el files are saved for faster loading
  (defun nh/recompile-elc-on-save ()
    "If there is a corresponding elc file, recompile after save."
    (when (and buffer-file-name
               (string-suffix-p ".el" buffer-file-name)
               (file-exists-p (byte-compile-dest-file buffer-file-name)))
      (byte-compile-file buffer-file-name)
      (message "Recompiled %s" (file-name-nondirectory buffer-file-name))))

  :hook ((emacs-lisp-mode . (lambda ()
                              ;; Enhanced indentation and formatting
                              (setq-local indent-tabs-mode nil
                                          tab-width 2)

                              ;; Enhanced function signature display
                              (when (fboundp 'eldoc-mode)
                                (eldoc-mode 1)
                                (setq-local eldoc-idle-delay 0.2))

                              ;; Enable outline minor mode for better code navigation
                              (outline-minor-mode 1)
                              (setq-local outline-regexp ";;;\\(;* \\)")

                              ;; Enhanced font-lock for development keywords
                              (font-lock-add-keywords
                               nil
                               '(("\\<\\(FIXME\\|TODO\\|BUG\\|HACK\\|NOTE\\|XXX\\|TEMP\\|KLUDGE\\):"
                                  1 'font-lock-warning-face t)
                                 ("'\\(\\sw\\|\\s_\\)+" . 'font-lock-constant-face)))

                              ;; Auto-recompile setup
                              (add-hook 'after-save-hook #'nh/recompile-elc-on-save nil t)))

         (lisp-interaction-mode . (lambda ()
                                    ;; Enable eldoc in scratch buffer for function signatures
                                    (when (fboundp 'eldoc-mode)
                                      (eldoc-mode 1)))))

  ;; Enhanced evaluation keybindings with better error handling
  :bind (:map emacs-lisp-mode-map
              ("C-c e e" . eval-last-sexp)
              ("C-c e E" . nh/eval-last-sexp-with-error-display)
              ("C-c e b" . nh/eval-buffer-with-feedback)
              ("C-c e r" . eval-region)
              ("C-c e R" . nh/eval-and-replace)
              ("C-c e f" . eval-defun)
              ("C-c e x" . edebug-defun)
              ("C-c e u" . edebug-remove-instrumentation)
              ("C-c e p" . pp-eval-last-sexp)
              ("C-c e P" . pp-eval-expression)
              ("C-c e l" . nh/elisp-find-library)
              ("C-c e i" . nh/elisp-insert-header)
              ("C-c e c" . nh/elisp-byte-compile-and-load)
              :map lisp-interaction-mode-map
              ("C-c e e" . eval-last-sexp)
              ("C-c e E" . nh/eval-last-sexp-with-error-display)
              ("C-c e b" . nh/eval-buffer-with-feedback)
              ("C-c e R" . nh/eval-and-replace)))

;; Eval Sexp Fu: Visual feedback when evaluating expressions
;; Provides visual highlighting when evaluating Lisp expressions
;; https://github.com/hchbaw/eval-sexp-fu.el
(use-package eval-sexp-fu
  :ensure t
  :hook ((emacs-lisp-mode . eval-sexp-fu-flash-mode)
         (lisp-interaction-mode . eval-sexp-fu-flash-mode))
  :config
  ;; Automatically adjust highlighting colors to match current theme
  (defun nh/eval-sexp-fu-set-face ()
    "Set `eval-sexp-fu' face to match current theme."
    (set-face-attribute 'eval-sexp-fu-flash nil
                        :background (face-attribute 'region :background)
                        :foreground (face-attribute 'default :foreground)
                        :weight 'bold
                        :underline t))

  (nh/eval-sexp-fu-set-face)
  (add-hook 'after-load-theme-hook #'nh/eval-sexp-fu-set-face))

;; Elisp Slime Nav: Enhanced navigation for Elisp symbols
;; Provides SLIME-like navigation features for Emacs Lisp development
;; Jump to definitions and get documentation for symbols
;; https://github.com/purcell/elisp-slime-nav
(use-package elisp-slime-nav
  :ensure t
  :diminish elisp-slime-nav-mode
  :hook ((emacs-lisp-mode . elisp-slime-nav-mode)
         (lisp-interaction-mode . elisp-slime-nav-mode))
  :bind (:map emacs-lisp-mode-map
              ("C-c e d" . elisp-slime-nav-find-elisp-thing-at-point)
              ("C-c e h" . elisp-slime-nav-describe-elisp-thing-at-point))
  :config
  ;; Auto-focus help window after opening documentation for better UX
  (advice-add 'elisp-slime-nav-describe-elisp-thing-at-point
              :after (lambda (&rest _)
                       (when (get-buffer "*Help*")
                         (pop-to-buffer "*Help*")))))

;; Elisp Refs: Find references to Elisp symbols
;; Search for references to functions, variables, and other symbols
;; Useful for understanding code dependencies and usage
;; https://github.com/Wilfred/elisp-refs
(use-package elisp-refs
  :ensure t
  :bind (:map emacs-lisp-mode-map
              ("C-c e r f" . elisp-refs-function)    ;; Find function references
              ("C-c e r v" . elisp-refs-variable)    ;; Find variable references
              ("C-c e r s" . elisp-refs-symbol)      ;; Find symbol references
              ("C-c e r m" . elisp-refs-macro)       ;; Find macro references
              ("C-c e r S" . elisp-refs-special)))   ;; Find special form references

;; Edebug X: Enhanced debugging features for edebug
;; Provides additional features and improvements for Emacs's built-in debugger
;; https://github.com/ScottyB/edebug-x
(use-package edebug-x
  :ensure t
  :after edebug)

;; Custom evaluation functions with enhanced error handling and feedback
(defun nh/eval-last-sexp-with-error-display ()
  "Evaluate the last sexp and display errors clearly in minibuffer."
  (interactive)
  (condition-case err
      (eval-last-sexp nil)
    (error (message "Eval error: %s" (error-message-string err)))))

(defun nh/eval-and-replace ()
  "Replace the preceding sexp with its evaluated value."
  (interactive)
  (let ((value (eval (elisp--preceding-sexp))))
    (backward-kill-sexp)
    (insert (format "%S" value))))

(defun nh/eval-buffer-with-feedback ()
  "Evaluate entire buffer and show result in minibuffer."
  (interactive)
  (condition-case err
      (progn
        (eval-buffer)
        (message "Buffer evaluated successfully"))
    (error (message "Buffer eval error: %s" (error-message-string err)))))

;; Development utility functions for enhanced productivity
(defun nh/elisp-find-library ()
  "Find and open an Elisp library file using completion."
  (interactive)
  (find-library (completing-read "Library: " (mapcar #'car load-history))))

(defun nh/elisp-insert-header ()
  "Insert a proper Elisp file header with standard format."
  (interactive)
  (let ((filename (file-name-nondirectory (buffer-file-name))))
    (save-excursion
      (goto-char (point-min))
      (insert (format ";;; %s --- DESCRIPTION -*- lexical-binding: t; -*-\n\n" filename))
      (insert ";;; Commentary:\n")
      (insert ";; COMMENTARY\n\n")
      (insert ";;; Code:\n\n")
      (goto-char (point-max))
      (insert (format "\n(provide '%s)\n\n" (file-name-sans-extension filename)))
      (insert (format ";;; %s ends here\n" filename)))))

(defun nh/elisp-byte-compile-and-load ()
  "Byte compile and load the current buffer for testing."
  (interactive)
  (when (buffer-file-name)
    (let ((compiled-file (byte-compile-file (buffer-file-name))))
      (when compiled-file
        (load-file compiled-file)
        (message "Compiled and loaded %s" (file-name-nondirectory compiled-file))))))

(provide 'nh-elisp)
;;; nh-elisp.el ends here
