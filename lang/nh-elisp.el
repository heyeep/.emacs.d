;;; nh-elisp.el --- Enhanced Emacs Lisp development configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Modern Emacs Lisp development configuration for Emacs 30.1
;; Includes enhanced editing, debugging, navigation, and evaluation features.

;;; Code:

;; Required for bind-keys macro
(require 'bind-key)

;; ===== ENHANCED ELISP MODE CONFIGURATION =====

(use-package elisp-mode
  :ensure nil
  :config
  ;; Auto-recompile .elc files when .el files are saved
  (defun nh/recompile-elc-on-save ()
    "If there is a corresponding elc file, recompile after save."
    (when (and buffer-file-name
               (string-suffix-p ".el" buffer-file-name)
               (file-exists-p (byte-compile-dest-file buffer-file-name)))
      (byte-compile-file buffer-file-name)
      (message "Recompiled %s" (file-name-nondirectory buffer-file-name))))

  ;; Better evaluation and debugging settings
  (setq load-prefer-newer t)                        ;; Prefer newer .el over .elc
  (setq edebug-trace nil)                           ;; Don't trace by default
  (setq edebug-print-length 80)                     ;; Longer print length for debugging
  (setq eval-expression-print-length 50)            ;; More generous printing
  (setq eval-expression-print-level 10)             ;; Deeper nesting allowed
  
  ;; Enhanced debugger settings for Emacs 30.1
  (when (boundp 'debugger-stack-frame-as-list)
    (setq debugger-stack-frame-as-list t))
  
  ;; Add auto-recompile to emacs-lisp-mode
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (add-hook 'after-save-hook #'nh/recompile-elc-on-save nil t))))

;; ===== VISUAL FEEDBACK FOR EVALUATION =====

;; Visual feedback when evaluating expressions
(use-package eval-sexp-fu
  :ensure t
  :hook ((emacs-lisp-mode . eval-sexp-fu-flash-mode)
         (lisp-interaction-mode . eval-sexp-fu-flash-mode))
  :config
  (defun nh/eval-sexp-fu-set-face ()
    "Set `eval-sexp-fu' face to match current theme."
    (set-face-attribute 'eval-sexp-fu-flash nil
                        :background (face-attribute 'region :background)
                        :foreground (face-attribute 'default :foreground)
                        :weight 'bold
                        :underline t))
  
  (nh/eval-sexp-fu-set-face)
  (add-hook 'after-load-theme-hook #'nh/eval-sexp-fu-set-face))

;; ===== ENHANCED NAVIGATION =====

;; Enhanced navigation for Elisp symbols (like SLIME for Common Lisp)
(use-package elisp-slime-nav
  :ensure t
  :diminish elisp-slime-nav-mode
  :hook ((emacs-lisp-mode . elisp-slime-nav-mode)
         (lisp-interaction-mode . elisp-slime-nav-mode))
  :bind (:map emacs-lisp-mode-map
              ("C-c e d" . elisp-slime-nav-find-elisp-thing-at-point)
              ("C-c e h" . elisp-slime-nav-describe-elisp-thing-at-point))
  :config
  ;; Auto-focus help window after opening documentation
  (advice-add 'elisp-slime-nav-describe-elisp-thing-at-point
              :after (lambda (&rest _)
                       (when (get-buffer "*Help*")
                         (pop-to-buffer "*Help*")))))

;; Find references to Elisp symbols
(use-package elisp-refs
  :ensure t
  :bind (:map emacs-lisp-mode-map
              ("C-c e r f" . elisp-refs-function)
              ("C-c e r v" . elisp-refs-variable)
              ("C-c e r s" . elisp-refs-symbol)
              ("C-c e r m" . elisp-refs-macro)
              ("C-c e r S" . elisp-refs-special)))

;; ===== DEBUGGING ENHANCEMENTS =====

;; Enhanced debugging with edebug-x (additional edebug features)
(use-package edebug-x
  :ensure t
  :after edebug)

;; ===== CUSTOM EVALUATION FUNCTIONS =====

;; Better error reporting for eval
(defun nh/eval-last-sexp-with-error-display ()
  "Evaluate the last sexp and display errors clearly."
  (interactive)
  (condition-case err
      (eval-last-sexp nil)
    (error (message "Eval error: %s" (error-message-string err)))))

;; Evaluate and replace expression
(defun nh/eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (let ((value (eval (elisp--preceding-sexp))))
    (backward-kill-sexp)
    (insert (format "%S" value))))

;; Evaluate buffer with feedback
(defun nh/eval-buffer-with-feedback ()
  "Evaluate buffer and show result in minibuffer."
  (interactive)
  (condition-case err
      (progn
        (eval-buffer)
        (message "Buffer evaluated successfully"))
    (error (message "Buffer eval error: %s" (error-message-string err)))))

;; ===== ENHANCED KEYBINDINGS =====

;; Enhanced Elisp evaluation and debugging keybindings
(bind-keys :map emacs-lisp-mode-map
           ("C-c e e" . eval-last-sexp)
           ("C-c e E" . nh/eval-last-sexp-with-error-display)
           ("C-c e b" . nh/eval-buffer-with-feedback)
           ("C-c e r" . eval-region)
           ("C-c e R" . nh/eval-and-replace)
           ("C-c e f" . eval-defun)
           ("C-c e x" . edebug-defun)
           ("C-c e u" . edebug-remove-instrumentation)
           ("C-c e p" . pp-eval-last-sexp)
           ("C-c e P" . pp-eval-expression))

;; Additional useful bindings for lisp-interaction-mode
(bind-keys :map lisp-interaction-mode-map
           ("C-c e e" . eval-last-sexp)
           ("C-c e E" . nh/eval-last-sexp-with-error-display)
           ("C-c e b" . nh/eval-buffer-with-feedback)
           ("C-c e R" . nh/eval-and-replace))

;; ===== ELISP DEVELOPMENT HELPERS =====

(defun nh/elisp-find-library ()
  "Find and open an Elisp library file."
  (interactive)
  (find-library (completing-read "Library: " (mapcar #'car load-history))))

(defun nh/elisp-insert-header ()
  "Insert a proper Elisp file header."
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
  "Byte compile and load the current buffer."
  (interactive)
  (when (buffer-file-name)
    (let ((compiled-file (byte-compile-file (buffer-file-name))))
      (when compiled-file
        (load-file compiled-file)
        (message "Compiled and loaded %s" (file-name-nondirectory compiled-file))))))

;; Keybindings for development helpers
(bind-keys :map emacs-lisp-mode-map
           ("C-c e l" . nh/elisp-find-library)
           ("C-c e i" . nh/elisp-insert-header)
           ("C-c e c" . nh/elisp-byte-compile-and-load))

;; ===== ADDITIONAL ELISP MODE ENHANCEMENTS =====

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            ;; Enhanced indentation and formatting
            (setq-local indent-tabs-mode nil)
            (setq-local tab-width 2)
            
            ;; Show function signatures more aggressively
            (when (fboundp 'eldoc-mode)
              (eldoc-mode 1)
              (setq-local eldoc-idle-delay 0.2))
            
            ;; Enable outline minor mode for better navigation
            (outline-minor-mode 1)
            (setq-local outline-regexp ";;;\\(;* \\)")
            
            ;; Enhanced font-lock for development
            (font-lock-add-keywords
             nil
             '(("\\<\\(FIXME\\|TODO\\|BUG\\|HACK\\|NOTE\\):" 
                1 'font-lock-warning-face t)
               ("\\<\\(XXX\\|TEMP\\|KLUDGE\\):" 
                1 'font-lock-warning-face t)))
            
            ;; Highlight quoted symbols and functions
            (font-lock-add-keywords
             nil
             '(("'\\(\\sw\\|\\s_\\)+" . 'font-lock-constant-face)))))

;; Make lisp-interaction-mode more useful
(add-hook 'lisp-interaction-mode-hook
          (lambda ()
            (when (fboundp 'eldoc-mode)
              (eldoc-mode 1))))

(provide 'nh-elisp)

;;; nh-elisp.el ends here 