;;; nh-elisp.el --- elisp -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'bind-key)

;; elisp-mode: Emacs Lisp editing, evaluation and debugging settings.
(use-package elisp-mode
  :ensure nil
  :config
  (setq load-prefer-newer t
        edebug-trace nil
        edebug-print-length 80
        eval-expression-print-length 50
        eval-expression-print-level 10)

  ;; Show backtrace frames as Lisp lists, which are easier to read.
  (when (boundp 'debugger-stack-frame-as-list)
    (setq debugger-stack-frame-as-list t))

  ;; Keep an existing .elc in step with its .el, so Emacs never loads stale code.
  (defun nh/recompile-elc-on-save ()
    "If there is a corresponding elc file, recompile after save."
    (when (and buffer-file-name
               (string-suffix-p ".el" buffer-file-name)
               (file-exists-p (byte-compile-dest-file buffer-file-name)))
      (byte-compile-file buffer-file-name)
      (message "Recompiled %s" (file-name-nondirectory buffer-file-name))))

  :hook ((emacs-lisp-mode . (lambda ()
                              (setq-local indent-tabs-mode nil
                                          tab-width 2)

                              (when (fboundp 'eldoc-mode)
                                (eldoc-mode 1)
                                (setq-local eldoc-idle-delay 0.2))

                              ;; Fold the file by its ;;; section headings.
                              (outline-minor-mode 1)
                              (setq-local outline-regexp ";;;\\(;* \\)")

                              ;; Highlight TODO-style tags and quoted symbols.
                              (font-lock-add-keywords
                               nil
                               '(("\\<\\(FIXME\\|TODO\\|BUG\\|HACK\\|NOTE\\|XXX\\|TEMP\\|KLUDGE\\):"
                                  1 'font-lock-warning-face t)
                                 ("'\\(\\sw\\|\\s_\\)+" . 'font-lock-constant-face)))

                              (add-hook 'after-save-hook #'nh/recompile-elc-on-save nil t)))

         (lisp-interaction-mode . (lambda ()
                                    (when (fboundp 'eldoc-mode)
                                      (eldoc-mode 1)))))
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

;; eval-sexp-fu: flashes the expression you just evaluated.
;; GitHub: https://github.com/hchbaw/eval-sexp-fu.el
(use-package eval-sexp-fu
  :ensure t
  :hook ((emacs-lisp-mode . eval-sexp-fu-flash-mode)
         (lisp-interaction-mode . eval-sexp-fu-flash-mode))
  :config
  ;; Recolor the flash after each theme change so it stays visible.
  (defun nh/eval-sexp-fu-set-face ()
    "Set `eval-sexp-fu' face to match current theme."
    (set-face-attribute 'eval-sexp-fu-flash nil
                        :background (face-attribute 'region :background)
                        :foreground (face-attribute 'default :foreground)
                        :weight 'bold
                        :underline t))

  (nh/eval-sexp-fu-set-face)
  (add-hook 'after-load-theme-hook #'nh/eval-sexp-fu-set-face))

;; elisp-slime-nav: jump to the definition or docs of the symbol at point.
;; GitHub: https://github.com/purcell/elisp-slime-nav
(use-package elisp-slime-nav
  :ensure t
  :diminish elisp-slime-nav-mode
  :hook ((emacs-lisp-mode . elisp-slime-nav-mode)
         (lisp-interaction-mode . elisp-slime-nav-mode))
  :bind (:map emacs-lisp-mode-map
              ("C-c e d" . elisp-slime-nav-find-elisp-thing-at-point)
              ("C-c e h" . elisp-slime-nav-describe-elisp-thing-at-point))
  :config
  ;; Move focus to the help window so you can read and close it at once.
  (advice-add 'elisp-slime-nav-describe-elisp-thing-at-point
              :after (lambda (&rest _)
                       (when (get-buffer "*Help*")
                         (pop-to-buffer "*Help*")))))

;; elisp-refs: find every use of a function, variable or macro.
;; GitHub: https://github.com/Wilfred/elisp-refs
(use-package elisp-refs
  :ensure t
  :bind (:map emacs-lisp-mode-map
              ("C-c F f" . elisp-refs-function)
              ("C-c F v" . elisp-refs-variable)
              ("C-c F s" . elisp-refs-symbol)
              ("C-c F m" . elisp-refs-macro)
              ("C-c F S" . elisp-refs-special)))

;; edebug-x: extra breakpoint and display commands for edebug.
;; GitHub: https://github.com/ScottyB/edebug-x
(use-package edebug-x
  :ensure t
  :after edebug)

(defun nh/eval-last-sexp-with-error-display ()
  "Evaluate the last sexp and display errors clearly in minibuffer with line number."
  (interactive)
  (condition-case err
      (eval-last-sexp nil)
    (error (message "Error at line %d: %s"
                    (line-number-at-pos)
                    (error-message-string err)))))

(defun nh/eval-and-replace ()
  "Replace the preceding sexp with its evaluated value."
  (interactive)
  (let ((value (eval (elisp--preceding-sexp))))
    (backward-kill-sexp)
    (insert (format "%S" value))))

(defun nh/eval-buffer-with-feedback ()
  "Evaluate entire buffer and show the result or error in the minibuffer."
  (interactive)
  (condition-case err
      (progn
        (eval-buffer)
        (message "Buffer evaluated successfully"))
    (error
     (message "Buffer eval error: %s" (error-message-string err)))))

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
    (let ((source-file (buffer-file-name))
          (compiled-result (byte-compile-file (buffer-file-name))))
      (when compiled-result
        (let ((compiled-file (byte-compile-dest-file source-file)))
          (if (file-exists-p compiled-file)
              (progn
                (load-file compiled-file)
                (message "Compiled and loaded %s" (file-name-nondirectory compiled-file)))
            (progn
              (load-file source-file)
              (message "Compilation succeeded, loaded source %s" (file-name-nondirectory source-file)))))))))

(provide 'nh-elisp)
;;; nh-elisp.el ends here
