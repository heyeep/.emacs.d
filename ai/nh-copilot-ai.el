;;; nh-copilot-ai.el --- AI code completion and chat tools -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for AI-powered code completion and chat in Emacs.

;;; Code:

;; GitHub Copilot integration for Emacs
;; https://github.com/copilot-emacs/copilot.el
(use-package copilot
  :ensure t
  :hook (prog-mode . copilot-mode)
  :custom
  (copilot-idle-delay 0.2)
  (copilot-server-executable "/Users/hiep/.asdf/shims/copilot-language-server")
  (copilot-enable-predicates '(copilot--buffer-changed))
  (copilot-disable-predicates '(copilot--current-line-empty-p))
  :config
  ;; Add error handling for server crashes
  (defun nh/copilot-handle-server-error (err)
    "Handle Copilot server errors gracefully."
    (message "Copilot server error: %s" (error-message-string err))
    (when (and (eq (car err) 'jsonrpc-error)
               (string-match-p "Server died" (error-message-string err)))
      (message "Attempting to restart Copilot server...")
      (copilot--start-server)))

  (advice-add 'copilot--start-server :around
              (lambda (orig-fun &rest args)
                (condition-case err
                    (apply orig-fun args)
                  (error (nh/copilot-handle-server-error err)))))

  (advice-add 'copilot--infer-indentation-offset :around
              (lambda (orig-fn &rest args)
                (ignore-errors
                  (apply orig-fn args))))

  ;; Accept Copilot suggestion with TAB
  :bind (:map copilot-completion-map
              ("C-<return>" . 'copilot-accept-completion)
              ("C-<right>" . 'copilot-accept-completion-by-word))
  )

(setq copilot-indent-offset nil)
(defun copilot--infer-indentation-offset () nil)

(provide 'nh-copilot-ai)
;;; nh-copilot-ai.el ends here
