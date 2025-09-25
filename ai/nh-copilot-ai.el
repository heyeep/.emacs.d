;;; nh-copilot-ai.el --- AI code completion and chat tools -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for AI-powered code completion and chat in Emacs.

;;; Code:

;; GitHub Copilot integration for Emacs
;; https://github.com/copilot-emacs/copilot.el
;; Only load copilot in GUI mode due to terminal compatibility issues
;; (when (display-graphic-p)
;;   (use-package copilot
;;       :ensure t
;;       :diminish copilot-mode
;;       :hook (prog-mode . copilot-mode)
;;       :custom
;;       (copilot-idle-delay 0.2)
;;       (copilot-server-executable "/Users/hiep/.asdf/installs/nodejs/18.19.0/bin/copilot-language-server")
;;       (copilot-enable-predicates '(copilot--buffer-changed))
;;       (copilot-disable-predicates '(copilot--current-line-empty-p))
;;       :config
;;       ;; Track restart attempts to prevent loops
;;       (defvar nh/copilot-restart-count 0)
;;       (defvar nh/copilot-restart-timer nil)

;;       ;; Reset restart count after successful operation
;;       (run-with-timer 60 60 (lambda () (setq nh/copilot-restart-count 0)))

;;       ;; Add error handling for server crashes with restart limiting
;;       (defun nh/copilot-handle-server-error (err)
;;         "Handle Copilot server errors gracefully."
;;         (when (< nh/copilot-restart-count 3)
;;           (setq nh/copilot-restart-count (1+ nh/copilot-restart-count))
;;           (message "Copilot server error (attempt %d/3): %s"
;;                    nh/copilot-restart-count
;;                    (error-message-string err))
;;           (when (and (eq (car err) 'jsonrpc-error)
;;                      (string-match-p "Server died\\|exited abnormally" (error-message-string err)))
;;             (when nh/copilot-restart-timer
;;               (cancel-timer nh/copilot-restart-timer))
;;             (setq nh/copilot-restart-timer
;;                   (run-with-timer 2 nil
;;                                   (lambda ()
;;                                     (message "Attempting to restart Copilot server...")
;;                                     (copilot--start-server))))))
;;         (when (>= nh/copilot-restart-count 3)
;;           (message "Copilot server failed too many times. Run M-x copilot-mode to try again.")
;;           (copilot-mode -1)))

;;       (advice-add 'copilot--start-server :around
;;                   (lambda (orig-fun &rest args)
;;                     (condition-case err
;;                         (apply orig-fun args)
;;                       (error (nh/copilot-handle-server-error err)))))

;;       ;; Prevent rapid connection attempts
;;       (setq copilot-reconnect-delay 5.0)

;;       ;; Fix for terminal mode - disable network process in terminal
;;       (when (not (display-graphic-p))
;;         ;; Use pipes instead of network processes in terminal
;;         (setq copilot-use-native-json-parsing nil)
;;         ;; Ensure we're using the right executable
;;         ;; (setq copilot-node-executable "/Users/hiep/.asdf/installs/nodejs/22.16.0/bin/node")
;;         )

;;       (advice-add 'copilot--infer-indentation-offset :around
;;                   (lambda (orig-fn &rest args)
;;                     (ignore-errors
;;                       (apply orig-fn args))))

;;       ;; Accept Copilot suggestion with TAB
;;       :bind (:map copilot-completion-map
;;                   ("C-<return>" . 'copilot-accept-completion)
;;                   ("C-<right>" . 'copilot-accept-completion-by-word))
;;       ))
                                        ; End of use-package and when

;; Only set these in GUI mode
(when (display-graphic-p)
  (setq copilot-indent-offset nil)
  (defun copilot--infer-indentation-offset () nil))

(provide 'nh-copilot-ai)
;;; nh-copilot-ai.el ends here
