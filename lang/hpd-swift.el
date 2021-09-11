(use-package swift-mode
  :ensure t
  :mode ("\\.swift\\'" . swift-mode)
  :config
  (setq swift-mode:parenthesized-expression-offset 4) ;; Match xcode.
  (setq swift-mode:multiline-statement-offset 4) ;; Match xcode.
  (define-key swift-repl-mode-map [(shift return)] 'evil-jump-forward))

(use-package flycheck-swiftlint
  :ensure t
  :config
  (with-eval-after-load 'flycheck
    (flycheck-swiftlint-setup)))

;; (use-package flycheck-swiftlint
;; ;;  :load-path "~/.emacs.d/fork/flycheck-swiftlint"
;;   :ensure nil
;;   :config
;;   (setq flycheck-swiftlint-should-run-swiftlint-function
;;         (lambda ()
;;           (and
;;            (flycheck-swiftlint-should-run-p)
;;            (not (string-match-p
;;                  "auth"
;;                  (flycheck-swiftlint--find-swiftlint-directory))))))
;;   (with-eval-after-load 'flycheck
;;     (flycheck-swiftlint-setup)))

;;;###autoload
(defun hpd-swift-mode ()
  "Bootstrap `hpd-swift'."
  (setq auto-mode-alist (rassq-delete-all #'j-swift-mode auto-mode-alist))
  (swift-mode))

(provide 'hpd-swift)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jn-swift.el ends here
