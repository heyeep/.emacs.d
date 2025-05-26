;; (use-package swift-mode
;;   :ensure t
;;   :mode ("\\.swift\\'" . swift-mode)
;;   :config
;;   (setq swift-mode:parenthesized-expression-offset 4) ;; Match xcode.
;;   (setq swift-mode:multiline-statement-offset 4) ;; Match xcode.
;;   (define-key swift-repl-mode-map [(shift return)] 'evil-jump-forward))

(provide 'nh-swift)
