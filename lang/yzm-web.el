;;;; -*- lexical-binding: t; -*-

(use-package web-mode
  :ensure t
  :mode
  ("\\.phtml\\'" . web-mode)
  ("\\.tpl\\.php\\'" . web-mode)
  ("\\.blade\\.php\\'" . web-mode)
  ("/\\(views\\|html\\|theme\\|templates\\)/.*\\.php\\'" . web-mode)
  ("\\.[agj]sp\\'" . web-mode)
  ("\\.as[cp]x\\'" . web-mode)
  ("\\.erb\\'" . web-mode)
  ("\\.mustache\\'" . web-mode)
  ("\\.djhtml\\'" . web-mode)
  ("\\.jsp\\'" . web-mode)
  ("\\.eex\\'" . web-mode)
  :init
  (add-hook 'web-mode-hook
            (lambda ()
              (when t
                (setq-local web-mode-markup-indent-offset 2)
                (setq-local web-mode-css-indent-offset 2)
                (setq-local web-mode-code-indent-offset 2)))))

;; colors for various 'color codes' aka hex strings
(use-package rainbow-mode
  :ensure t
  :commands (rainbow-mode)
  :init
  (add-hook 'css-mode-hook
            (lambda ()
              ;; This 2 spaces check could go in a css mode package.
              ;; Adding it here for now out of laziness.
              (when t
                (setq css-indent-offset 2))
              (rainbow-mode)))
  :diminish rainbow-mode
  :config
  (rainbow-mode 1))

(use-package js2-mode
  :ensure t
  :mode
  ("\\.js\\'" . js2-mode)
  ;; ("\\.jsx?\\'" . js2-jsx-mode)
  :interpreter
  ("node" . js2-mode)
  ;; ("node" . js2-jsx-mode)
  :config
  (setq js2-highlight-level 3)
  ;; (use-package xref-js2) Look into this for Emacs 25.

  ;; https://www.reddit.com/r/emacs/comments/4xiaym/packages_for_javascript/
  ;; js2-mode
  ;; js2-refactor for refactorings
  ;; xref-js2 for navigation to references/definitions
  ;; jade for a REPL, inspector & stepping debugger
  ;; company-tern for autocompletion

  (add-hook 'js2-mode-hook (lambda ()
                             (when t
                               (setq js2-basic-offset 2)))))

(use-package ac-js2
  :ensure t
  :commands
  (ac-js2-mode)
  :init
  (defun jojo/ac-js2-hook ()
    "Sets up ac-js2."
    (ac-js2-mode)
    (jojo/company-push-backend-local 'ac-js2-company))
  (add-hook 'js2-mode-hook #'jojo/ac-js2-hook))

(provide 'yzm-web)
