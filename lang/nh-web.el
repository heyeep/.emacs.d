;;; nh-web.el --- Web development configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; HTML, CSS, JavaScript and TypeScript setup.

;;; Code:

;; web-mode: edits HTML templates (ERB, PHP, JSP and others) and .tsx files.
;; GitHub: https://github.com/fxbois/web-mode
(use-package web-mode
  :ensure t
  :mode
  (("\\.html\\'" . web-mode)
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
  ("\\.ejs\\'" . web-mode)
  ("\\.tsx\\'" . web-mode))
  :hook (web-mode . nh/web-mode-setup)
  :init
  (defun nh/web-mode-setup ()
    "Set indentation for web-mode buffers to 2 spaces."
    (let ((n 2))
      (setq-local web-mode-markup-indent-offset n)
      (setq-local web-mode-css-indent-offset n)
      (setq-local web-mode-code-indent-offset n))))


;; Disabled: emmet-mode expands shorthand like ul>li*3 into HTML.
;; GitHub: https://github.com/smihica/emmet-mode
;; (use-package emmet-mode
;;   :ensure t
;;   :hook ((web-mode css-mode html-mode) . emmet-mode))

;; mhtml-mode: Emacs's built-in mode for HTML with embedded CSS and JavaScript.
(use-package mhtml-mode
  :ensure nil
  :mode ("\\.[sx]?html?\\(\\.[a-zA-Z_]+\\)?\\'" . mhtml-mode)
  :hook (mhtml-mode . nh/mhtml-setup)
  :init
  (defun nh/mhtml-setup ()
    "Set indentation for HTML, CSS, and JS in mhtml-mode to 2 spaces."
    (setq-local css-indent-offset 2)
    (setq-local js-indent-level 2)
    (setq-local sgml-basic-offset 2)))

;; rainbow-mode: shows CSS color codes in their own color.
;; GitHub: https://github.com/emacsmirror/rainbow-mode
(use-package rainbow-mode
  :ensure t
  :commands (rainbow-mode)
  :diminish rainbow-mode
  :hook (css-mode . nh/css-rainbow-setup)
  :init
  (defun nh/css-rainbow-setup ()
    "Enable rainbow-mode and set css-indent-offset to 2 in css-mode."
    (setq css-indent-offset 2)
    (rainbow-mode 1)))

;; js2-mode: JavaScript mode with its own parser and error checking.
;; GitHub: https://github.com/mooz/js2-mode
;; rjsx-mode below owns .js files. js2-mode loads only as its base library,
;; so js2, js2-jsx and rjsx don't compete for .js in auto-mode-alist.
(use-package js2-mode
  :ensure t
  :hook (js2-mode . nh/js2-setup)
  :custom
  (js-indent-level 2)
  (js2-basic-offset 2)
  (js2-highlight-level 3)
  (js2-idle-timer-delay 0.5)
  ;; Show syntax errors, but not strict warnings. Those flag every line of
  ;; valid JavaScript written without semicolons.
  (js2-mode-show-parse-errors t)
  (js2-mode-show-strict-warnings nil)
  (js2-strict-missing-semi-warning nil)
  :config
  (defun nh/js2-setup ()
    "Custom setup for js2-mode."
    (setq mode-name "JS2")))

;; rjsx-mode: JavaScript and React JSX files.
;; GitHub: https://github.com/felipeochoa/rjsx-mode
(use-package rjsx-mode
  :ensure t
  :mode (("\\.js[x]?\\'" . rjsx-mode))
  :interpreter ("node" . rjsx-mode)
  :config
  ;; js-jsx indents a lone closing > one level too deep; pull it back.
  (defun nh/js-jsx-indent-line-align-closing-bracket ()
    "Align closing JSX bracket with opening bracket."
    (save-excursion
      (beginning-of-line)
      (when (looking-at-p "^ +/?> *$")
        (delete-char sgml-basic-offset))))
  (advice-add #'js-jsx-indent-line :after #'nh/js-jsx-indent-line-align-closing-bracket)
  ;; rjsx rebinds < and C-d to insert tags; keep Emacs's normal keys.
  (with-eval-after-load 'rjsx-mode
    (define-key rjsx-mode-map "<" nil)
    (define-key rjsx-mode-map (kbd "C-d") nil)))

;; typescript-mode: major mode for TypeScript files.
;; GitHub: https://github.com/emacs-typescript/typescript.el
(use-package typescript-mode
  :ensure t
  :mode ("\\.ts\\'" . typescript-mode)
  :init
  (add-hook 'typescript-mode-hook
            (lambda ()
              (setq-local typescript-indent-level 2))))

;; Tide: TypeScript language support through tsserver, also used for JavaScript.
;; https://github.com/ananthakumaran/tide
(use-package tide
  :ensure t
  :commands (tide-setup)
  :init
  (defun nh/setup-tide-mode ()
    (interactive)
    (when (locate-dominating-file default-directory "tsfmt.json")
      (add-hook 'before-save-hook #'tide-format-before-save nil t))
    (tide-setup)
    (tide-hl-identifier-mode +1)
    ;; Don't lint TypeScript definition files.
    (flycheck-mode (if (and buffer-file-name
                            (string-match-p "\\.d\\.ts\\'" buffer-file-name))
                       -1
                     +1))
    (setq-local flycheck-checkers '(typescript-tide))
    (setq-local flycheck-check-syntax-automatically '(save mode-enabled)))
  (add-hook 'typescript-mode-hook #'nh/setup-tide-mode)

  (add-hook 'js2-mode-hook
            (lambda ()
              (when (or
                     (locate-dominating-file default-directory "tsconfig.json")
                     (locate-dominating-file default-directory "jsconfig.json"))
                (nh/setup-tide-mode))))

  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (setq-local web-mode-enable-auto-quoting nil)
                (when (fboundp 'yas-activate-extra-mode)
                  (yas-activate-extra-mode 'typescript-mode))
                (nh/setup-tide-mode))))
  :config
  (with-eval-after-load 'flycheck
    ;; Flycheck only runs a checker in modes it lists. Tide also runs in
    ;; JavaScript buffers when the project has a tsconfig or jsconfig file.
    (flycheck-add-mode 'typescript-tide 'web-mode)
    (flycheck-add-mode 'typescript-tide 'typescript-mode)
    (flycheck-add-mode 'typescript-tide 'js2-mode)
    (flycheck-add-mode 'typescript-tide 'rjsx-mode)))

;; Disabled to avoid formatting on save: prettier-js runs Prettier on JS and TS.
;; GitHub: https://github.com/prettier/prettier-emacs
;; (use-package prettier-js
;;   :ensure t
;;   :hook ((js2-mode . prettier-js-mode)
;;          (typescript-mode . prettier-js-mode)
;;          (rjsx-mode . prettier-js-mode)))


;; Disabled: adds the project's node_modules/.bin to the command path.
;; (use-package add-node-modules-path
;;   :ensure t
;;   :commands (add-node-modules-path)
;;   :init
;;   (mapcar
;;    (lambda (x)
;;      (add-hook x #'add-node-modules-path))
;;    '(js-mode-hook
;;      js2-mode-hook
;;      rjsx-mode-hook
;;      typescript-mode-hook
;;      web-mode-hook)))

;;;; JS Indentation

;; Indent continued arguments one level, instead of under the opening paren.
(setq js-indent-align-list-continuation nil)

(provide 'nh-web)
;;; nh-web.el ends here 
