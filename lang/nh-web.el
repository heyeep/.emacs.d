;;; nh-web.el --- Web development configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for web development (HTML, CSS, JS, etc.)

;;; Code:

;; Web Mode: Major mode for editing web templates (HTML, CSS, JS, etc.)
;; https://web-mode.org/
(use-package web-mode
  :ensure t
  :mode
  (("\\.html?\\'" . web-mode)
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
   ("\\.tsx\\'" . web-mode)
   ("\\.ejs\\'" . web-mode)
   ("\\.jsx\\'" . web-mode)
   ("\\.ts\\'" . web-mode)
   ("\\.css\\'" . web-mode)
   ("\\.vue\\'" . web-mode))
  :hook (web-mode . nh/web-mode-setup)
  :init
  (defun nh/web-mode-setup ()
    "Set indentation for web-mode buffers to 2 spaces."
    (let ((n 2))
      (setq-local web-mode-markup-indent-offset n)
      (setq-local web-mode-css-indent-offset n)
      (setq-local web-mode-code-indent-offset n))))

;; Emmet Mode: Fast HTML and CSS writing using abbreviations
;;  (e.g., 'ul>li*3' expands to a list)
;; https://github.com/smihica/emmet-mode
;; (use-package emmet-mode
;;   :ensure t
;;   :hook ((web-mode css-mode html-mode) . emmet-mode))

;; mhtml-mode: Built-in mode for HTML with embedded JS and CSS
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/HTML-Mode.html
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

;; Rainbow Mode: Highlight color codes in CSS files
;; https://elpa.gnu.org/packages/rainbow-mode.html
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

;; JS2 Mode: Advanced JavaScript editing
;; https://github.com/mooz/js2-mode
(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'" . js2-mode)
  :interpreter ("node" . js2-mode)
  :hook (js2-mode . nh/js2-setup)
  :custom
  (js-indent-level 2)
  (js2-basic-offset 2)
  (js2-highlight-level 3)
  (js2-idle-timer-delay 0.5)
  (js2-mode-show-parse-errors nil)
  (js2-mode-show-strict-warnings nil)
  :config
  (defun nh/js2-setup ()
    "Custom setup for js2-mode."
    (setq mode-name "JS2")))

;; JS2-JSX Mode: Fallback for mixed JS/HTML files
(use-package js2-jsx-mode
  :ensure nil ; it's part of js2-mode
  :mode ("\\.js\\'" . js2-jsx-mode))

;; RJSX Mode: Best for React/JSX files
(use-package rjsx-mode
  :ensure t
  :mode (("\\.js[x]?\\'" . rjsx-mode))
  :interpreter ("node" . rjsx-mode)
  :config
  ;; Workaround: align closing bracket with opening bracket in JSX
  (defun nh/js-jsx-indent-line-align-closing-bracket ()
    "Align closing JSX bracket with opening bracket."
    (save-excursion
      (beginning-of-line)
      (when (looking-at-p "^ +/?> *$")
        (delete-char sgml-basic-offset))))
  (advice-add #'js-jsx-indent-line :after #'nh/js-jsx-indent-line-align-closing-bracket)
  ;; Restore standard Emacs behavior for < and C-d in rjsx-mode
  (with-eval-after-load 'rjsx
    (define-key rjsx-mode-map "<" nil)
    (define-key rjsx-mode-map (kbd "C-d") nil)))

;; TypeScript Mode: Major mode for TypeScript
;; https://github.com/emacs-typescript/typescript.el
(use-package typescript-mode
  :ensure t
  :mode ("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-mode))

;; Tide: TypeScript Interactive Development Environment (also works for JS)
;; https://github.com/ananthakumaran/tide
(use-package tide
  :ensure t
  :commands (tide-setup)
  :init
  (defun +setup-tide-mode ()
    "Setup Tide in the current buffer, with project and file checks."
    (interactive)
    (when (locate-dominating-file default-directory "tsfmt.json")
      (add-hook 'before-save-hook #'tide-format-before-save nil t))
    ;; Disable linting for Typescript Definition files.
    (when (and (buffer-file-name)
               (string-match-p ".d.ts$" (buffer-file-name)))
      (flycheck-mode -1))
    (tide-setup)
    (tide-hl-identifier-mode +1))

  (defun my/js2-tide-setup ()
    (when (or (locate-dominating-file default-directory "tsconfig.json")
              (locate-dominating-file default-directory "jsconfig.json"))
      (+setup-tide-mode)))

  (defun my/web-tide-setup ()
    (when (and buffer-file-name
               (string-equal "tsx" (file-name-extension buffer-file-name)))
      (setq-local web-mode-enable-auto-quoting nil)
      (when (fboundp 'yas-activate-extra-mode)
        (yas-activate-extra-mode 'typescript-mode))
      (+setup-tide-mode)))
  :hook
  (typescript-mode . +setup-tide-mode)
  (js2-mode . my/js2-tide-setup)
  (web-mode . my/web-tide-setup)
  :config
  ;; Set up Typescript linting with `web-mode'.
  (with-eval-after-load 'flycheck
    (flycheck-add-mode 'typescript-tslint 'web-mode))
  ;; Fix eldoc warnings
  (setq eldoc-documentation-functions '(tide-eldoc-function))
  ;; Fix point-at-eol warnings
  (defalias 'tide-point-at-eol 'line-end-position))

;; Prettier-js: Format JS/TS/JSON/HTML/CSS using Prettier
;; https://github.com/prettier/prettier-emacs
(use-package prettier-js
  :ensure t
  :hook ((js2-mode . prettier-js-mode)
         (typescript-mode . prettier-js-mode)
         (rjsx-mode . prettier-js-mode)))

(provide 'nh-web)
;;; nh-web.el ends here 