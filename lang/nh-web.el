;;; nh-web.el --- Web development configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for web development (HTML, CSS, JS, etc.)

;;; Code:

;; Web Mode: Major mode for editing web templates
;; Provides syntax highlighting and indentation for HTML, CSS, JavaScript, and
;; various template languages like PHP, JSP, and ERB.
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


;; Emmet Mode: Fast HTML and CSS writing using abbreviations
;; Allows rapid HTML and CSS development using shorthand syntax that expands
;; into full markup (e.g., 'ul>li*3' becomes a list with three items).
;; GitHub: https://github.com/smihica/emmet-mode
;; (use-package emmet-mode
;;   :ensure t
;;   :hook ((web-mode css-mode html-mode) . emmet-mode))

;; Mhtml Mode: Built-in mode for HTML with embedded JS and CSS
;; Provides multi-mode editing capabilities for HTML files containing embedded
;; JavaScript and CSS with proper syntax highlighting for each language.
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
;; Automatically displays color values (hex, RGB, HSL) with their actual colors
;; as background, making it easy to visualize colors while editing stylesheets.
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

;; JS2 Mode: Advanced JavaScript editing
;; Enhanced JavaScript major mode with better syntax highlighting, error
;; detection, and support for modern JavaScript features including ES6+ syntax.
;; GitHub: https://github.com/mooz/js2-mode
;; NOTE: .js is owned by rjsx-mode (below), which derives from js2-mode.
;; js2-mode is loaded as the base library but does not claim .js itself,
;; to avoid a three-way auto-mode-alist fight (js2 / js2-jsx / rjsx).
(use-package js2-mode
  :ensure t
  :hook (js2-mode . nh/js2-setup)
  :custom
  (js-indent-level 2)
  (js2-basic-offset 2)
  (js2-highlight-level 3)
  (js2-idle-timer-delay 0.5)
  ;; Show real syntax errors (e.g. a mangled `const'), but keep strict
  ;; warnings off: they flag "missing ; after statement" on every line of
  ;; semicolon-less (ASI-style) code, which is valid JS.
  (js2-mode-show-parse-errors t)
  (js2-mode-show-strict-warnings nil)
  ;; Extra guard: never warn about omitted semicolons.
  (js2-strict-missing-semi-warning nil)
  :config
  (defun nh/js2-setup ()
    "Custom setup for js2-mode."
    (setq mode-name "JS2")))

;; RJSX Mode: React JSX syntax highlighting
;; Specialized major mode for React JSX files with enhanced support for JSX
;; syntax, automatic tag completion, and proper indentation for React components.
;; GitHub: https://github.com/felipeochoa/rjsx-mode
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
  (with-eval-after-load 'rjsx-mode
    (define-key rjsx-mode-map "<" nil)
    (define-key rjsx-mode-map (kbd "C-d") nil)))

;; TypeScript Mode: Major mode for TypeScript
;; Provides syntax highlighting, indentation, and basic editing support for
;; TypeScript files with type annotations and modern JavaScript features.
;; GitHub: https://github.com/emacs-typescript/typescript.el
(use-package typescript-mode
  :ensure t
  :mode ("\\.ts\\'" . typescript-mode)
  :init
  (add-hook 'typescript-mode-hook
            (lambda ()
              (setq-local typescript-indent-level 2))))

;; Tide: TypeScript Interactive Development Environment (also works for JS)
;; https://github.com/ananthakumaran/tide
(use-package tide
  :ensure t
  :commands (tide-setup)
  :init
  (defun nh/setup-tide-mode ()
    (interactive)
    (when (locate-dominating-file default-directory "tsfmt.json")
      (add-hook 'before-save-hook #'tide-format-before-save nil t))
    ;; Disable linting for Typescript Definition files.
    (when (and (buffer-file-name)
               (string-match-p ".d.ts$" (buffer-file-name)))
      (flycheck-mode -1))
    (tide-setup)
    (tide-hl-identifier-mode +1)
    ;; Ensure flycheck is enabled
    (flycheck-mode +1)
    ;; Set the checkers for this buffer
    (setq-local flycheck-checkers '(typescript-tide typescript-tsc)))
  (add-hook 'typescript-mode-hook #'nh/setup-tide-mode)

  (add-hook 'js2-mode-hook
            (lambda ()
              (when (or
                     (locate-dominating-file default-directory "tsconfig.json")
                     (locate-dominating-file default-directory "jsconfig.json"))
                (nh/setup-tide-mode))))

  (add-hook 'web-mode-hook
            (lambda ()
              ;; Set up Tide mode if Typescript.
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (setq-local web-mode-enable-auto-quoting nil)
                (when (fboundp 'yas-activate-extra-mode)
                  (yas-activate-extra-mode 'typescript-mode))
                (nh/setup-tide-mode))))
  :config
  ;; Configure Flycheck to use both tide and tsc checkers
  (with-eval-after-load 'flycheck
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    
    ;; Add Tide support to modes.
    ;; js2-mode/rjsx-mode are included so tide diagnostics work in .js
    ;; buffers too (nh/setup-tide-mode attaches tide there when a
    ;; jsconfig.json/tsconfig.json exists; without these entries flycheck
    ;; refuses to run the checker in those modes).
    (flycheck-add-mode 'typescript-tide 'web-mode)
    (flycheck-add-mode 'typescript-tide 'typescript-mode)
    (flycheck-add-mode 'typescript-tide 'js2-mode)
    (flycheck-add-mode 'typescript-tide 'rjsx-mode)

    ;; Define a proper typescript-tsc checker if it doesn't exist.
    ;; NOTE: web-mode is used for .tsx here (there is no `tsx-mode'), so the
    ;; checker must list web-mode or it never runs in .tsx buffers.
    (unless (flycheck-valid-checker-p 'typescript-tsc)
      (flycheck-define-checker typescript-tsc
        "TypeScript compiler for type checking."
        :command ("tsc" "--noEmit" "--pretty" "false"
                  "--jsx" "react"
                  "--skipLibCheck"
                  source-inplace)
        :error-patterns
        ((error line-start (file-name) "(" line "," column "): error TS" (id (one-or-more digit)) ": " (message) line-end))
        :modes (typescript-mode web-mode)))

    ;; Ensure the tsc checker is also recognized in web-mode (.tsx) buffers
    (flycheck-add-mode 'typescript-tsc 'web-mode)

    ;; Chain the checkers: run tsc after tide
    (flycheck-add-next-checker 'typescript-tide 'typescript-tsc 'append)))

;; Prettier JS: Format JavaScript code using Prettier
;; Automatically formats JavaScript, TypeScript, and JSX code on save using the
;; Prettier code formatter for consistent code style across projects.
;; GitHub: https://github.com/prettier/prettier-emacs
;; DISABLED: Uncommenting below to prevent auto-formatting on save
;; (use-package prettier-js
;;   :ensure t
;;   :hook ((js2-mode . prettier-js-mode)
;;          (typescript-mode . prettier-js-mode)
;;          (rjsx-mode . prettier-js-mode)))


;; Nodes Path
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

;; Don't line up function parameters / list continuations under the opening
;; paren; indent them one level instead.
;; (Emacs 26+ variable; the old Emacs-25 `js--proper-indentation' override
;; that used to live here was dead code on this Emacs and has been removed.)
(setq js-indent-align-list-continuation nil)

(provide 'nh-web)
;;; nh-web.el ends here 
