;;; nh-web.el --- Web development configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for web development (HTML, CSS, JS, etc.)

;;; Code:

;; Web Mode: Major mode for editing web templates (HTML, CSS, JS, etc.)
;; https://web-mode.org/
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
  ("\\.tsx\\'" . web-mode)
  ("\\.ejs\\'" . web-mode))
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
  (defun nh/setup-tide-mode ()
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

  (defun nh/js2-tide-setup ()
    (when (or (locate-dominating-file default-directory "tsconfig.json")
              (locate-dominating-file default-directory "jsconfig.json"))
      (nh/setup-tide-mode)))

  (defun nh/web-tide-setup ()
    (when (and buffer-file-name
               (string-equal "tsx" (file-name-extension buffer-file-name)))
      (setq-local web-mode-enable-auto-quoting nil)
      (when (fboundp 'yas-activate-extra-mode)
        (yas-activate-extra-mode 'typescript-mode))
      (nh/setup-tide-mode)))
  :hook
  (typescript-mode . nh/setup-tide-mode)
  (js2-mode . nh/js2-tide-setup)
  (web-mode . nh/web-tide-setup)
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

;;;; JS Identation
;; Leaving Javascript indentation code outside of package block so other
;; packages can use the same indentation settings.

;; https://emacs.stackexchange.com/questions/29973/stop-javascript-mode-from-lining-up-function-parameters-after-newline/29975#29975
(defun +js--proper-indentation (parse-status)
  "Return the proper indentation for the current line."
  (save-excursion
    (back-to-indentation)
    (cond ((nth 4 parse-status)    ; inside comment
           (js--get-c-offset 'c (nth 8 parse-status)))
          ((nth 3 parse-status) 0) ; inside string
          ((eq (char-after) ?#) 0)
          ((save-excursion (js--beginning-of-macro)) 4)
          ;; Indent array comprehension continuation lines specially.
          ((let ((bracket (nth 1 parse-status))
                 beg)
             (and bracket
                  (not (js--same-line bracket))
                  (setq beg (js--indent-in-array-comp bracket))
                  ;; At or after the first loop?
                  (>= (point) beg)
                  (js--array-comp-indentation bracket beg))))
          ((js--chained-expression-p))
          ((js--ctrl-statement-indentation))
          ((js--multi-line-declaration-indentation))
          ((nth 1 parse-status)
           ;; A single closing paren/bracket should be indented at the
           ;; same level as the opening statement. Same goes for
           ;; "case" and "default".
           (let ((same-indent-p (looking-at "[]})]"))
                 (switch-keyword-p (looking-at "default\\_>\\|case\\_>[^:]"))
                 (continued-expr-p (js--continued-expression-p)))
             (goto-char (nth 1 parse-status)) ; go to the opening char
             (progn ; nothing following the opening paren/bracket
               (skip-syntax-backward " ")
               (when (eq (char-before) ?\)) (backward-list))
               (back-to-indentation)
               (js--maybe-goto-declaration-keyword-end parse-status)
               (let* ((in-switch-p (unless same-indent-p
                                     (looking-at "\\_<switch\\_>")))
                      (same-indent-p (or same-indent-p
                                         (and switch-keyword-p
                                              in-switch-p)))
                      (indent
                       (cond (same-indent-p
                              (current-column))
                             (continued-expr-p
                              (+ (current-column) (* 2 js-indent-level)
                                 js-expr-indent-offset))
                             (t
                              (+ (current-column) js-indent-level
                                 (pcase (char-after (nth 1 parse-status))
                                   (?\( js-paren-indent-offset)
                                   (?\[ js-square-indent-offset)
                                   (?\{ js-curly-indent-offset)))))))
                 (if in-switch-p
                     (+ indent js-switch-indent-offset)
                   indent)))))

          ((js--continued-expression-p)
           (+ js-indent-level js-expr-indent-offset))
          (t (prog-first-column)))))

(defun +25-js--proper-indentation (parse-status)
  "Return the proper indentation for the current line."
  (save-excursion
    (back-to-indentation)
    (cond ((nth 4 parse-status)    ; inside comment
           (js--get-c-offset 'c (nth 8 parse-status)))
          ((nth 3 parse-status) 0) ; inside string
          ((eq (char-after) ?#) 0)
          ((save-excursion (js--beginning-of-macro)) 4)
          ;; Indent array comprehension continuation lines specially.
          ((let ((bracket (nth 1 parse-status))
                 beg)
             (and bracket
                  (not (js--same-line bracket))
                  (setq beg (js--indent-in-array-comp bracket))
                  ;; At or after the first loop?
                  (>= (point) beg)
                  (js--array-comp-indentation bracket beg))))
          ((js--ctrl-statement-indentation))
          ((js--multi-line-declaration-indentation))
          ((nth 1 parse-status)
           ;; A single closing paren/bracket should be indented at the
           ;; same level as the opening statement. Same goes for
           ;; "case" and "default".
           (let ((same-indent-p (looking-at "[]})]"))
                 (switch-keyword-p (looking-at "default\\_>\\|case\\_>[^:]"))
                 (continued-expr-p (js--continued-expression-p)))
             (goto-char (nth 1 parse-status)) ; go to the opening char
             (progn ; nothing following the opening paren/bracket
               (skip-syntax-backward " ")
               (when (eq (char-before) ?\)) (backward-list))
               (back-to-indentation)
               (js--maybe-goto-declaration-keyword-end parse-status)
               (let* ((in-switch-p (unless same-indent-p
                                     (looking-at "\\_<switch\\_>")))
                      (same-indent-p (or same-indent-p
                                         (and switch-keyword-p
                                              in-switch-p)))
                      (indent
                       (cond (same-indent-p
                              (current-column))
                             (continued-expr-p
                              (+ (current-column) (* 2 js-indent-level)
                                 js-expr-indent-offset))
                             (t
                              (+ (current-column) js-indent-level
                                 (pcase (char-after (nth 1 parse-status))
                                   (?\( js-paren-indent-offset)
                                   (?\[ js-square-indent-offset)
                                   (?\{ js-curly-indent-offset)))))))
                 (if in-switch-p
                     (+ indent js-switch-indent-offset)
                   indent)))))

          ((js--continued-expression-p)
           (+ js-indent-level js-expr-indent-offset))
          (t 0))))

;; When Emacs 26 is released, parts of this can be removed.
(cond
 ((boundp 'js-indent-align-list-continuation)
  (setq js-indent-align-list-continuation nil))
 ((>= emacs-major-version 26)
  (advice-add 'js--proper-indentation :override '+js--proper-indentation))
 (:else
  (advice-add 'js--proper-indentation :override '+25-js--proper-indentation)))
(provide 'nh-web)
;;; nh-web.el ends here 
