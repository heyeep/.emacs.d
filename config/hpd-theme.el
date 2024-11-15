;;;; -*- lexical-binding: t; -*-

;;; Themes
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;;(set-frame-font "Inconsolata for Powerline 12" nil t)
(set-face-attribute 'default nil :font "Inconsolata for Powerline" :height 128)
(use-package solarized-theme :defer :ensure t
  :config
  (setq solarized-distinct-fringe-background t)
  (setq solarized-use-less-bold t))

(when (memq window-system '(mac ns))
      ;; solves issue of not buildling in macOS
      (setenv "PKG_CONFIG_PATH" "/usr/local/lib/pkgconfig:/usr/local/Cellar/libffi/3.2.1/lib/pkgconfig"))

(use-package gotham-theme :defer :ensure t)

;; (use-package spacemacs-theme
;;   :defer
;;   :ensure t
;;   :init
;;   (setq spacemacs-theme-comment-bg nil))

(use-package theme-changer
  :ensure t
  :init
  ;; Dallas
  (setq calendar-latitude 32.85)
  (setq calendar-longitude -96.85)
  ;; Japan
  ;; (setq calendar-latitude 36.3147)
  ;; (setq calendar-longitude 139.8)
  :config
  (defun hpd/make-modeline-taller ()
    "Make the mode line taller."
    (dolist (sym '(mode-line mode-line-inactive))
      (set-face-attribute
       sym nil
       :box `(:line-width 5 :color ,(face-attribute `,sym :background)))))

  (defun is-daytime()
    "Figuring out day or night."
    (let*
        ((today-times (sunrise-sunset-times (today)))
         (sunrise-today (first today-times))
         (sunset-today (second today-times)))
      (daytime-p sunrise-today sunset-today)))
  (defun hpd/update-theme (&rest _args)
    "Update various UI elements when theme changes"
    (hpd/make-modeline-taller)
    (eval-after-load 'org-mode
      (lambda ()
        (hpd/customize-org-ui)))
    (when (fboundp 'org-reload)
      (eval-after-load 'org-faces
        (lambda ()
          (set-face-background 'org-hide (face-attribute 'default :background))
          (set-face-foreground 'org-hide (face-attribute 'default :background))))
      (when (let (has-org-mode)
              (dolist (b (buffer-list) has-org-mode)
                (with-current-buffer b
                  (when (eq major-mode 'org-mode)
                    (setq has-org-mode t)))))
        (org-reload)))
    (eval-after-load 'company
      (lambda ()
        (set-face-attribute
         'company-preview
         nil
         :background (face-attribute 'company-preview-common :background)))))

  (advice-add 'change-theme :after #'hpd/update-theme)
  (set-frame-parameter nil 'background-mode 'light)
  (change-theme 'solarized-dark-high-contrast 'solarized-dark)
  (run-hooks 'after-load-theme-hook))

(dolist (mode '(prog-mode-hook
                yaml-mode-hook
                css-mode-hook
                html-mode-hook
                nxml-mode-hook))
  (add-hook mode (lambda ()
                   (interactive)
                   (electric-indent-local-mode 1))))

(add-hook 'prog-mode-hook 'eldoc-mode)

;; Set this variable to a non-nil value to speed up display of characters
;; using large fonts, at the price of a larger memory footprint of the
;; Emacs session.
(setq inhibit-compacting-font-caches t)

;;; Misc

;; Tree
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  ;; Double undo limit
  (setq undo-limit 160000)
  (setq undo-strong-limit 240000)
  (setq undo-outer-limit 24000000)
  (global-undo-tree-mode))

;; Path initialize
(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))

;; Diminish modeline clutter.
(when (require 'diminish nil 'noerror)
  (diminish 'subword-mode)
  (diminish 'visual-line-mode)
  (diminish 'abbrev-mode)
  (eval-after-load "eldoc"
    '(diminish 'eldoc-mode))
  (eval-after-load "hideshow"
    '(diminish 'hs-minor-mode))
  (eval-after-load "autorevert"
    '(diminish 'auto-revert-mode)))


(dolist (mode '(prog-mode-hook
                yaml-mode-hook
                css-mode-hook
                html-mode-hook
                nxml-mode-hook))
  (add-hook mode (lambda ()
                   (interactive)
                   (electric-indent-local-mode 1))))

(add-hook 'prog-mode-hook 'eldoc-mode)

;; Highlight matching parentheses.
(use-package paren
  :ensure nil
  :config
  (show-paren-mode t))

; Smart Parentesis
(use-package smartparens
  :ensure t
  :config
  (use-package smartparens-config :ensure nil)
  (smartparens-global-mode 1)
  (sp-pair "'" nil :actions :rem))
(add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode)

;; Indentation
(defun indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (unless indent-tabs-mode
    (untabify (point-min) (point-max))))

(defun indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (unless indent-tabs-mode
            (untabify (point-min) (point-max)))
          (message "Indented selected region."))
      (progn
        (indent-buffer)
        (message "Indented buffer.")))))

;; Same filenames get the directory name inserted also.
(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator "|")
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*"))

(use-package highlight-symbol
  :ensure t
  :diminish highlight-symbol-mode
  :defer 5
  :config
  (setq highlight-symbol-idle-delay .5)

  (defun +match-highlight-symbol-face ()
    "Make `highlight-symbol-face' look like `highlight'."
    (set-face-attribute 'highlight-symbol-face nil
                        :background nil
                        :foreground nil
                        :inherit 'highlight))

  ;; Match for existing buffers.
  (dolist (b (buffer-list))
    (with-current-buffer b
      (when (and (derived-mode-p 'prog-mode)
                 (not (member major-mode '(typescript-mode))))
        (+match-highlight-symbol-face)
        (highlight-symbol-mode 1))))

  ;; Match after theme changes.
  (add-hook 'after-load-theme-hook #'+match-highlight-symbol-face)

  (add-hook 'prog-mode-hook
            (lambda ()
              ;; `tide-mode' already supplies a highlight.
              (unless (member major-mode '(typescript-mode))
                (+match-highlight-symbol-face)
                (highlight-symbol-mode 1)))))

(defun +lisp-modes ()
  "Return modes that are lispy."
  '(lisp-mode
    lisp-interaction-mode
    emacs-lisp-mode
    common-lisp-mode
    slime-mode
    clojure-mode
    cider-mode
    cider-repl-mode
    scheme-mode
    geiser-mode
    geiser-repl-mode))

(defun +lisp-hooks ()
  "Return hooks that are lispy."
  (mapcar #'+mode-hook (+lisp-modes)))

(defun +mode-hook (mode)
  "Return hook given `mode' symbol."
  (intern (concat (symbol-name mode) "-hook")))

(use-package rainbow-delimiters
  ;; Use colorful parens.
  :ensure t
  :commands (rainbow-delimiters-mode)
  :init
  (defun +bold-parens ()
    "Bold parentheses."
    (dotimes (i 9)
      (set-face-attribute
       (intern (format "rainbow-delimiters-depth-%d-face" (+ i 1))) nil :bold t)))
  (add-hook 'after-load-theme-hook #'+bold-parens)

  (dolist (hook (+lisp-hooks))
    (add-hook hook #'rainbow-delimiters-mode))
  :config
  (+bold-parens))

(use-package expand-region
  :ensure t
  :commands er/expand-region
  :bind ("C-;" . er/expand-region))

(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :config
  (setq dired-sidebar-use-custom-font t)
  (setq dired-sidebar-face
        (cond
         ((eq system-type 'darwin)
          '(:family "Inconsolata-g for Powerline" :height 140))
         ((eq system-type 'windows-nt)
          '(:family "Times New Roman" :height 150))
         (:default
          '(:family "Arial" :height 150))))

  (use-package all-the-icons-dired
    ;; M-x all-the-icons-install-fonts
    :ensure t
    :commands (all-the-icons-dired-mode)))

(use-package dired-collapse
  :ensure t
  :commands (dired-collapse-mode)
  :init
  (add-hook 'dired-mode-hook #'dired-collapse-mode))

(use-package dired-subtree
  :ensure t
  :commands (dired-subtree-toggle dired-subtree-cycle)
  :config
  (setq dired-subtree-line-prefix "_ ")
  (setq dired-subtree-use-backgrounds nil))

(use-package ace-window
  :ensure t
  :commands (ace-delete-window
             ace-swap-window
             ace-delete-other-windows
             ace-window
             aw-select))

(org-babel-do-load-languages
 'org-babel-load-languages '(
       (awk . t)
       (calc .t)
       (C . t)
       (emacs-lisp . t)
       (haskell . t)
       (gnuplot . t)
       (latex . t)
       (java . t)
       (js . t)
       (haskell . t)
       (perl . t)
       (python . t)
       (R . t)
       (ruby . t)
       (scheme . t)
       (shell . t)
       (sql . t)
       ))

(defun my-asm-mode-hook ()
  ;; you can use `comment-dwim' (M-;) for this kind of behaviour anyway
  (local-unset-key (vector asm-comment-char))
  ;; asm-mode sets it locally to nil, to "stay closer to the old TAB behaviour".
  (setq tab-always-indent (default-value 'tab-always-indent)))

(provide 'hpd-theme)
