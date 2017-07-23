;;;; -*- lexical-binding: t; -*-

;;; Themes
(use-package solarized-theme :defer :ensure t
  :config
  (setq solarized-distinct-fringe-background t)
  (setq solarized-use-less-bold t))


(use-package gotham-theme :defer :ensure t)

(use-package spacemacs-theme
  :defer
  :ensure t
  :init
  (setq spacemacs-theme-comment-bg nil))

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
  (defun jojo/make-modeline-taller ()
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

  (defun jojo/update-theme (&rest _args)
    "Update various UI elements when theme changes"
    (jojo/make-modeline-taller)
    (eval-after-load 'org-mode
      (lambda ()
        (jojo/customize-org-ui)))
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

  (advice-add 'change-theme :after #'jojo/update-theme)
  (set-frame-parameter nil 'background-mode 'light)
  (change-theme 'solarized-light 'solarized-dark))

;;; Neotree
(use-package neotree
  :ensure t
  :commands (neo-global--window-exists-p
             neotree-dir
             neotree-toggle
             neotree-enter
             neotree-hide)
  :init
  (setq neo-persist-show nil)
  (setq neo-window-fixed-size nil)
  (setq neo-create-file-auto-open t)
  (setq neo-smart-open nil)
  (setq neo-mode-line-type 'neotree)
  (setq neo-show-hidden-files t)
  (setq neo-mode-line-type 'none)
  (defun neotree-resize-window (&rest _args)
    "Resize neotree window.
https://github.com/jaypei/emacs-neotree/pull/110"
    (interactive)
    (neo-buffer--with-resizable-window
     (let ((fit-window-to-buffer-horizontally t))
       (fit-window-to-buffer))))

  (defun neotree-reopen (&rest _args)
    "Close and reopen neotree returning to the current window after.
`neotree-resize-window' doesn't work on file open (neotree window isn't live anymore).
Hiding and showing again will resize the neotree window properly."
    (interactive)
    (save-selected-window
      (neotree-hide)
      (neotree-show)))

  (defun neotree-resize-window-with-timer (&rest args)
    "Resize neotree window after waiting for redisplay."
    (when (neo-global--window-exists-p)
      (redisplay)
      (run-with-timer .1 nil #'neotree-resize-window args)))

  ;; When opening a file from neotree.
  (advice-add 'neo-open-file :after #'neotree-reopen)
  ;; When going up the directory stack.
  (advice-add 'neotree-change-root :after #'neotree-resize-window)
  ;; When opening a directory node.
  (advice-add 'neo-open-dir :after #'neotree-resize-window)
  ;; When showing neotree with `neotree-show'.
  (advice-add 'neotree-show :after #'neotree-resize-window)
  ;; Inside neotree-projectile
  (advice-add 'neotree-dir :after #'neotree-resize-window)
  ;; Inside neotree-projectile
  (advice-add 'neotree-find :after #'neotree-resize-window)

  (defun jojo/neotree-around-reopen (orig-fun &rest args)
    "Close neotree before calling advised function.
Reopen neotree after advised function runs."
    (interactive)
    (if (not (neo-global--window-exists-p))
        (apply orig-fun args)
      (save-selected-window
        (neotree-hide)
        (let ((res (apply orig-fun args)))
          (neotree-show)
          res))))

  (defun neotree-projectile ()
    "Open neotree with projectile as root and open node for current file.
If projectile unavailable or not in a project, open node at file path.
If file path is not available, open $HOME."
    (interactive)
    (if (neo-global--window-exists-p)
        (call-interactively 'neotree-hide)
      (let ((file-name (buffer-file-name)))
        (if (and (not file-name)
                 (let ((buffer-name (buffer-name)))
                   (cond
                    ((equal buffer-name "*cider-repl server*") nil)
                    (t t))))
            (neotree-dir "~/")
          (let ((dir-name (if (and (fboundp 'projectile-project-p)
                                   (projectile-project-p))
                              (projectile-project-root)
                            (file-name-directory file-name))))
            (neotree-dir dir-name)
            (neotree-find file-name))))))
  :config
  ;; Upon resizing window
  (advice-add #'toggle-frame-maximized
              :after #'neotree-resize-window-with-timer)

  ;; Try to use icons from `all-the-icons',
  ;; if font is not installed, we use fallback.
  (if (null (find-font (font-spec :name "github-octicons")))
      (setq neo-theme 'nerd)
    (use-package all-the-icons :ensure t)
    (setq neo-theme 'icons))
  )

;; emacs 24+ auto indents by default if `electric-indent-mode' is on
;; so disable automatic indent by default
;; but enable it in all programming modes.
(electric-indent-mode 0)

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

;; nlinum
(use-package nlinum
  :ensure t
  :init
  (dolist (hook '(prog-mode-hook
                  nxml-mode-hook))
    (add-hook hook #'nlinum-mode)))

;; https://stackoverflow.com/questions/3631220/fix-to-get-smooth-scrolling-in-emacs
(setq scroll-margin 5
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

;; emacs 24+ auto indents by default if `electric-indent-mode' is on
;; so disable automatic indent by default
;; but enable it in all programming modes.
(electric-indent-mode 0)

(dolist (mode '(prog-mode-hook
                yaml-mode-hook
                css-mode-hook
                html-mode-hook
                nxml-mode-hook))
  (add-hook mode (lambda ()
                   (interactive)
                   (electric-indent-local-mode 1))))

(add-hook 'prog-mode-hook 'eldoc-mode)

;; Reverting buffers
(global-auto-revert-mode t) ; automatically reload buffers on change

;; No Autosave by default
(setq auto-save-default nil)

;; Don't make backups of files in version control.
(setq vc-make-backup-files nil)

;; Disable startup screen.
(setq inhibit-startup-screen t)

;; Mute system sound.
(setq ring-bell-function #'ignore)

;; Set title of window to current file.
(setq frame-title-format '("%f"))

;; Wrap line when it reaches end.
(setq-default truncate-lines 1)
(global-visual-line-mode 0)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

(setq kill-whole-line t)
;; yes or no to y or n
(fset 'yes-or-no-p 'y-or-n-p)
;; Make the column number show up.
(column-number-mode 1)

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
  (smartparens-global-mode 1))


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

;; Indenting
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq c-basic-offset 4)

;; Same filenames get the directory name inserted also.
(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator "|")
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*"))

;; Set this variable to a non-nil value to speed up display of characters
;; using large fonts, at the price of a larger memory footprint of the
;; Emacs session.
(setq inhibit-compacting-font-caches t)

(provide 'yzm-theme)
