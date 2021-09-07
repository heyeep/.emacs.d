;;;; -*- lexical-binding: t; -*-

(setq initial-scratch-message "")
(global-display-line-numbers-mode)

;; Reverting buffers
(global-auto-revert-mode t) ; automatically reload buffers on change

;; Indenting
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)

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

;; Stop trailing spaces
(setq sentence-end-double-space nil)

;; Wrap line when it reaches end.
(setq-default truncate-lines 1)
(global-visual-line-mode 0)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

(setq kill-whole-line t)
;; yes or no to y or n
(fset 'yes-or-no-p 'y-or-n-p)
;; Make the column number show up.
(column-number-mode 1)

;; emacs 24+ auto indents by default if `electric-indent-mode' is on
;; so disable automatic indent by default
;; but enable it in all programming modes.
(electric-indent-mode 0)

;; Indenting for these modes
(dolist (mode '(prog-mode-hook
                yaml-mode-hook
                css-mode-hook
                html-mode-hook
                nxml-mode-hook))
  (add-hook mode (lambda ()
                   (electric-indent-local-mode 1))))

;; Set this variable to a non-nil value to speed up display of characters
;; using large fonts, at the price of a larger memory footprint of the
n;; Emacs session.
(setq inhibit-compacting-font-caches t)

;; https://stackoverflow.com/questions/3631220/fix-to-get-smooth-scrolling-in-emacs
(setq scroll-margin 5
      scroll-step 1
      scroll-conservatively 20000
      scroll-preserve-screen-position 1)

;; Which-Key
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init
  (which-key-mode)
  :config
  (setq which-key-sort-order
        'which-key-key-order-alpha))

(provide 'hpd-default)

;; (use-package markdown-mode
;;   :ensure t
;;   :commands (markdown-mode gfm-mode)
;;   :mode (("README\\.md\\'" . gfm-mode)
;;          ("\\.md\\'" . markdown-mode)
;;          ("\\.markdown\\'" . markdown-mode))
;;   :init (setq markdown-command "pandoc -t html"))

;; (use-package simple-httpd
;;   :ensure t
;;   :config
;;   (setq httpd-port 7070)
;;   (setq httpd-host (system-name)))

;; (use-package impatient-mode
;;   :ensure t
;;   :commands impatient-mode)

;; (defun my-markdown-filter (buffer)
;;   (princ
;;    (with-temp-buffer
;;      (let ((tmp (buffer-name)))
;;        (set-buffer buffer)
;;        (set-buffer (markdown tmp))
;;        (format "<!DOCTYPE html><html><title>Markdown preview</title><link rel=\"stylesheet\" href = \"https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/3.0.1/github-markdown.min.css\"/>
;; <body><article class=\"markdown-body\" style=\"box-sizing: border-box;min-width: 200px;max-width: 980px;margin: 0 auto;padding: 45px;\">%s</article></body></html>" (buffer-string))))
;;    (current-buffer)))

;; (defun my-markdown-preview ()
;;   "Preview markdown."
;;   (interactive)
;;   (unless (process-status "httpd")
;;     (httpd-start))
;;   (impatient-mode)
;;   (imp-set-user-filter 'my-markdown-filter)
;;   (imp-visit-buffer))
;; ;; Start grip when opening a markdown/org buffer
;; (add-hook 'markdown-mode-hook #'grip-mode)
;; (add-hook 'org-mode-hook #'grip-mode)

;; ;; Path to grip
;; (setq grip-binary-path "~/.local/bin/grip")

;; ;; set a browser for preview
;; (setq grip-url-browser "google-chrome")
;; (setq grip-update-after-change nil)


;; in my case it is /home/rushan/.local/bin/grip

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "/usr/bin/multimarkdown"))
