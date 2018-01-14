;;;; -*- lexical-binding: t; -*-

(setq initial-scratch-message ";; Nothing.")
(global-display-line-numbers-mode)

;; Reverting buffers
(global-auto-revert-mode t) ; automatically reload buffers on change

;; Indenting
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq c-basic-offset 4)

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

;; Set this variable to a non-nil value to speed up display of characters
;; using large fonts, at the price of a larger memory footprint of the
;; Emacs session.
(setq inhibit-compacting-font-caches t)

;; https://stackoverflow.com/questions/3631220/fix-to-get-smooth-scrolling-in-emacs
(setq scroll-margin 5
      scroll-step 1
      scroll-conservatively 10000
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

(provide 'yzm-default)
