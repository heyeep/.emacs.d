;;;; -*- lexical-binding: t; -*-

;; (package-initialize)


(setq gc-cons-threshold 100000000) ; 100 mb
(add-hook 'focus-out-hook 'garbage-collect)

(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))


;; M-x list-packages U x to upgrade packages.
(setq package-list '(diminish))

;; Disable in favor of `use-package'.
(setq package-enable-at-startup nil)

;; Package Repositories
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))

;; Activate all packages (in particular autoloads).
(package-initialize)

;; Bootstrap `use-package'.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish) ; for :diminish
(require 'bind-key) ; for :bind

;; Install package if not existing.
(setq use-package-always-ensure nil)

;; Check loading times with `use-package'.
(setq use-package-verbose t)

;; Fetch the list of packages when unavailable.
(when (not package-archive-contents)
  (package-refresh-contents))

;; Install any missing packages.
(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))

(use-package dash :ensure t)
(use-package s :ensure t)

(use-package gotham-theme :defer :ensure t)

;; https://stackoverflow.com/questions/9840558/why-cant-emacs-24-find-a-custom-theme-i-added
;; Add wildcard matching to themes in elpa folder.
 (-each
     (-map
      (lambda (item)
        (format "~/.emacs.d/elpa/%s" item))
      (-filter
       (lambda (item) (s-contains? "theme" item))
       (directory-files "~/.emacs.d/elpa/")))
   (lambda (item)
     (add-to-list 'custom-theme-load-path item)))

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
   (change-theme 'gotham 'gotham))

; Diminish modeline clutter.
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

 ;; Disable startup screen.
 (setq inhibit-startup-screen t)

 ;; Mute system sound.
 (setq ring-bell-function #'ignore)

 ;; Set title of window to current file.
 (setq frame-title-format '("%f"))

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

(dolist (mode '(prog-mode-hook
               yaml-mode-hook
               css-mode-hook
               html-mode-hook
               nxml-mode-hook))
 (add-hook mode (lambda ()
                  (interactive)
                  (electric-indent-local-mode 1))))

;; Indenting
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq c-basic-offset 4)

(add-hook 'prog-mode-hook 'eldoc-mode)

;; Reverting buffers
(global-auto-revert-mode t) ; automatically reload buffers on change

;; Highlight matching parentheses.
(use-package paren
 :ensure nil
 :config
 (show-paren-mode t))

;; No Autosave by default
(setq auto-save-default nil)

;; Don't make backups of files in version control.
(setq vc-make-backup-files nil)

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

(use-package exec-path-from-shell
   :ensure t
   :config
   (setq exec-path-from-shell-check-startup-files nil)
   (exec-path-from-shell-initialize))

(use-package magit
 :ensure t
 :commands (magit-toplevel magit-status magit-blame magit-log)
 :config
 (setq magit-repository-directories '("~/Developer"))
 (setq magit-refresh-status-buffer nil)
 ;; (setq magit-completing-read-function 'ivy-completing-read)
 (setq vc-handled-backends (delq 'Git vc-handled-backends)))

;; Don't let osx swallow Meta key.
 (setq mac-pass-command-to-system nil)

 ;; Command -> Meta
 ;; Option -> Super
 (setq mac-option-modifier 'super)
 (setq mac-command-modifier 'meta)

























































(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (magit exec-path-from-shell nlinum s dash use-package))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
