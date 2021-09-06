;;; -*- lexical-binding: t; -*-

;; (package-initialize)

(setq gc-cons-threshold 100000000) ; 100 mb
(add-hook 'focus-out-hook 'garbage-collect)

;; Separate package directories according to Emacs version.
;; Bytecode compiled in different Emacs versions are not
;; guaranteed to work with another.
(setq package-user-dir
      (format "%selpa/%s/" user-emacs-directory emacs-major-version))

(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))

;; Disable in favor of `use-package'.
(setq package-enable-at-startup nil)

;; Package Repositories
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))

(setq package-archive-priorities '(("org" . 10)
                                   ("melpa" . 5)
                                   ("gnu" . 1)))
;; Activate all packages (in particular autoloads).
(package-initialize)

;; Auto downloads packages from mepla
;; diminish is does not come with 'use-package' any
;; has to be installed manually
(require 'cl-lib)
(defvar my-packages
  '(diminish)
  "A list of packages to ensure are installed at launch.")

(defun my-packages-installed-p ()
  (cl-loop for p in my-packages
           when (not (package-installed-p p)) do (cl-return nil)
           finally (cl-return t)))

(unless (my-packages-installed-p)
  ;; check for new packages (package versions)
  (package-refresh-contents)
  ;; install the missing packages
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))

;; M-x list-packages U x to upgrade packages.
(setq package-list '(diminish))

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

;; Start daemon automatically.
(add-hook 'after-init-hook (lambda ()
                             (load "server") ;; server-running-p is not autoloaded.
                             (unless (server-running-p)
                               (server-start))))

;; Config
(use-package hpd-default :ensure nil)
(use-package hpd-autocompletion :ensure nil)
(use-package hpd-keybindings :ensure nil)
(use-package hpd-mouse :ensure nil)
(use-package hpd-org :ensure nil)
(use-package hpd-theme :ensure nil)
(use-package hpd-git :ensure nil)
;; (use-package hpd-evil :ensure nil)
(use-package hpd-documents :ensure nil)

;; Lang
(use-package hpd-elixir :ensure nil)
(use-package hpd-elisp :ensure nil)
(use-package hpd-golang :ensure nil)
(use-package hpd-haskell :ensure nil)
(use-package hpd-latex :ensure nil)
(use-package hpd-lisp :ensure nil
  :mode
  ("\\.l\\'" . +commonlisp-mode)
  ("\\.li?sp\\'" . +commonlisp-mode)
  ("\\.lisp\\'" . +commonlisp-mode))
(use-package hpd-lua :ensure nil)
(use-package hpd-python :ensure nil)
(use-package hpd-scheme :ensure nil)
(use-package hpd-web :ensure nil)
;; Yaml
(use-package yaml-mode
  :ensure t
  :mode ("\\.yml\\'" . yaml-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "b550fc3d6f0407185ace746913449f6ed5ddc4a9f0cf3be218af4fb3127c7877" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default))
 '(package-selected-packages
   '(markdown-preview-mode simple-httpd markdown-mode grip-mode auto-sudoedit go-autocomplete meghanada javadoc-lookup android-mode magit lorem-ipsum graphviz-dot-mode org-plus-contrib htmlize yaml-mode go-dlv godoctor company-go go-guru go-mode company-anaconda anaconda-mode slime-company evil-magit preview-latex auctex pdf-tools ni all-the-icons-dired slime malinka ws-butler rtags dummy-h-mode flycheck-pos-tip flycheck company-quickhelp company projectile smex ivy expand-region typescript-mode tide add-node-modules-path clang-format company-ycmd ycmd counsel solarized-theme use-package))
 '(safe-local-variable-values '((geiser-scheme-implementation racket))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
