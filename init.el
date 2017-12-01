;;;; -*- lexical-binding: t; -*-

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

;; Start daemon automatically.
(add-hook 'after-init-hook (lambda ()
                             (load "server") ;; server-running-p is not autoloaded.
                             (unless (server-running-p)
                               (server-start))))

;; Config
(use-package yzm-autocompletion
  :load-path "config/yzm-autocompletion"
  :ensure nil)
(use-package yzm-keybindings
  :load-path "config/yzm-keybindings"
  :ensure nil)
(use-package yzm-mouse
  :load-path "config/yzm-mouse"
  :ensure nil)
(use-package yzm-theme
  :load-path "config/yzm-theme"
  :ensure nil)
(use-package yzm-git
  :load-path "config/yzm-git"
  :ensure nil)

;; Lang
(use-package yzm-elixir
  :load-path "lang/yzm-elixir"
  :ensure nil)
(use-package yzm-lisp
  :load-path "lang/yzm-lisp"
  :ensure nil)
(use-package yzm-lua
  :load-path "lang/yzm-lau"
  :ensure nil)
(use-package yzm-web
  :load-path "lang/yzm-web"
  :ensure nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (malinka ws-butler rtags dummy-h-mode flycheck-pos-tip flycheck company-quickhelp company projectile smex ivy expand-region typescript-mode tide add-node-modules-path clang-format company-ycmd ycmd counsel solarized-theme use-package))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
