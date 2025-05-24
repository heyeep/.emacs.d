;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-

;;; Prevent package.el from automatically loading packages at startup
(setq package-enable-at-startup nil)
(setq load-prefer-newer t)

;;; Store installed packages in a versioned elpa directory for each Emacs major version
(setq package-user-dir
      (format "%selpa/%s/" user-emacs-directory emacs-major-version))

;;; Optimize garbage collection for faster startup and efficient runtime
(setq gc-cons-threshold (* 128 1024 1024))  ; 128MB during startup
(setq gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 100000000) ; 100MB after startup
            (setq gc-cons-percentage 0.1)))

;; Run garbage collection when Emacs loses focus
(add-hook 'focus-out-hook #'garbage-collect)

;; Ensure our submodule version of transient is loaded FIRST.
;; This must go before normal-top-level-add-subdirs-to-load-path,
;; because that function prepends all subdirectories (including elpa, site-lisp, etc.)
;; to the load-path. If we add our submodule after, it will be at the end and Emacs
;; will find the built-in or ELPA version first, causing version mismatches.
(add-to-list 'load-path (expand-file-name "submodules/transient/lisp" user-emacs-directory))

;;; Add all subdirectories of ~/.emacs.d/ to the load-path
(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))

;;; Set up package repositories (GNU, MELPA, MELPA Stable, Org)
(require 'package)
(setq package-archives
      '(("gnu"          . "https://elpa.gnu.org/packages/")
        ("melpa"        . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("org"          . "https://orgmode.org/elpa/")))

;;; Set package archive priorities: org > melpa/melpa-stable > gnu
(setq package-archive-priorities
      '(("org"          . 20)
        ("melpa"        . 10)
        ("melpa-stable" . 10)
        ("gnu"          . 5)))

;;; Bootstrap use-package and diminish
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;; Ensure diminish is installed for use-package :diminish and runtime use
(unless (package-installed-p 'diminish)
  (package-refresh-contents)
  (package-install 'diminish))

;;; Require diminish at compile time so :diminish works in use-package declarations
(eval-when-compile
  (require 'use-package)
  (require 'bind-key)
  (require 'diminish))

;;; Require diminish at runtime in case it is used outside of use-package
(require 'diminish)

(setq use-package-always-ensure t)

;;; Show use-package loading times for profiling
(setq use-package-verbose t)

;;; Fetch the list of packages when unavailable
(when (not package-archive-contents)
  (package-refresh-contents))

;;; Install any missing packages from package-list
(defvar package-list nil
  "List of packages to ensure are installed at startup.")
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Load helpers first to get utility functions
(require 'nh-helpers)

(add-hook 'after-init-hook
          (lambda ()
            (load "server") ;; server-running-p is not autoloaded.
            (unless (server-running-p)
              (server-start))
            ;; Load configuration files in explicit order
            (require 'nh-env)
            (require 'nh-helpers)
            (require 'nh-default)
            (require 'nh-theme)
            (require 'nh-dired)
            (require 'nh-autocompletion)
            (require 'nh-copilot-ai)
            (require 'nh-git)
            (require 'nh-terminal)
            (require 'nh-keybindings)
            (require 'nh-mouse)
            (require 'nh-org)
            ;; Load all language-specific configuration files
            (nh/load-directory (expand-file-name "lang" user-emacs-directory))))

(setq native-comp-async-report-warnings-errors nil)

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("2b0fcc7cc9be4c09ec5c75405260a85e41691abb1ee28d29fcd5521e4fca575b"
     "b49f66a2e1724db880692485a5d5bcb9baf28ed2a3a05c7a799fa091f24321da"
     "7fea145741b3ca719ae45e6533ad1f49b2a43bf199d9afaee5b6135fd9e6f9b8"
     default))
 '(package-selected-packages
   '(ag all-the-icons-ivy-rich circadian company-box copilot counsel
        diminish enh-ruby-mode exec-path-from-shell geiser
        graphviz-dot-mode highlight-parentheses highlight-symbol
        htmlize ivy-prescient keycast magit multi-vterm org-bullets
        org-download org-modern paredit prettier-js rainbow-delimiters
        rainbow-mode rjsx-mode robe slime solarized-theme tide
        typescript-mode web-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
