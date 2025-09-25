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
(add-hook 'focus-out-hooks #'garbage-collect-maybe)

;; Ensure our submodule version of transient is loaded FIRST.
;; This must go before normal-top-level-add-subdirs-to-load-path,
;; because that function prepends all subdirectories (including elpa, site-lisp, etc.)
;; to the load-path. If we add our submodule after, it will be at the end and Emacs
;; will find the built-in or ELPA version first, causing version mismatches.
(add-to-list 'load-path (expand-file-name "submodules/transient/lisp" user-emacs-directory))

;; Prevent loading of ELPA transient by marking it as loaded
(provide 'transient-autoloads)

;;; Add all subdirectories of ~/.emacs.d/ to the load-path
(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))

;; Remove any ELPA transient directories from load-path
;; (require 'cl-lib)  ; Required for cl-remove-if
;; (setq load-path
;;       (cl-remove-if (lambda (path)
;;                       (and (string-match-p "elpa" path)
;;                            (string-match-p "transient" path)))
;;                     load-path))

;; ;; Load transient from submodule immediately to prevent conflicts
;; (require 'transient nil t)

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

;;; Install any missing packages from package-list
(defvar package-list nil
  "List of packages to ensure are installed at startup.")
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;;; Set up exec-path-from-shell early to ensure PATH is correct
;;; This needs to happen before any other packages that depend on external programs
;;; Note: This is particularly important on macOS and Linux where PATH may not be set correctly in GUI Emacs
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :config
  (setq exec-path-from-shell-arguments '("-l" "-i"))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs '("OPENAI_API_KEY" "OPEN_API_KEY" "GEMINI_API_KEY" "CLAUDE_API_KEY" "ANTHROPIC_API_KEY" 
                                     "EDITOR" "VISUAL"
                                     "GOPATH" "GOBIN"
                                     "ANDROID_HOME" "ANDROID_SDK_ROOT" "ANDROID_AVD_HOME"
                                     "NPM_AUTH_TOKEN" "NPM_TOKEN"
                                     "LIBCLANG_LIBDIR"
                                     "DISABLE_SPRING" "OBJC_DISABLE_INITIALIZE_FORK_SAFETY"
                                     "LDFLAGS" "CPPFLAGS" "PKG_CONFIG_PATH"
                                     "HOMEBREW_NO_ANALYTICS"
                                     "XCODE_BUILD"
                                     "COPYFILE_DISABLE" "DISABLE_AUTO_TITLE"
                                     "LC_COLLATE" "LC_ALL" "LANG" "LANGUAGE")))

;; Track loading statistics
(defvar nh/load-stats '(:success 0 :failed 0 :errors nil)
  "Statistics for module loading.")

;; Function to show loading summary
(defun nh/show-load-summary ()
  "Display a summary of module loading statistics."
  (let ((success (plist-get nh/load-stats :success))
        (failed (plist-get nh/load-stats :failed))
        (errors (plist-get nh/load-stats :errors))
        (total-time (float-time (time-subtract after-init-time before-init-time))))
    (message "════════════════════════════════════════")
    (message "Emacs Startup Summary:")
    (message "  Total modules: %d (✓ %d succeeded, ✗ %d failed)"
             (+ success failed) success failed)
    (message "  Total startup time: %.3f seconds" total-time)
    (when (> failed 0)
      (message "  Failed modules:")
      (dolist (error errors)
        (message "    - %s: %s" (car error) (cdr error))))
    (message "════════════════════════════════════════")))

;; Debug function for tracking module loading
(defun nh/require-with-log (feature)
  "Require FEATURE with logging and error tracking."
  (let ((start-time (current-time)))
    (message "[init.el] Loading %s..." feature)
    (condition-case err
        (progn
          (require feature)
          (let ((load-time (float-time (time-subtract (current-time) start-time))))
            (message "[init.el] ✓ Loaded %s (%.3fs)" feature load-time)
            (plist-put nh/load-stats :success (1+ (plist-get nh/load-stats :success)))))
      (error
       (message "[init.el] ✗ Failed to load %s: %s" feature (error-message-string err))
       (plist-put nh/load-stats :failed (1+ (plist-get nh/load-stats :failed)))
       (plist-put nh/load-stats :errors
                  (append (plist-get nh/load-stats :errors)
                          (list (cons feature (error-message-string err)))))))))

;; Set font early before loading other modules
(set-face-attribute 'default nil
                    :font "Iosevka Etoile"
                    :height 140)
(add-to-list 'default-frame-alist '(font . "Iosevka Etoile-14"))

;; Load core configuration files immediately
(message "[init.el] Starting configuration load...")
(nh/require-with-log 'nh-env)
(nh/require-with-log 'nh-default)  ;; This contains inhibit-startup-screen setting

(add-hook 'after-init-hook
          (lambda ()
            (load "server") ;; server-running-p is not autoloaded.
            (unless (server-running-p)
              (server-start))
            ;; Load remaining configuration files
            (nh/require-with-log 'nh-helpers)
            (nh/require-with-log 'nh-commands)
            (nh/require-with-log 'nh-theme)
            (nh/require-with-log 'nh-dired)
            (nh/require-with-log 'nh-autocompletion)
            (nh/require-with-log 'nh-git)
            (nh/require-with-log 'nh-terminal)
            (nh/require-with-log 'nh-keybindings)
            (nh/require-with-log 'nh-mouse)
            (nh/require-with-log 'nh-org)
            (nh/require-with-log 'nh-debug)
            (nh/require-with-log 'nh-copilot-ai)
            (nh/require-with-log 'nh-aider-ai)
            (nh/require-with-log 'nh-tools)
            ;; Load all language-specific configuration files
            (if (fboundp 'nh/load-directory)
                (nh/load-directory (expand-file-name "lang" user-emacs-directory))
              (message "[init.el] ✗ Cannot load language files - nh/load-directory not defined"))
            ;; Load all experiment configuration files
            ;; (if (fboundp 'nh/load-directory)
            ;;     (nh/load-directory (expand-file-name "experiments" user-emacs-directory))
            ;;   (message "[init.el] ✗ Cannot load experiment files - nh/load-directory not defined"))
            ;; Display loading summary
            (nh/show-load-summary)))

(setq byte-compile-warnings nil)  ; Suppress all byte-compilation warnings

;; Suppress the specific make-network-process warning
(advice-add 'display-warning :around
            (lambda (orig-fun type message &optional level buffer-name)
              (unless (and (eq type 'bytecomp)
                          (string-match-p "make-network-process.*:service" message))
                (apply orig-fun type message level buffer-name))))

;; (add-to-list 'load-path "/Users/hiep/Code/claude/vaibe")
;; (require 'vaibe)
;; (add-to-list 'load-path "/Users/hiep/Code/ellm")
(add-to-list 'load-path "/Users/hiep/Code/claude/ellm")
(require 'ellm)
;; (add-to-list 'load-path "/Users/hiep/Code/claude/fragment")
;; (require 'fragment-demo)

;;Set logging config BEFORE loading vaibe
;; (setq vaibe-log-buffer-enabled t
;;       vaibe-log-file-enabled nil)

;;(add-to-list 'load-path "/Users/hiep/Code/claude/vaibe-mode")
;; (require 'vaibe)
;; (require 'vaibe-tools)
;; (setq vaibe-api-streaming-enabled t
;;       vaibe-logging-enabled t
;;       vaibe-logging-level 'trace
;;       vaibe-logging-categories 'all)

;;Auto-run vaibe API test after Emacs starts
;; (add-hook 'after-init-hook
;;          (lambda ()
;;            (vaibe-test-markdown-folding)))

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("c5975101a4597094704ee78f89fb9ad872f965a84fb52d3e01b9102168e8dc40"
     "a9028cd93db14a5d6cdadba789563cb90a97899c4da7df6f51d58bb390e54031"
     "1c2fb3448ce245f18c62fde3c7cfd008e69a27e88ae8a03fbb62857f13d0b6fe"
     "7235b77f371f46cbfae9271dce65f5017b61ec1c8687a90ff30c6db281bfd6b7"
     "7fd8b914e340283c189980cd1883dbdef67080ad1a3a9cc3df864ca53bdc89cf"
     "53a4efdca4c9fb870c3f92e4cfca0fbb638bb29b168a26a363298f9b1d9b9bcf"
     "2b0fcc7cc9be4c09ec5c75405260a85e41691abb1ee28d29fcd5521e4fca575b"
     "b49f66a2e1724db880692485a5d5bcb9baf28ed2a3a05c7a799fa091f24321da"
     "7fea145741b3ca719ae45e6533ad1f49b2a43bf199d9afaee5b6135fd9e6f9b8"
     default))
 '(highlight-parentheses-colors '("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900"))
 '(package-selected-packages nil)
 '(warning-suppress-log-types '((copilot copilot-no-mode-indent) (bytecomp)))
 '(warning-suppress-types '((use-package) (bytecomp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(header-line ((t (:inherit default :background unspecified))))
 '(lsp-ui-doc ((t (:background "#fbf7f0"))))
 '(lsp-ui-doc-background ((t (:background "#fbf7f0"))))
 '(lsp-ui-doc-header ((t (:inherit font-lock-keyword-face :background "#dfd9cf" :foreground "#0031a9" :weight bold :height 1.1 :box (:line-width (4 . 4) :color "#dfd9cf")))))
 '(lsp-ui-doc-markdown-code-block-face ((t (:background "#fbf7f0"))))
 '(lsp-ui-doc-url ((t (:inherit link :background "#fbf7f0"))))
 '(markdown-blockquote-face ((t (:inherit font-lock-comment-face :slant italic))))
 '(markdown-bold-face ((t (:weight bold))))
 '(markdown-code-face ((t (:inherit fixed-pitch :background "#f6f8fa"))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.8 :weight extra-bold))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.4 :weight bold))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.2 :weight bold))))
 '(markdown-header-face-4 ((t (:inherit markdown-header-face :height 1.0 :weight semi-bold))))
 '(markdown-header-face-5 ((t (:inherit markdown-header-face :height 0.9))))
 '(markdown-header-face-6 ((t (:inherit markdown-header-face :height 0.8))))
 '(markdown-inline-code-face ((t (:inherit fixed-pitch :background "#f6f8fa" :foreground "#e36209"))))
 '(markdown-italic-face ((t (:slant italic))))
 '(markdown-link-face ((t (:foreground "#0366d6" :underline t))))
 '(markdown-pre-face ((t (:inherit fixed-pitch :background "#f6f8fa"))))
 '(markdown-url-face ((t (:foreground "#6a737d" :height 0.8)))))
