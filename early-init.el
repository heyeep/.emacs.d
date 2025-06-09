;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-

;;; Commentary:
;; This file is loaded before init.el and before package.el initialization.
;; It's used to set up fundamental settings that need to be in place very early.

;;; Code:

;; Prevent package.el from loading packages at startup
;; This is important because we handle package initialization manually in init.el
(setq package-enable-at-startup nil)

;; Prefer loading newer compiled files
(setq load-prefer-newer t)

;; Native compilation settings
(when (featurep 'native-compile)
  ;; Silence compiler warnings as they're quite noisy
  (setq native-comp-async-report-warnings-errors nil)
  
  ;; Set the right directory for native compilation cache
  (add-to-list 'native-comp-eln-load-path 
               (expand-file-name "eln-cache/" user-emacs-directory)))

;; Temporarily increase GC threshold during startup
;; This makes startup faster by reducing garbage collection frequency
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Restore GC settings after startup (this will be overridden by init.el's more specific settings)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 100 1024 1024)  ; 100MB
                  gc-cons-percentage 0.1)))

;; Disable some UI elements early for faster startup
;; These can be re-enabled in init.el if needed
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Don't implicitly resize the frame when changing font size
(setq frame-inhibit-implied-resize t)

;; Disable startup screen early
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)

;; Avoid initial flash of unstyled modeline
;;(setq-default mode-line-format nil)

(provide 'early-init)
;;; early-init.el ends here
