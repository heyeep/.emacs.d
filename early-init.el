;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-

;;; Commentary:
;; Runs before init.el and before packages load. Only settings that must exist
;; that early belong here.

;;; Code:

;; init.el starts the package system itself.
(setq package-enable-at-startup nil)

(setq load-prefer-newer t)

(when (featurep 'native-compile)
  ;; Native compilation warnings are noisy and rarely actionable.
  (setq native-comp-async-report-warnings-errors nil)
  
  (add-to-list 'native-comp-eln-load-path 
               (expand-file-name "eln-cache/" user-emacs-directory)))

;; Skip garbage collection during startup, which makes startup faster.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; init.el sets its own values after this.
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 100 1024 1024)
                  gc-cons-percentage 0.1)))

;; Hide toolbars before the first frame draws, so it doesn't flicker.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq frame-inhibit-implied-resize t)

(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)

;; Disabled: hide the mode line until the theme loads.
;;(setq-default mode-line-format nil)

(provide 'early-init)
;;; early-init.el ends here
