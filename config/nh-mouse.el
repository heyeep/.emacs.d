;;; nh-mouse.el --- mouse -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Enable mouse wheel support
(mouse-wheel-mode 1)

;; Configure mouse wheel scrolling - single line at a time
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)  ;; Don't accelerate scrolling
(setq mouse-wheel-follow-mouse t)         ;; Scroll window under mouse

;; Smooth scrolling settings - prevent jumping
(setq scroll-margin 0                     ;; No margin to prevent micro-movements
      scroll-step 1                       ;; Scroll one line at a time
      scroll-conservatively 101           ;; Never recenter point
      scroll-preserve-screen-position t   ;; Keep point position stable
      auto-window-vscroll nil)            ;; Disable partial line scrolling

;; macOS-specific mouse wheel settings
(when (eq system-type 'darwin)
  ;; Disable macOS momentum scrolling which can cause jitter
  (setq mac-mouse-wheel-smooth-scroll nil)
  ;; Use precise scrolling amounts
  (setq mouse-wheel-scroll-amount-horizontal 1))

;; Fix mouse wheel in terminal mode
(unless (display-graphic-p)
  ;; Enable mouse support in terminal
  (xterm-mouse-mode 1)
  ;; Map terminal mouse wheel events
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

(provide 'nh-mouse)
;;; nh-mouse.el ends here
