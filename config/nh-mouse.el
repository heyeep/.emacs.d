;;; nh-mouse.el --- mouse -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(mouse-wheel-mode 1)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse t)

;; Scroll one line at a time and never recenter, so the view doesn't jump.
(setq scroll-margin 0
      scroll-step 1
      scroll-conservatively 101
      scroll-preserve-screen-position t
      auto-window-vscroll nil)

(when (eq system-type 'darwin)
  ;; macOS momentum scrolling makes the view jitter.
  (setq mac-mouse-wheel-smooth-scroll nil)
  (setq mouse-wheel-scroll-amount-horizontal 1))

;; Terminals send the wheel as mouse-4 and mouse-5 clicks.
(unless (display-graphic-p)
  (xterm-mouse-mode 1)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

(provide 'nh-mouse)
;;; nh-mouse.el ends here
