;;; nh-env.el --- Environment detection helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides variables and functions to detect the OS and whether Emacs is running
;; in a graphical (GUI) or terminal (text) environment.

;;; Code:

;; Detect operating system
(defconst nh-env/is-mac (eq system-type 'darwin)
  "Non-nil if running on macOS.")
(defconst nh-env/is-windows (eq system-type 'windows-nt)
  "Non-nil if running on Windows.")
(defconst nh-env/is-linux (eq system-type 'gnu/linux)
  "Non-nil if running on GNU/Linux.")

;; Detect if Emacs is running in a graphical environment
(defconst nh-env/is-gui (display-graphic-p)
  "Non-nil if Emacs is running in a graphical (GUI) environment.")

;; Detect if Emacs is running in a terminal
(defconst nh-env/is-terminal (not (display-graphic-p))
  "Non-nil if Emacs is running in a terminal (text) environment.")

;; Global variable indicating the current operating system: 'mac, 'windows, or 'linux
(defconst nh-env/system
  (cond
   (nh-env/is-mac 'mac)
   (nh-env/is-windows 'windows)
   (nh-env/is-linux 'linux)
   (t 'unknown))
  "Symbol representing the current operating system: 'mac, 'windows, or 'linux.")

;;; GUI environment settings
(when nh-env/is-gui
  ;; Enable right-click context menu
  (context-menu-mode 1)
  ;; Enable pixel-precision scrolling for smooth experience (Emacs 29+ only)
  (when (fboundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode 1)))

;;; Terminal environment settings
(when nh-env/is-terminal
  ;; Enable mouse support (for selection, moving point, etc.)
  (xterm-mouse-mode 1)
  ;; Smoother scrolling in terminal
  (setq scroll-margin 5
        scroll-step 1
        scroll-conservatively 20000
        scroll-preserve-screen-position 1
        auto-window-vscroll nil))

(provide 'nh-env)
;;; nh-env.el ends here 