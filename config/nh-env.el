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

;; Detect if running on a MacBook's built-in display (robust, checks name and width)
(defun nh/macbook-retina-p ()
  "Return t if running on a MacBook's built-in display (macOS only).
Checks for display name and known MacBook widths as fallback."
  (and nh-env/is-mac
       (let* ((displays (display-monitor-attributes-list))
              (macbook-names '("Color LCD" "Built-in Retina Display"))
              (macbook-widths '(1280 1440 1512 1728 1800 2234 2560 3024 3456))) ; Add your model's width if needed
         (or
          ;; Name-based detection
          (cl-some (lambda (display)
                     (let ((name (cdr (assoc 'name display))))
                       (and name (cl-some (lambda (n) (string-match-p n name)) macbook-names))))
                   displays)
          ;; Width-based fallback
          (let ((width (or (nth 3 (assoc 'geometry (frame-monitor-attributes))) 0)))
            (memq width macbook-widths))))))

;; Helper: Get the width in pixels of the current monitor (GUI only)
(defun nh/monitor-width ()
  "Return the width in pixels of the current monitor, or nil if not available.
Only returns a value in a GUI session."
  (when nh-env/is-gui
    (nth 3 (assoc 'geometry (frame-monitor-attributes)))))

;; Helper: Detect if running on a desktop or external monitor
(defun nh/desktop-p ()
  "Return t if running on a desktop or external monitor.
Checks for Windows, known desktop hostnames, or very wide monitors."
  (or
   nh-env/is-windows
   (member (system-name) '("nh-desktop" "desktop"))
   (> (or (nh/monitor-width) 0) 4400)))

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