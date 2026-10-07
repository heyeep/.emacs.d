;;; nh-helpers.el --- Helper functions and utilities -*- lexical-binding: t; -*-

;;; Commentary:
;; Helper functions used across the config.

;;; Code:

(defun nh/load-directory (dir)
  "Load all .el files from DIR."
  (add-to-list 'load-path dir)
  (message "[init.el] Loading files from %s..." dir)
  (dolist (file (directory-files dir t "\\.el\\'"))
    (when (not (string-match-p "\\`\\." (file-name-nondirectory file)))
      (let ((feature (intern (file-name-base file)))
            (start-time (current-time)))
        (message "[init.el] Loading %s..." feature)
        (condition-case err
            (progn
              (require feature)
              (let ((load-time (float-time (time-subtract (current-time) start-time))))
                (message "[init.el] ✓ Loaded %s (%.3fs)" feature load-time)
                (when (boundp 'nh/load-stats)
                  (plist-put nh/load-stats :success (1+ (plist-get nh/load-stats :success))))))
          (error
           (message "[init.el] ✗ Failed to load %s: %s" feature (error-message-string err))
           (when (boundp 'nh/load-stats)
             (plist-put nh/load-stats :failed (1+ (plist-get nh/load-stats :failed)))
             (plist-put nh/load-stats :errors 
                        (append (plist-get nh/load-stats :errors) 
                                (list (cons feature (error-message-string err))))))))))))

(defun nh/indent-buffer ()
  "Indent the currently visited buffer, remove trailing whitespace, and untabify if needed."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (unless indent-tabs-mode
    (untabify (point-min) (point-max))))

(defun nh/indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer. Cleans up whitespace and untabifies if needed."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (unless indent-tabs-mode
            (untabify (point-min) (point-max)))
          (message "Indented selected region."))
      (progn
        (nh/indent-buffer)
        (message "Indented buffer.")))))

(defun nh/format-open-buffers ()
  "Format all open, writable file buffers using `indent-region` and save them.
Skips read-only and special buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      ;; Skip read-only buffers and special buffers, whose names start with * or a space.
      (unless (or buffer-read-only
                  (string-prefix-p " " (buffer-name))
                  (string-prefix-p "*" (buffer-name)))
        (indent-region (point-min) (point-max))
        (when buffer-file-name
          (save-buffer))))))

(defun nh/lisp-modes ()
  "Return a list of major mode symbols for common Lisp dialects and REPLs. Useful for batch operations on all Lisp modes."
  '(cider-repl-mode
    clojure-mode
    clojurec-mode
    clojurescript-mode
    clojurex-mode
    common-lisp-mode
    emacs-lisp-mode
    eshell-mode
    geiser-mode
    geiser-repl-mode
    inf-clojure-mode
    inferior-emacs-lisp-mode
    inferior-lisp-mode
    inferior-scheme-mode
    lisp-interaction-mode
    lisp-mode
    monroe-mode
    racket-mode
    racket-repl-mode
    scheme-interaction-mode
    scheme-mode
    slime-repl-mode
    stumpwm-mode))

(defun nh/mode-hook (mode)
  "Return the hook symbol for a given MODE symbol."
  (intern (concat (symbol-name mode) "-hook")))

(defun nh/lisp-hooks ()
  "Return a list of hook symbols for all Lisp-related modes."
  (mapcar #'nh/mode-hook (nh/lisp-modes)))

(defun nh/rename-current-buffer-file ()
  "Rename the current buffer and the file it is visiting.
Prompts for a new file name, renames the file on disk, and updates the buffer."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defun nh/toggle-window-split ()
  "If there are exactly two windows, toggle between horizontal and vertical split.
Preserves the buffers in each window. Shows a message if not exactly two windows."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))
    (message "You need exactly two windows to toggle the split.")))

(defun nh/rotate-windows-helper (win-list first-buf)
  "Helper for nh/rotate-windows. Rotates buffers in WIN-LIST, putting FIRST-BUF in the last window."
  (if (null (cdr win-list))
      (set-window-buffer (car win-list) first-buf)
    (set-window-buffer (car win-list) (window-buffer (cadr win-list)))
    (nh/rotate-windows-helper (cdr win-list) first-buf)))

(defun nh/rotate-windows ()
  "Rotate the buffers shown in all windows forward by one.
The buffer in the first window moves to the last window."
  (interactive)
  (let ((win-list (window-list)))
    (when (> (length win-list) 1)
      (let ((first-buf (window-buffer (car win-list))))
        (nh/rotate-windows-helper win-list first-buf)
        (select-window (car (last win-list)))))))


(defun nh/explorer-finder ()
  "Open the current file or directory in the system's file explorer.
Supports macOS (Finder) and Windows (Explorer). Uses nh-env for OS detection."
  (interactive)
  (cond
   ((and (boundp 'nh-env/is-mac) nh-env/is-mac (fboundp #'reveal-in-osx-finder))
    (reveal-in-osx-finder))
   ((and (boundp 'nh-env/is-windows) nh-env/is-windows (fboundp #'explorer))
    (explorer))
   (t
    (message "Implement `nh/explorer-finder' for this OS!"))))

(defalias 'explore 'nh/explorer-finder)
(defalias 'finder 'nh/explorer-finder)

(defun nh/whitespace-region-or-buffer-cleanup ()
  "Clean up whitespace in the active region, or the whole buffer if no region is active."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (whitespace-cleanup-region (region-beginning) (region-end))
      (whitespace-cleanup)))
  (message "Cleaned up whitespace in %s."
           (if (region-active-p) "selected region" "buffer")))

(defun nh/indent-offset ()
  "Return the preferred indent offset for the current buffer.
Returns 2 for common web/JS/CSS/TS modes, otherwise 4.
If not in a Projectile project, defaults to 4."
  (cond
   ((member major-mode '(web-mode
                         rjsx-mode
                         json-mode
                         js2-mode
                         js-mode
                         js2-jsx-mode
                         js-jsx-mode
                         css-mode
                         html-mode
                         mhtml-mode
                         typescript-mode))
    2)
   ((not (and (fboundp 'projectile-project-p) (projectile-project-p))) 4)
   (t 4)))

(setq-local js-indent-level (nh/indent-offset))
(setq-local css-indent-offset (nh/indent-offset))

(defun nh/standard-modes ()
  "Return a list of major mode symbols for languages that use braces (C-like, JS, etc)."
  '(c++-mode
    c-mode
    csharp-mode
    css-mode
    elixir-mode
    go-mode
    groovy-mode
    java-mode
    js-mode
    js2-mode
    json-mode
    kotlin-mode
    lua-mode
    mhtml-mode
    objc-mode
    php-mode
    protobuf-mode
    python-mode
    rjsx-mode
    ruby-mode
    rust-mode
    sh-mode
    swift-mode
    typescript-mode
    web-mode))

(defun nh/c-set-c-style (alist)
  "Add ALIST to `c-default-style` after `cc-vars` is loaded.
ALIST should be a cons cell like (major-mode-symbol . \"style-name\")."
  (eval-after-load 'cc-vars
    (lambda () (push alist c-default-style))))

;; Example:
;; (nh/c-set-c-style '(java-mode . "java"))
;; (nh/c-set-c-style '(c-mode . "linux"))

(defun nh/find-file-dwim ()
  "Open a file in a context-aware way.
- In Dired or Dired Sidebar, opens file prompt in the current directory.
- In Magit, opens file prompt in the directory of the file at point (if any).
- Otherwise, just calls `find-file` as usual."
  (interactive)
  (cond
   ((or (eq major-mode 'dired-mode)
        (eq major-mode 'dired-sidebar-mode))
    (let ((default-directory (dired-current-directory)))
      (call-interactively #'find-file)))
   ;; In Magit, start from the folder of the file at point.
   ((derived-mode-p 'magit-mode)
    (if-let ((magit-file (magit-file-at-point)))
        (let ((default-directory
                (file-name-directory
                 (concat (magit-toplevel) magit-file))))
          (call-interactively #'find-file))
      (call-interactively #'find-file)))
   (t
    (call-interactively #'find-file))))

(provide 'nh-helpers)
;;; nh-helpers.el ends here 
