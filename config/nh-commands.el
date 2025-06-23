;;; nh-commands.el --- commands -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)

;;; Macro: Create a command to run a shell CMD in iTerm2 at the project root or current directory
(defmacro nh/make-iterm-dot-app-command (cmd)
  "Create a function that run a terminal CMD in iTerm2.
The function will be named nh/iterm-<cmd-with-dashes>."
  (let* ((fn-name (concat "nh/iterm-"
                         (replace-regexp-in-string "[^a-zA-Z0-9]+" "-" (string-trim cmd))))
         (sym (intern fn-name)))
    `(defun ,sym ()
       ,(concat "Run '" cmd "' in iTerm2 at the project root or current directory.")
       (interactive)
       (let* ((dir (if (and (fboundp 'projectile-project-p) (projectile-project-p))
                       (projectile-project-root)
                     default-directory))
              (script (format
"tell application \"iTerm\"
  activate
  try
    set newWindow to (create window with default profile)
    tell current session of newWindow
      write text \"cd %s; %s\"
    end tell
  on error
    beep
  end try
end tell"
                       (replace-regexp-in-string "\"" "\\\"" dir)
                       (replace-regexp-in-string "\"" "\\\"" ,cmd))))
         (call-process "osascript" nil 0 nil "-e" script)))))

;; Open an Emacs shell appropriate for the OS and available packages
(defun nh/open-shell ()
  "Open an Emacs shell appropriate for the OS and available packages.
Prefers vterm, then multi-term, then ansi-term, then eshell."
  (interactive)
  (cond
   ;; On macOS or Linux, prefer vterm, then multi-term, then ansi-term
   ((or (eq system-type 'darwin) (eq system-type 'gnu/linux))
    (cond
     ((fboundp 'vterm) (vterm))
     ((fboundp 'multi-term) (multi-term))
     ((fboundp 'ansi-term) (ansi-term (getenv "SHELL")))
     (t (eshell))))
   ;; On Windows, use eshell
   ((eq system-type 'windows-nt)
    (eshell))
   (t
    (message "Implement `nh/open-shell' for this OS!"))))

;; Open the system terminal in the current directory, preferring iTerm2 on macOS
(defun nh/open-terminal ()
  "Open the system terminal in the current directory.
On macOS, prefers iTerm2, then Terminal.app. On Linux, tries common terminal emulators. On Windows, opens cmd.exe."
  (interactive)
  (cond
   ;; macOS: Try iTerm2, then Terminal.app
   ((eq system-type 'darwin)
    (let ((dir (shell-quote-argument default-directory)))
      (cond
       ;; Try iTerm2
       ((eq 0 (call-process "open" nil nil nil "-b" "com.googlecode.iterm2" dir)))
       ;; Fallback to Terminal.app
       ((eq 0 (call-process "open" nil nil nil "-b" "com.apple.terminal" dir)))
       (t (message "Could not open iTerm2 or Terminal.app.")))))
   ;; Windows: Open cmd.exe
   ((eq system-type 'windows-nt)
    (let ((proc (start-process "cmd" nil "cmd.exe" "/C" "start" "cmd.exe")))
      (set-process-query-on-exit-flag proc nil)))
   ;; Linux: Try common terminal emulators
   ((eq system-type 'gnu/linux)
    (let ((term (or (executable-find "gnome-terminal")
                    (executable-find "konsole")
                    (executable-find "x-terminal-emulator")
                    (executable-find "xterm"))))
      (if term
          (start-process "terminal" nil term "--working-directory" default-directory)
        (message "No known terminal emulator found!"))))
   (t
    (message "Implement `nh/open-terminal' for this OS!"))))

;; defalias creates an alternative name for nh/open-terminal
(defalias 'terminal 'nh/open-terminal)
;; defalias creates an alternative name for nh/open-shell
(defalias 'zterm 'nh/open-shell)

(defun nh/recentf-dwim ()
  "Open a recent file using the best available completion framework.
Prefers Helm, then vanilla Emacs."
  (interactive)
  (cond
   ;; If Helm is active, use helm-recentf
   ((bound-and-true-p helm-mode)
    (helm-recentf))
   (t
    (recentf-open-files))))

(defun nh/buffers-dwim ()
  "Switch to another buffer using the best available completion framework.
Prefers Helm, then vanilla Emacs."
  (interactive)
  (cond
   ;; If Helm is active, use helm-buffers-list
   ((bound-and-true-p helm-mode)
    (helm-buffers-list))
   (t
    (call-interactively #'switch-to-buffer))))

(defun nh/save-all-buffers ()
  "Save all modified buffers without prompting for confirmation.
Uses `save-some-buffers' with the :all-buffers-no-confirm keyword."
  (interactive)
  (save-some-buffers :all-buffers-no-confirm))

;; Return a list of buffers whose major mode matches MODE
(defun nh/get-buffers-matching-mode (mode)
  "Return a list of buffers whose `major-mode` is MODE."
  (cl-loop for buf in (buffer-list)
           when (with-current-buffer buf (eq major-mode mode))
           collect buf))

;; Run multi-occur on all buffers with the same (or chosen) major mode
(defun nh/multi-occur-in-mode (&optional mode)
  "Show all lines matching a regexp in buffers with the given MODE.
If called interactively with a prefix argument, prompt for the mode.
Otherwise, use the current buffer's major mode."
  (interactive
   (list (if current-prefix-arg
             ;; Prompt for a mode if prefix arg is given
             (intern (completing-read
                      "Mode: "
                      (delete-dups
                       (mapcar (lambda (buf)
                                 (buffer-local-value 'major-mode buf))
                               (buffer-list)))
                      nil t))
           major-mode)))
  (let ((buffers (nh/get-buffers-matching-mode mode)))
    (if buffers
        (multi-occur buffers (car (occur-read-primary-args)))
      (message "No buffers found with mode: %s" mode))))

(defun nh/buffer-contains-string-p (string)
  "Return non-nil if the current buffer contain STRING.
Preserves point, mark, and match data."
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (search-forward string nil t))))

(defun nh/buffer-contains-regex-p (regex)
  "Return non-nil if the current buffer contain a match for REGEX.
Preserves point, mark, and match data."
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (re-search-forward regex nil t))))

(defun nh/paste-column ()
  "Paste a column (rectangle) of text at point.
If Evil mode is active, use register 0; otherwise, use the most recent kill."
  (interactive)
  (insert-rectangle
   (split-string
    (if (bound-and-true-p evil-mode)
        (evil-get-register ?0)
      (current-kill 0 t))
    "[\r]?\n")))

(defun nh/sidebar-toggle ()
  "Toggle both Dired and Ibuffer sidebars, if available."
  (interactive)
  ;; Toggle Dired sidebar if available
  (when (fboundp 'dired-sidebar-toggle-sidebar)
    (dired-sidebar-toggle-sidebar))
  ;; Toggle Ibuffer sidebar if available
  (when (fboundp 'ibuffer-sidebar-toggle-sidebar)
    (ibuffer-sidebar-toggle-sidebar)))

;; Context-sensitive RET binding for comments in programming modes
(defun nh/newline-or-indent-new-comment-line ()
  "If in a comment line, call the function bound to M-j, else insert a newline."
  (interactive)
  ;; Check if point is inside a comment (nth 4 from syntax-ppss is non-nil)
  (if (and (nth 4 (syntax-ppss))
           ;; Also check if the beginning of the line is inside a comment
           (save-excursion
             (beginning-of-line-text)
             (nth 4 (syntax-ppss))))
      (let ((fn (key-binding (kbd "M-j"))))
        ;; If M-j is bound to a function (usually c-indent-new-comment-line), call it
        (if fn
            (call-interactively fn)
          ;; If not, just insert a newline
          (newline)))
    ;; If not in a comment line, insert a newline as usual
    (newline)))

(defun nh/bind-newline-or-indent-new-comment-line ()
  "Bind RET to `nh/newline-or-indent-new-comment-line' in the current buffer."
  ;; This makes RET context-sensitive for comments in this buffer only.
  ;; Use local-set-key so the binding is buffer-local and doesn't affect other buffers.
  (local-set-key (kbd "RET") #'nh/newline-or-indent-new-comment-line))

;; Add the context-sensitive RET binding to all programming modes.
(add-hook 'prog-mode-hook #'nh/bind-newline-or-indent-new-comment-line)

;; DWIM: Get the active region as a string, or the symbol at point if no region is active
(defun nh/symbol-at-point ()
  "For search commands, this lets you prefill the minibuffer with the most relevant text."
  (cond
   ;; If a region is active, return its contents as a string
   ((use-region-p)
    (buffer-substring-no-properties (region-beginning) (region-end)))
   ;; Otherwise, if there's a symbol at point, return it as a string
   ((symbol-at-point)
    (substring-no-properties (symbol-name (symbol-at-point))))
   ;; If neither, return nil
   (t nil)))

;; Call counsel-rg with the symbol at point or active region as initial input
(defun nh/counsel-rg ()
  "Quickly search for the word under the cursor or the selected text in the current project."
  (interactive)
  (counsel-rg (nh/symbol-at-point)))

;; Call counsel-ag with the symbol at point or active region as initial input
(defun nh/counsel-ag ()
  "Quickly search for the word under the cursor or the selected text in the current project using ag."
  (interactive)
  (counsel-ag (nh/symbol-at-point)))

;; Fill and comment a region, using a slightly smaller fill column
(defun nh/fill-region-and-comment ()
  "Use case: For code review or documentation, you can quickly reformat and comment a block of text."
  (interactive)
  (let* ((old-fill-column fill-column)
         (fill-column (- old-fill-column 2)))
    ;; Fill the region
    (call-interactively #'fill-region)
    ;; Then comment the region
    (call-interactively #'comment-region)))

(provide 'nh-commands)
;;; nh-commands.el ends here
