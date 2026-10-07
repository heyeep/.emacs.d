;;; nh-commands.el --- commands -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)

(defun nh/open-shell ()
  "Open an Emacs shell appropriate for the OS and available packages.
Prefers vterm, then multi-term, then ansi-term, then eshell."
  (interactive)
  (cond
   ((or (eq system-type 'darwin) (eq system-type 'gnu/linux))
    (cond
     ((fboundp 'vterm) (vterm))
     ((fboundp 'multi-term) (multi-term))
     ((fboundp 'ansi-term) (ansi-term (getenv "SHELL")))
     (t (eshell))))
   ((eq system-type 'windows-nt)
    (eshell))
   (t
    (message "Implement `nh/open-shell' for this OS!"))))

(defun nh/open-terminal ()
  "Open the system terminal in the current directory.
On macOS, prefers iTerm2, then Terminal.app. On Linux, tries common terminal emulators. On Windows, opens cmd.exe."
  (interactive)
  (cond
   ((eq system-type 'darwin)
    (let ((dir (expand-file-name default-directory)))
      (cond
       ((eq 0 (call-process "open" nil nil nil "-b" "com.googlecode.iterm2" dir)))
       ((eq 0 (call-process "open" nil nil nil "-b" "com.apple.terminal" dir)))
       (t (message "Could not open iTerm2 or Terminal.app.")))))
   ((eq system-type 'windows-nt)
    (let ((proc (start-process "cmd" nil "cmd.exe" "/C" "start" "cmd.exe")))
      (set-process-query-on-exit-flag proc nil)))
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

(defalias 'terminal 'nh/open-terminal)
(defalias 'zterm 'nh/open-shell)

(defun nh/recentf-dwim ()
  "Open a recent file using the best available completion framework.
Prefers Helm, then vanilla Emacs."
  (interactive)
  (cond
   ((bound-and-true-p helm-mode)
    (helm-recentf))
   (t
    (recentf-open-files))))

(defun nh/buffers-dwim ()
  "Switch to another buffer using the best available completion framework.
Prefers Helm, then vanilla Emacs."
  (interactive)
  (cond
   ((bound-and-true-p helm-mode)
    (helm-buffers-list))
   (t
    (call-interactively #'switch-to-buffer))))

(defun nh/save-all-buffers ()
  "Save all modified buffers without prompting for confirmation.
Uses `save-some-buffers' with the :all-buffers-no-confirm keyword."
  (interactive)
  (save-some-buffers :all-buffers-no-confirm))

(defun nh/get-buffers-matching-mode (mode)
  "Return a list of buffers whose `major-mode` is MODE."
  (cl-loop for buf in (buffer-list)
           when (with-current-buffer buf (eq major-mode mode))
           collect buf))

(defun nh/multi-occur-in-mode (&optional mode)
  "Show all lines matching a regexp in buffers with the given MODE.
If called interactively with a prefix argument, prompt for the mode.
Otherwise, use the current buffer's major mode."
  (interactive
   (list (if current-prefix-arg
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
  (when (fboundp 'dired-sidebar-toggle-sidebar)
    (dired-sidebar-toggle-sidebar))
  (when (fboundp 'ibuffer-sidebar-toggle-sidebar)
    (ibuffer-sidebar-toggle-sidebar)))

(defun nh/newline-or-indent-new-comment-line ()
  "If in a comment line, call the function bound to M-j, else insert a newline."
  (interactive)
  ;; Element 4 of syntax-ppss is non-nil inside a comment.
  (if (and (nth 4 (syntax-ppss))
           ;; Also require the line itself to start inside the comment.
           (save-excursion
             (beginning-of-line-text)
             (nth 4 (syntax-ppss))))
      (let ((fn (key-binding (kbd "M-j"))))
        ;; M-j continues the comment on a new line in most modes.
        (if fn
            (call-interactively fn)
          (newline)))
    (newline)))

(defun nh/bind-newline-or-indent-new-comment-line ()
  "Bind RET to `nh/newline-or-indent-new-comment-line' in the current buffer."
  (local-set-key (kbd "RET") #'nh/newline-or-indent-new-comment-line))

(add-hook 'prog-mode-hook #'nh/bind-newline-or-indent-new-comment-line)

(defun nh/symbol-at-point ()
  "For search commands, this lets you prefill the minibuffer with the most relevant text."
  (cond
   ((use-region-p)
    (buffer-substring-no-properties (region-beginning) (region-end)))
   ((symbol-at-point)
    (substring-no-properties (symbol-name (symbol-at-point))))
   (t nil)))

(defun nh/counsel-rg ()
  "Quickly search for the word under the cursor or the selected text in the current project."
  (interactive)
  (counsel-rg (nh/symbol-at-point)))

(defun nh/counsel-ag ()
  "Quickly search for the word under the cursor or the selected text in the current project using ag."
  (interactive)
  (counsel-ag (nh/symbol-at-point)))

(defun nh/fill-region-and-comment ()
  "Use case: For code review or documentation, you can quickly reformat and comment a block of text."
  (interactive)
  (let* ((old-fill-column fill-column)
         (fill-column (- old-fill-column 2)))
    (call-interactively #'fill-region)
    (call-interactively #'comment-region)))

(provide 'nh-commands)
;;; nh-commands.el ends here
