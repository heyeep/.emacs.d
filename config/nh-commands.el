;;; nh-commands.el --- Custom shell command macros -*- lexical-binding: t; -*-

;;; Commentary:
;; Defines macros for generating shell command helpers, e.g., for iTerm2.

;;; Code:

;;; Macro: Create a command to run a shell CMD in iTerm2 at the project root or current directory
(defmacro nh/make-iterm-dot-app-command (cmd)
  "Creates a function that runs a terminal CMD in iTerm2.
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

(provide 'nh-commands)
;;; nh-commands.el ends here
