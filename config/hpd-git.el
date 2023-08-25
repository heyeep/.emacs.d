;;;; -*- lexical-binding: t; -*-

(use-package magit
  :ensure t
  :commands (magit-toplevel magit-status magit-blame magit-log)
  :config
  (setq magit-refresh-status-buffer nil)
  (setq magit-completing-read-function 'ivy-completing-read))

;; Don't make backups of files in version control.
(setq vc-make-backup-files nil)

(use-package blamer
  :ensure t
  :bind (("s-i" . blamer-show-commit-info))
  :defer 20
  :custom
  (blamer-idle-time 0)
  (blamer-min-offset 100)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                    :background nil
                    :height 100
                    :italic t)))
  :config
  (global-blamer-mode 1)
  (setq blamer-prettify-time-p t)
  (setq blamer-type 'both)
  (setq blamer-view 'overlay)
  (setq blamer-max-commit-message-length 75))
;; Write backup files to own directory.
;; https://www.emacswiki.org/emacs/BackupDirectory
(defvar +backup-directory (expand-file-name (concat user-emacs-directory "backups"))
  "Location of backup directory.")

(setq backup-directory-alist
      `((".*" . ,+backup-directory)))

;; Purge old backups.
(add-hook 'after-init-hook
          (lambda ()
            (run-with-idle-timer 5 nil #'+delete-backups)))

(defun +delete-backups ()
  "Delete backups."
  (lambda ()
    (let ((week (* 60 60 24 7))
          (current (float-time (current-time))))
      (dolist (file (directory-files +backup-directory t))
        (when (and (backup-file-name-p file)
                   (> (- current (float-time (nth 5 (file-attributes file))))
                      week))
          (message "Deleted backup %s" file)
          (delete-file file))))))

(provide 'hpd-git)
