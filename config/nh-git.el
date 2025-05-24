;;; nh-git.el --- Git configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for Git-related packages and settings.

;;; Code:

;; Magit: A Git porcelain inside Emacs
(use-package magit
  :ensure t
  :commands (magit-toplevel   ; Show the top-level directory of the current Git repo
               magit-status     ; Open the Magit status buffer for the current repo
               magit-blame      ; Annotate lines in a file with commit info
               magit-log)      ; Show the Git log for the current repo or file
  :config
  (setq magit-rlefresh-status-buffer nil) ; Disable auto-refresh of status buffer
  :bind (("C-x g"   . magit-status)
         ("C-x C-g" . magit-status)))

(provide 'nh-git)
;;; nh-git.el ends here 
