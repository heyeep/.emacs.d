;;; nh-git.el --- Git configuration -*- lexical-binding: t; -*-
(setq debug-on-error t)
;;; Commentary:
;; Configuration for Git-related packages and settings.

;;; Code:

(use-package transient
  :ensure t)

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
         ("C-x C-g" . magit-status))
  :custom
  ;; Show diffs in a separate buffer
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  ;; Highlight word-level changes in diffs
  (magit-diff-refine-hunk 'all)
  ;; Show more detailed logs
  (magit-log-section-arguments '("--graph" "--color" "--decorate" "-n256"))
  ;; Set default push behavior
  (magit-push-current-set-remote-if-missing t)
  (magit-push-always-verify nil)
  ;; Refresh and performance tweaks
  (magit-refresh-status-buffer nil)
  (magit-process-connection-type nil)
  ;; Show unpulled commits in status buffer
  (add-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream))

;; Show Git blame info as a tooltip for the current line using blamer.el
;; (use-package blamer
;;   :ensure t
;;   :bind (("s-i" . blamer-show-commit-info))
;;   :defer 20
;;   :custom
;;   (blamer-idle-time 0)
;;   (blamer-min-offset 100)
;;   :custom-face
;;   (blamer-face ((t :foreground "#7a88cf"
;;                     :background nil
;;                     :height 100
;;                     :italic t)))
;;   :config
;;   (global-blamer-mode 1)
;;   (setq blamer-prettify-time-p t)
;;   (setq blamer-type 'both)
;;   (setq blamer-view 'tooltip)
;;   (setq blamer-max-commit-message-length 75))

(provide 'nh-git)
;;; nh-git.el ends here 
