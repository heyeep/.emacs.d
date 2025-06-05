;;; nh-git.el --- Git configuration -*- lexical-binding: t; -*-
(setq debug-on-error t)
;;; Commentary:
;; Configuration for Git-related packages and settings.

;;; Code:

;; We use (require 'transient) here instead of (use-package transient)
;; because we are loading transient from a git submodule, not from ELPA/MELPA.
;; Using use-package with :ensure t would try to install it from the package archives,
;; which we do not want. This ensures we always use our submodule version.
(require 'transient)

;; Magit: A Git porcelain inside Emacs
;; Comprehensive Git interface providing intuitive staging, committing, branching,
;; and history browsing with powerful visual diff and merge conflict resolution.
;; GitHub: https://github.com/magit/magit
(use-package magit
  :ensure t
  :after transient
  :commands (magit-toplevel   ; Show the top-level directory of the current Git repo
               magit-status     ; Open the Magit status buffer for the current repo
               magit-blame      ; Annotate lines in a file with commit info
               magit-log)      ; Show the Git log for the current repo or file
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch))
  :custom
  ;; Show diffs in a separate buffer
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  ;; Highlight word-level changes in diffs
  (magit-diff-refine-hunk t)
   ;; Show more detailed logs
  (magit-log-section-arguments '("--graph" "--color" "--decorate" "-n256"))
  :config
  ;; Use ido for repository selection
  (setq magit-completing-read-function 'magit-ido-completing-read)
  
  ;; Show diff in magit-status buffer
  (setq magit-status-sections-hook
        '(magit-insert-status-headers
          magit-insert-merge-log
          magit-insert-rebase-sequence
          magit-insert-am-sequence
          magit-insert-sequencer-sequence
          magit-insert-bisect-output
          magit-insert-bisect-rest
          magit-insert-bisect-log
          magit-insert-untracked-files
          magit-insert-unstaged-changes
          magit-insert-staged-changes
          magit-insert-stashes
          magit-insert-unpushed-to-pushremote
          magit-insert-unpushed-to-upstream-or-recent
          magit-insert-unpulled-from-pushremote
          magit-insert-unpulled-from-upstream))
  
  ;; Configure Magit performance settings
  (setq magit-revision-show-gravatars nil)  ; Disable gravatars for performance
  (setq git-commit-summary-max-length 50)  ; Enforce commit message length
  
  ;; Magit key customizations
  :bind (:map magit-mode-map
              ("q" . magit-mode-bury-buffer))) ; 'q' to bury magit buffers
  ;; ;; Set default push behavior
  ;; (magit-push-current-set-remote-if-missing t)
  ;; (magit-push-always-verify nil)
  ;; ;; Refresh and performance tweaks
  ;; (magit-refresh-status-buffer nil)
  ;; (magit-process-connection-type nil)
  ;; ;; Show unpulled commits in status buffer
  ;; (add-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream))

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
