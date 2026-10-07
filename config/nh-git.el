;;; nh-git.el --- git -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Transient comes from a git submodule. use-package with :ensure would
;; install the package-archive copy instead, so require it directly.
(require 'transient)

;; Magit: a full Git interface inside Emacs.
;; GitHub: https://github.com/magit/magit
(use-package magit
  :ensure t
  :after transient
  :commands (magit-toplevel
               magit-status
               magit-blame
               magit-log)
  :bind (("C-x g"   . magit-status)
         ("C-x C-g" . magit-status))
  :custom
  ;; Open Magit in the current window, but show diffs in another one.
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  ;; Highlight changed words in every hunk, not just the selected one.
  (magit-diff-refine-hunk 'all)
  (magit-log-section-arguments '("--graph" "--color" "--decorate" "-n256"))
  :config
  ;; Newer Magit removed this function, so drop it from the status sections.
  (when (boundp 'magit-status-sections-hook)
    (remove-hook 'magit-status-sections-hook 'magit-insert-bisect-output)))
  ;; ;; Set default push behavior
  ;; (magit-push-current-set-remote-if-missing t)
  ;; (magit-push-always-verify nil)
  ;; ;; Refresh and performance tweaks
  ;; (magit-refresh-status-buffer nil)
  ;; (magit-process-connection-type nil)
  ;; ;; Show unpulled commits in status buffer
  ;; (add-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream))

;; Disabled: blamer shows Git blame for the current line in a tooltip.
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
