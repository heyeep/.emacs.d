;;; nh-dired.el --- dired -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Dired: Emacs's built-in file manager.
(use-package dired
  :ensure nil
  :config
  ;; dired-omit-mode lives in dired-x, which is not autoloaded
  (require 'dired-x)
  (add-hook 'dired-mode-hook 'dired-omit-mode)

  (add-hook 'dired-mode-hook 'dired-hide-details-mode)

  (define-key dired-mode-map (kbd "g") 'revert-buffer)

  ;; Patterns are anchored so they match whole names, not any name containing them.
  (setq dired-omit-files
        (rx (or (seq bos "#" (* anything) "#" eos) ; auto-save files
                (seq "~" eos)                      ; backup files
                (seq ".elc" eos)                   ; compiled Emacs Lisp
                (seq bos (or ".#"                  ; lock files
                             ".aider"
                             ".zcompdump"
                             "eln-"))
                (seq bos (or ".DS_Store"
                             ".gitignore"
                             ".gitmodules"
                             ".projectile"
                             ".dir-locals.el"
                             ".smex-items")
                     eos))))
  )

;; dired-sidebar: a file tree in a side window.
;; GitHub: https://github.com/jojojames/dired-sidebar
(use-package dired-sidebar
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :config
  (add-hook 'dired-sidebar-mode-hook 'dired-hide-details-mode)

  ;; Use Emacs's own ls, since macOS ls lacks --group-directories-first.
  (setq dired-sidebar-use-ls-lisp t)

  (setq dired-sidebar-display-header nil)

  (setq dired-sidebar-listing-switches "-la --group-directories-first")

  ;; Clicking a folder expands it in place.
  (setq dired-sidebar-pop-to-sidebar-on-toggle-open nil)
  (setq dired-sidebar-cycle-subtree-on-click t)

  (setq dired-sidebar-width 35)
  (setq dired-sidebar-theme 'icons)

  (setq dired-sidebar-use-custom-font t)
  (setq dired-sidebar-face
        (cond
         ((eq system-type 'darwin)
          '(:family "Inconsolata-g for Powerline" :height 140))
         ((eq system-type 'windows-nt)
          '(:family "Times New Roman" :height 150))
         (:default
          '(:family "Arial" :height 150)))))

;; all-the-icons-dired: file type icons in Dired.
;; GitHub: https://github.com/jtbm37/all-the-icons-dired
(use-package all-the-icons-dired
  :ensure t
  :commands (all-the-icons-dired-mode)
  :hook (dired-mode . all-the-icons-dired-mode))

;; dired-collapse: shows a chain of single-child folders on one line.
;; GitHub: https://github.com/Fuco1/dired-hacks
(use-package dired-collapse
  :ensure t
  :hook (dired-mode . dired-collapse-mode))

;; dired-subtree: expand folders inline with TAB.
;; GitHub: https://github.com/Fuco1/dired-hacks
(use-package dired-subtree
  :ensure t
  :commands (dired-subtree-toggle dired-subtree-cycle)
  :bind (:map dired-mode-map
              ("TAB" . dired-subtree-toggle)
              ("<backtab>" . dired-subtree-cycle))
  :config
  (setq dired-subtree-line-prefix "_ ")
  (setq dired-subtree-use-backgrounds nil))

;; dired-git-info: shows each file's Git status in Dired.
(use-package dired-git-info
  :ensure t
  :after dired
  :bind (:map dired-mode-map
              (")" . dired-git-info-mode))
  :config
  (add-hook 'dired-after-readin-hook #'dired-git-info-auto-enable)

  (setq dgi-auto-hide-details-p nil)

  (defun dgi-commit-message ()
    "Show symbolic status instead of full commit message."
    (let* ((filename (dired-get-filename nil t))
           (status (and filename
                        (dgi--git-status filename))))
      (if status
          (pcase (substring status 0 1)
            ("M" "★ Modified")
            ("A" "✚ Added")
            ("D" "✖ Deleted")
            ("R" "↻ Renamed")
            ("C" "⇒ Copied")
            ("U" "✘ Conflict")
            ("?" "? Untracked")
            (_ status))
        "Not a git file")))

  (setq dired-git-info-format "    %s"))

;; dired-k: colors files in Dired by their Git status.
(use-package dired-k
  :ensure t
  :after dired
  :init
  (setq dired-k-padding 0)
  (setq dired-k-human-readable nil)

  :config
  (setq dired-k-style 'git)

  (add-hook 'dired-initial-position-hook 'dired-k)
  (add-hook 'dired-after-readin-hook 'dired-k-no-revert)

  ;; Disabled: custom colors per Git status.
  ;; (set-face-foreground 'dired-k-modified "red")
  ;; (set-face-foreground 'dired-k-added "green")
  ;; (set-face-foreground 'dired-k-untracked "purple")

  ;; Also mark changed files with a symbol before the name.
  (defun my-dired-k-highlight ()
    "Add Git status indicators using text properties."
    (remove-overlays (point-min) (point-max) 'nh-git-status t)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (dired-move-to-filename)
          (let* ((file (dired-get-filename nil t))
                 (mark (pcase (and file (vc-state file))
                         ('edited "✱ ")
                         ('added "✚ ")
                         ('removed "✖ ")
                         ('unregistered "? "))))
            (when mark
              (let ((overlay (make-overlay (point) (point))))
                (overlay-put overlay 'nh-git-status t)
                (overlay-put overlay 'before-string mark)))))
        (forward-line 1))))

  (add-hook 'dired-mode-hook 'my-dired-k-highlight)
  (add-hook 'dired-after-readin-hook 'my-dired-k-highlight)

  :bind (:map dired-mode-map
              ("g" . dired-k)))

(provide 'nh-dired)

;;; nh-dired.el ends here
