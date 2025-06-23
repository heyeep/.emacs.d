;;; nh-dired.el --- dired -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Dired: Built-in directory editor
;; Provides file management capabilities within Emacs, allowing you to navigate,
;; manipulate, and organize files and directories with keyboard commands.
(use-package dired
  :ensure nil ; dired is built-in, no need to ensure
  :config
  ;; Enable dired-omit-mode globally to hide unwanted files
  (add-hook 'dired-mode-hook 'dired-omit-mode)

  ;; Hide details by default for cleaner view
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)

  ;; Shortcut to toggle details view
  (define-key dired-mode-map (kbd "<tab>") 'dired-hide-details-mode)

  ;; Ensure standard dired keybindings are available
  (define-key dired-mode-map (kbd "g") 'revert-buffer)

  ;; Configure the regex for files to omit.
  ;; This regex hides Emacs backup files (~), auto-save files (#),
  ;; Emacs lock files (.#), and common ignored files.
  (setq dired-omit-files (rx
                          (or
                           "#" ; Emacs auto-save files (e.g., #filename#)
                           "~" ; Emacs backup files (e.g., filename~)
                           ".#" ; Emacs lock files (e.g., .#filename)
                           ".DS_Store" ; macOS directory metadata
                           ".gitignore" ; Git ignore file
                           ".gitmodules" ; Git submodules file
                           ".projectile" ; Projectile file
                           ".dir-locals.el" ; Directory local variables
                           ".elc" ; Compiled Emacs Lisp files
                           ".aider*" ; Aider related files/dirs
                                                      ".smex-items"
                                                      ".zcompdump" ;
                                                      "eln-*" ; Native compilation files
                           )))
  )

;; Dired Sidebar: File explorer sidebar for Dired
;; Provides a persistent file tree sidebar using dired, offering quick file
;; navigation and project exploration similar to modern IDE sidebars.
;; GitHub: https://github.com/jojojames/dired-sidebar
(use-package dired-sidebar
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :config
  ;; Hide details by default in the sidebar
  (add-hook 'dired-sidebar-mode-hook 'dired-hide-details-mode)

  ;; Use ls-lisp to avoid issues with different ls versions
  (setq dired-sidebar-use-ls-lisp t)

  ;; Don't show the header in the sidebar
  (setq dired-sidebar-display-header nil)

  ;; Use a minimal listing format for the sidebar
  (setq dired-sidebar-listing-switches "-la --group-directories-first")

  ;; Only show one directory at a time
  (setq dired-sidebar-pop-to-sidebar-on-toggle-open nil)
  (setq dired-sidebar-cycle-subtree-on-click t)

  ;; Make sidebar width adjustable
  (setq dired-sidebar-width 35)
  (setq dired-sidebar-theme 'icons)

  ;; Font settings
  (setq dired-sidebar-use-custom-font t)
  (setq dired-sidebar-face
        (cond
         ((eq system-type 'darwin)
          '(:family "Inconsolata-g for Powerline" :height 140))
         ((eq system-type 'windows-nt)
          '(:family "Times New Roman" :height 150))
         (:default
          '(:family "Arial" :height 150)))))

;; All The Icons Dired: Pretty icons in Dired buffers
;; Adds colorful file type icons to Dired buffers, making it easier to
;; identify different file types at a glance with visual file type indicators.
;; GitHub: https://github.com/jtbm37/all-the-icons-dired
(use-package all-the-icons-dired
  :ensure t
  :commands (all-the-icons-dired-mode)
  :hook (dired-mode . all-the-icons-dired-mode))

;; Dired Collapse: Collapse single-child directories in Dired
;; Automatically collapses single-child directory hierarchies into a single
;; line, reducing visual clutter in deep directory structures.
;; GitHub: https://github.com/Fuco1/dired-hacks
(use-package dired-collapse
  :ensure t
  :hook (dired-mode . dired-collapse-mode))

;; Dired Subtree: Expand/collapse directories inline in Dired
;; Allows expanding and collapsing directory contents inline within the same
;; Dired buffer, providing a tree-like navigation experience.
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

;; Dired Git Info: Show Git status information in Dired
;; Displays the Git status of files directly in the Dired buffer, making it
;; easy to see which files are modified, new, or untracked.
(use-package dired-git-info
  :ensure t
  :after dired
  :bind (:map dired-mode-map
              (")" . dired-git-info-mode))
  :config
  ;; Auto-enable dired-git-info-mode in all Dired buffers
  (add-hook 'dired-after-readin-hook #'dired-git-info-auto-enable)

  ;; Customize the display of Git information
  (setq dgi-auto-hide-details-p nil)  ;; Don't hide details automatically

  ;; Show brief status instead of commit messages
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

;; Git status highlighting for Dired
;; Colors files based on Git status and adds status indicators
(use-package dired-k
  :ensure t
  :after dired
  :init
  ;; Fix potential issues with dired-k initialization
  (setq dired-k-padding 0)
  (setq dired-k-human-readable nil)

  :config
  ;; Enable more vivid colors based on Git status
  (setq dired-k-style 'git)

  ;; Automatically run dired-k when opening dired
  (add-hook 'dired-initial-position-hook 'dired-k)
  (add-hook 'dired-after-readin-hook 'dired-k-no-revert)

  ;; Use different colors for different Git statuses
  ;; (set-face-foreground 'dired-k-modified "red")
  ;; (set-face-foreground 'dired-k-added "green")
  ;; (set-face-foreground 'dired-k-untracked "purple")

  ;; Alternative method to show Git status with icons
  (defun my-dired-k-highlight ()
    "Add Git status indicators using text properties."
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (dired-move-to-filename)
          (let* ((file (dired-get-filename nil t))
                 (status (and file (vc-state file))))
            (when status
              (let ((overlay (make-overlay (point) (+ (point) 1))))
                (overlay-put overlay 'display
                             (pcase status
                               ('edited "✱ ")
                               ('added "✚ ")
                               ('removed "✖ ")
                               ('unregistered "? ")
                               (_ nil)))))))
        (forward-line 1))))

  ;; Add additional hooks for more reliable display
  (add-hook 'dired-mode-hook 'my-dired-k-highlight)
  (add-hook 'dired-after-readin-hook 'my-dired-k-highlight)

  ;; Add keybinding to manually refresh Git status
  :bind (:map dired-mode-map
              ("g" . dired-k)))

(provide 'nh-dired)

;;; nh-dired.el ends here
