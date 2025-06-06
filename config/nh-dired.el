;;; nh-dired.el --- Dired and file management enhancements -*- lexical-binding: t; -*-

;;; Commentary:
;; Modern Dired enhancements and region expansion for Emacs 30+.
;; Provides sidebar, icons, subtree, collapse, and expand-region support.

;;; Code:

;; Dired: Built-in directory editor
;; Provides file management capabilities within Emacs, allowing you to navigate,
;; manipulate, and organize files and directories with keyboard commands.
(use-package dired
  :ensure nil ; dired is built-in, no need to ensure
  :config
  ;; Enable dired-omit-mode globally to hide unwanted files
  (add-hook 'dired-mode-hook 'dired-omit-mode)

  ;; Configure the regex for files to omit.
  ;; This regex hides Emacs backup files (~), auto-save files (#),
  ;; Emacs lock files (.#), and common ignored files.
  (setq dired-omit-files (rx
                          (or
                           "#" ; Emacs auto-save files (e.g., #filename#)
                           "~" ; Emacs backup files (e.g., filename~)
                           ".#" ; Emacs lock files (e.g., .#filename)
                           ".DS_Store" ; macOS directory metadata
                           ".git" ; Git directory
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

(provide 'nh-dired)

;;; nh-dired.el ends here
