;;; nh-dired.el --- Dired and file management enhancements -*- lexical-binding: t; -*-

;;; Commentary:
;; Modern Dired enhancements and region expansion for Emacs 30+.
;; Provides sidebar, icons, subtree, collapse, and expand-region support.

;;; Code:

;; Dired: Built-in directory editor
;; Emacs built-in file manager providing directory navigation, file operations,
;; and batch processing capabilities with extensive customization options.
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
                           ".#"))) ; Emacs lock files (e.g., .#filename)
  )

;; Dired Sidebar: Tree-style directory sidebar
;; Provides a collapsible tree-style directory browser in a dedicated sidebar,
;; similar to modern IDEs, for convenient project navigation and file management.
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

;; All The Icons Dired: File type icons for dired
;; Adds beautiful file type icons to dired buffers based on file extensions,
;; making it easier to identify different file types at a glance.
;; GitHub: https://github.com/jtbm37/all-the-icons-dired
(use-package all-the-icons-dired
  :ensure t
  :commands (all-the-icons-dired-mode)
  :hook (dired-mode . all-the-icons-dired-mode))

;; Dired Collapse: Collapse single-child directories in Dired
;; GitHub: https://github.com/Fuco1/dired-hacks
(use-package dired-collapse
  :ensure t
  :hook (dired-mode . dired-collapse-mode))

;; Dired Subtree: Tree-style directory expansion
;; Allows expanding directories inline within dired buffers to create a tree
;; view, enabling hierarchical navigation without opening separate buffers.
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
