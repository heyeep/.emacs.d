;;; nh-dired.el --- Dired and file management enhancements -*- lexical-binding: t; -*-

;;; Commentary:
;; Modern Dired enhancements and region expansion for Emacs 30+.
;; Provides sidebar, icons, subtree, collapse, and expand-region support.

;;; Code:

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
                           "eln-*" ; Native compilation files
                           )))
  )

;; Dired Sidebar: File explorer sidebar for Dired
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

;; Pretty icons in Dired buffers
(use-package all-the-icons-dired
  :ensure t
  :commands (all-the-icons-dired-mode)
  :hook (dired-mode . all-the-icons-dired-mode))

;; Dired Collapse: Collapse single-child directories in Dired
(use-package dired-collapse
  :ensure t
  :hook (dired-mode . dired-collapse-mode))

;; Dired Subtree: Expand/collapse directories inline in Dired
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
