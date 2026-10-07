;;; nh-org.el --- org -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; htmlize: keeps code colors when Org exports to HTML.
;; GitHub: https://github.com/hniksic/emacs-htmlize
(use-package htmlize
  :ensure t
  :commands (htmlize-buffer htmlize-region htmlize-file htmlize-many-files htmlize-many-files-dired)
  :custom
  (htmlize-output-type 'css)
  (htmlize-html-charset "UTF-8")
  (htmlize-generate-hyperlinks t)
  (htmlize-generate-anchors t))

;; Org: notes, to-do lists and documents in plain text.
;; GitHub: https://github.com/bzg/org-mode
(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :init
  (defun nh/set-up-writing-conditionally ()
    (when (string-equal (buffer-name) "M.org.gpg")
      (turn-on-auto-fill)
      (set-fill-column 80)))

  (defun nh/customize-org-ui ()
    (set-face-attribute 'org-document-title nil :weight 'bold :height 1.4)
    (set-face-attribute 'org-level-1 nil :inherit 'outline-1 :height 1.3 :weight 'bold)
    (set-face-attribute 'org-level-2 nil :inherit 'outline-2 :height 1.2 :weight 'bold)
    (set-face-attribute 'org-level-3 nil :inherit 'outline-3 :height 1.1)
    (set-face-attribute 'org-level-4 nil :inherit 'outline-4 :height 1.0))

  (defun nh/indent-org-block-automatically-or-cycle ()
    "Indent source code in source blocks, otherwise org-cycle."
    (interactive)
    (if (org-in-src-block-p)
        (progn
          (org-edit-special)
          (indent-region (point-min) (point-max))
          (org-edit-src-exit))
      (call-interactively #'org-cycle)))

  (add-hook 'org-mode-hook #'nh/set-up-writing-conditionally)
  (add-hook 'org-mode-hook #'nh/customize-org-ui)
  :config
  (require 'ox-odt nil t)
  (require 'ob-dot nil t)
  (require 'org-notmuch nil t)
  (add-hook 'org-babel-after-execute-hook #'org-redisplay-inline-images)
  (customize-set-variable 'org-src-fontify-natively t)
  (customize-set-variable 'org-src-preserve-indentation nil)
  (customize-set-variable 'org-edit-src-content-indentation 0)
  (customize-set-variable 'org-src-tab-acts-natively t)
  (customize-set-variable 'org-src-window-setup 'current-window)
  (customize-set-variable 'org-src-strip-leading-and-trailing-blank-lines t)
  (customize-set-variable 'org-src-ask-before-returning-to-edit-buffer nil)
  (customize-set-variable 'org-export-backends '(ascii html icalendar latex md))

  (add-to-list 'org-structure-template-alist '("s" . "src"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("js" . "src javascript"))
  (add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
  (add-to-list 'org-structure-template-alist '("html" . "src html"))
  (add-to-list 'org-structure-template-alist '("css" . "src css"))
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("sql" . "src sql"))
  (add-to-list 'org-structure-template-alist '("r" . "src R"))
  (add-to-list 'org-structure-template-alist '("rb" . "src ruby"))
  (add-to-list 'org-structure-template-alist '("hs" . "src haskell"))
  (add-to-list 'org-structure-template-alist '("cl" . "src common-lisp"))
  (add-to-list 'org-structure-template-alist '("scm" . "src scheme"))
  (add-to-list 'org-structure-template-alist '("lsp" . "src lisp"))
  (add-to-list 'org-structure-template-alist '("dot" . "src dot"))
  (add-to-list 'org-structure-template-alist '("plant" . "src plantuml"))
  (add-to-list 'org-structure-template-alist '("mermaid" . "src mermaid"))

  (customize-set-variable 'org-latex-default-class "article")
  (customize-set-variable 'org-latex-packages-alist
   '(;; Graphics and figures
     ("" "graphicx" t)        ;; Needed for \includegraphics
     ("" "longtable" nil)     ;; Tables that can span multiple pages
     ("" "wrapfig" nil)       ;; Wrap text around figures
     ("" "rotating" nil)      ;; Rotate tables and figures

     ;; Text formatting and typography
     ("normalem" "ulem" t)    ;; Underline and strike-through text
     ("" "amsmath" t)         ;; Math layout
     ("" "textcomp" t)        ;; Additional text symbols
     ("" "amssymb" t)         ;; Additional math symbols
     ("" "capt-of" nil)       ;; Captions for non-floating environments
     ("" "hyperref" nil)      ;; Hyperlinks and PDF metadata
     ("" "xcolor" t)          ;; Colors

     ;; Code and verbatim text
     ("" "listings" t)        ;; Source code listings with syntax highlighting
     ("" "fancyvrb" t)        ;; Verbatim text with more options

     ;; Tables and typography
     ("" "booktabs" t)        ;; Cleaner table rules
     ("" "microtype" t)       ;; Better spacing and line breaks
     ("" "geometry" t)        ;; Page layout and margins

     ;; Language and typography
     ("" "babel" t)           ;; Language-specific typography
     ("" "csquotes" t)        ;; Context-sensitive quotation marks

     ;; Bibliography
     ("" "natbib" t)          ;; Citation management
     ("" "biblatex" t)))      ;; Bibliographies

  ;; XeLaTeX handles Unicode and system fonts. Three passes resolve references and citations.
  (customize-set-variable 'org-latex-pdf-process
   '("xelatex -interaction nonstopmode -output-directory %o %f"
     "xelatex -interaction nonstopmode -output-directory %o %f"
     "xelatex -interaction nonstopmode -output-directory %o %f"))

  (org-babel-do-load-languages
   'org-babel-load-languages '(
                               (awk . t)
                               (calc . t)
                               (C . t)
                               (emacs-lisp . t)
                               (haskell . t)
                               (gnuplot . t)
                               (latex . t)
                               (java . t)
                               (js . t)
                               (perl . t)
                               (python . t)
                               (R . t)
                               (ruby . t)
                               (scheme . t)
                               (shell . t)
                               (sql . t)))

  (customize-set-variable 'org-src-block-faces
   '(("emacs-lisp" (:background "#f8f8f8" :extend t))
     ("python" (:background "#f8f8f8" :extend t))
     ("js" (:background "#f8f8f8" :extend t))
     ("typescript" (:background "#f8f8f8" :extend t))
     ("html" (:background "#f8f8f8" :extend t))
     ("css" (:background "#f8f8f8" :extend t))
     ("shell" (:background "#f8f8f8" :extend t)))))

;; org-bullets: shows heading stars as Unicode bullets.
;; GitHub: https://github.com/emacsorphanage/org-bullets
(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

;; org-modern: restyles Org headings, tables and blocks.
;; GitHub: https://github.com/minad/org-modern
(use-package org-modern
  :ensure t
  :hook (org-mode . org-modern-mode))

;; Use a proportional font for Org prose.
(add-hook 'org-mode-hook #'variable-pitch-mode)

;; org-download: drag images into Org files; it saves them and inserts a link.
;; GitHub: https://github.com/abo-abo/org-download
(use-package org-download
  :ensure t
  :hook (org-mode . org-download-enable))

(add-hook 'org-mode-hook #'org-indent-mode)

;; graphviz-dot-mode: edit and preview Graphviz .dot and .gv files.
(use-package graphviz-dot-mode
  :ensure t
  :mode (("\\.dot\\'" . graphviz-dot-mode)
         ("\\.gv\\'" . graphviz-dot-mode))
  :init
  (setq default-tab-width 4))

;; Org-roam: linked notes, where each note is a node in a graph.

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-directory "~/org/roam")
  :custom
  (org-roam-completion-everywhere t)
  (org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+date: %U\n\n")
      :unnarrowed t)
     ("p" "project" plain
      "* Goals\n%?\n\n* Tasks\n** TODO Add initial tasks\n\n* Notes\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+filetags: Project\n#+date: %U\n\n")
      :unnarrowed t)))
  (org-roam-file-extensions '("org"))
  (org-roam-db-node-include-function (lambda () (not (org-in-commented-heading-p))))
  (org-roam-db-location (expand-file-name "org-roam.db" org-roam-directory))
  (org-roam-db-autosync t)
  (org-roam-db-autosync-delay 0.5)
  (org-roam-db-cache-dir (expand-file-name "cache" org-roam-directory))
  (org-roam-db-cache-enabled t)
  (org-roam-db-cache-ttl 3600)
  (org-roam-db-cache-max-size 1000000)
  (org-roam-db-cache-cleanup-interval 3600)
  (org-roam-db-gc-threshold most-positive-fixnum)
  (org-roam-db-update-method 'immediate)
  (org-roam-db-update-on-save t)
  (org-roam-protocol-store-links t)
  (org-roam-protocol-store-html t)
  (org-roam-protocol-store-images t)
  (org-roam-protocol-capture-templates
   '(("r" "ref" plain
      "* ${title}\n:PROPERTIES:\n:ROAM_REFS: ${ref}\n:END:\n\n%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+filetags: :reference:\n#+date: %U\n\n")
      :unnarrowed t)
     ("w" "web" plain
      "* ${title}\n:PROPERTIES:\n:ROAM_REFS: ${ref}\n:END:\n\n%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+filetags: :web:\n#+date: %U\n\n")
      :unnarrowed t)
     ("e" "email" plain
      "* ${title}\n:PROPERTIES:\n:ROAM_REFS: ${ref}\n:END:\n\n%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+filetags: :email:\n#+date: %U\n\n")
      :unnarrowed t)
     ("t" "tweet" plain
      "* ${title}\n:PROPERTIES:\n:ROAM_REFS: ${ref}\n:END:\n\n%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+filetags: :tweet:\n#+date: %U\n\n")
      :unnarrowed t)))
  (org-roam-dailies-directory "daily/")
  (org-roam-dailies-capture-today-format "%Y-%m-%d")
  (org-roam-dailies-find-date-format "%Y-%m-%d")
  (org-roam-dailies-capture-templates
   '(("d" "default" entry
      "* %?"
      :if-new (file+head "%<%Y-%m-%d>.org"
                         "#+title: %<%Y-%m-%d>\n#+filetags: :daily:\n#+date: %U\n\n")
      :unnarrowed t)
     ("j" "journal" entry
      "* Journal\n%?\n\n* Tasks\n** TODO \n\n* Notes\n\n"
      :if-new (file+head "%<%Y-%m-%d>.org"
                         "#+title: %<%Y-%m-%d>\n#+filetags: :daily:journal:\n#+date: %U\n\n")
      :unnarrowed t)
     ("w" "work" entry
      "* Work Log\n%?\n\n* Meetings\n\n* Tasks\n** TODO \n\n* Notes\n\n"
      :if-new (file+head "%<%Y-%m-%d>.org"
                         "#+title: %<%Y-%m-%d>\n#+filetags: :daily:work:\n#+date: %U\n\n")
      :unnarrowed t)))
  (org-roam-dailies-capture-today t)
  (org-roam-dailies-capture-yesterday t)
  (org-roam-dailies-capture-tomorrow t)
  (org-roam-migrate-auto-backup t)
  (org-roam-migrate-backup-directory "~/org/roam/backups/")
  (org-roam-migrate-file-naming-scheme 'title)
  (org-roam-migrate-file-extension ".org")
  (org-roam-migrate-link-style 'wiki)
  (org-roam-migrate-link-format "[[%s]]")
  (org-roam-migrate-tag-style 'org)
  (org-roam-migrate-tag-format ":%s:")
  :bind
  (("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n c" . org-roam-capture)
   ("C-c n j" . org-roam-dailies-capture-today)
   ("C-c n y" . org-roam-dailies-capture-yesterday)
   ("C-c n t" . org-roam-dailies-capture-tomorrow)
   ("C-c n d" . org-roam-dailies-find-date)
   ("C-c n p" . org-roam-dailies-find-previous-note)
   ("C-c n n" . org-roam-dailies-find-next-note)
   ("C-c n m" . org-roam-migrate-wizard))
  :config
  (org-roam-db-autosync-mode))

;; org-roam-ui: browse the note graph in a web browser.
(use-package org-roam-ui
  :ensure t
  :after org-roam
  :custom
  (org-roam-ui-port 35901)
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-node-display-template "${title:100}")
  :bind
  (("C-c n g" . org-roam-ui-mode)))

;; PDF Tools: view and annotate PDFs inside Emacs.
(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install)
  :custom
  (pdf-view-display-size 'fit-page)
  (pdf-view-use-scaling t)
  (pdf-view-use-imagemagick t)
  (pdf-isearch-minor-mode t)
  (pdf-view-auto-slice-minor-mode t)
  (pdf-view-midnight-minor-mode t)
  (pdf-view-printer-minor-mode t)
  (pdf-annot-activate-created-annotations t)
  (pdf-annot-minor-mode t)
  (pdf-links-minor-mode t)
  (pdf-outline-minor-mode t)
  :bind
  (:map pdf-view-mode-map
   ("C-c C-p" . pdf-view-scroll-up-or-next-page)
   ("C-c C-n" . pdf-view-scroll-down-or-previous-page)
   ("C-c C-f" . pdf-view-fit-page-to-window)
   ("C-c C-w" . pdf-view-fit-width-to-window)
   ("C-c C-m" . pdf-view-midnight-minor-mode)
   ("C-c C-a" . pdf-annot-add-annotation)
   ("C-c C-l" . pdf-links-action-perform)
   ("C-c C-o" . pdf-outline)))

;; org-roam-timestamps: records when notes are created and changed.
(use-package org-roam-timestamps
  :ensure t
  :after org-roam
  :custom
  (org-roam-timestamps-properties
   '("CREATED" "MODIFIED" "ACCESSED" "REVIEWED" "PUBLISHED" "ARCHIVED"))
  (org-roam-timestamps-format "%Y-%m-%d %H:%M:%S")
  (org-roam-timestamps-update-on-save t)
  (org-roam-timestamps-update-on-access t)
  (org-roam-timestamps-update-on-create t)
  (org-roam-timestamps-property-location 'head)
  (org-roam-timestamps-property-position 'top)
  (org-roam-timestamps-templates
   '(("CREATED" . "Created: %s by %u")
     ("MODIFIED" . "Last modified: %s by %u")
     ("ACCESSED" . "Last accessed: %s by %u")
     ("REVIEWED" . "Last reviewed: %s by %u")
     ("PUBLISHED" . "Published: %s by %u")
     ("ARCHIVED" . "Archived: %s by %u")))
  :config
  (org-roam-timestamps-mode))

(provide 'nh-org)
;;; nh-org.el ends here
