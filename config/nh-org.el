;;; nh-org.el --- org -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Htmlize: Export Org buffers to HTML with syntax highlighting
;; Converts Emacs buffers to HTML while preserving font-lock syntax highlighting,
;; essential for exporting Org mode documents with properly styled code blocks.
;; GitHub: https://github.com/hniksic/emacs-htmlize
(use-package htmlize
  :ensure t
  :commands (htmlize-buffer htmlize-region htmlize-file htmlize-many-files htmlize-many-files-dired)
  :custom
  (htmlize-output-type 'css)
  (htmlize-html-charset "UTF-8")
  (htmlize-generate-hyperlinks t)
  (htmlize-generate-anchors t))

;; Org: Outline-based notes management and organizer
;; Provides a comprehensive system for notes, planning, and authoring with
;; support for TODO lists, scheduling, code execution, and document export.
;; GitHub: https://github.com/bzg/org-mode
(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :init
  ;; Set up writing enhancements for encrypted org files (like M.org.gpg)
  (defun nh/set-up-writing-conditionally ()
    ;; If the buffer is "M.org.gpg", enable auto-fill and set fill column
    (when (string-equal (buffer-name) "M.org.gpg")
      (turn-on-auto-fill)
      (set-fill-column 80)))

  ;; Customize the appearance of Org headings and document title for readability
  (defun nh/customize-org-ui ()
    (set-face-attribute 'org-document-title nil :weight 'bold :height 1.4)
    (set-face-attribute 'org-level-1 nil :inherit 'outline-1 :height 1.3 :weight 'bold)
    (set-face-attribute 'org-level-2 nil :inherit 'outline-2 :height 1.2 :weight 'bold)
    (set-face-attribute 'org-level-3 nil :inherit 'outline-3 :height 1.1)
    (set-face-attribute 'org-level-4 nil :inherit 'outline-4 :height 1.0))

  ;; Improve source block editing: indent code or cycle org structure
  (defun nh/indent-org-block-automatically-or-cycle ()
    "Indent source code in source blocks, otherwise org-cycle."
    (interactive)
    (if (org-in-src-block-p)
        (progn
          (org-edit-special)
          (indent-region (point-min) (point-max))
          (org-edit-src-exit))
      (call-interactively #'org-cycle)))

  ;; Add hooks for writing and UI customization
  (add-hook 'org-mode-hook #'nh/set-up-writing-conditionally)
  (add-hook 'org-mode-hook #'nh/customize-org-ui)
  :config
  ;; Enable ODT export (Open Document Text)
  (require 'ox-odt nil t)
  ;; Enable Graphviz DOT support in org-babel
  (require 'ob-dot nil t)
  ;; Enable notmuch email integration in Org
  (require 'org-notmuch nil t)
  ;; Automatically redisplay inline images after executing code blocks
  (add-hook 'org-babel-after-execute-hook #'org-redisplay-inline-images)
  ;; Org source block and export settings
  (customize-set-variable 'org-src-fontify-natively t)      ;; Syntax highlight code in src blocks
  (customize-set-variable 'org-src-preserve-indentation nil)
  (customize-set-variable 'org-edit-src-content-indentation 0)
  (customize-set-variable 'org-src-tab-acts-natively t)     ;; TAB acts as expected in src blocks
  (customize-set-variable 'org-src-window-setup 'current-window)  ;; Edit in current window
  (customize-set-variable 'org-src-strip-leading-and-trailing-blank-lines t)  ;; Clean up blank lines
  (customize-set-variable 'org-src-ask-before-returning-to-edit-buffer nil)  ;; Don't ask before returning
  (customize-set-variable 'org-export-backends '(ascii html icalendar latex md))

  ;; Source block templates
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

  ;; LaTeX configuration
  ;; Set the default document class for LaTeX export
  (customize-set-variable 'org-latex-default-class "article")
  ;; Configure LaTeX packages with detailed explanations
  (customize-set-variable 'org-latex-packages-alist
   '(;; Graphics and figures
     ("" "graphicx" t)        ;; Enhanced graphics support, required for \includegraphics
     ("" "longtable" nil)     ;; Tables that can span multiple pages
     ("" "wrapfig" nil)       ;; Wrap text around figures
     ("" "rotating" nil)      ;; Rotate tables and figures

     ;; Text formatting and typography
     ("normalem" "ulem" t)    ;; Underline and strike-through text
     ("" "amsmath" t)         ;; Advanced math formatting
     ("" "textcomp" t)        ;; Additional text symbols
     ("" "amssymb" t)         ;; Additional math symbols
     ("" "capt-of" nil)       ;; Captions for non-floating environments
     ("" "hyperref" nil)      ;; Hyperlinks and PDF metadata
     ("" "xcolor" t)          ;; Extended color support

     ;; Code and verbatim text
     ("" "listings" t)        ;; Source code listings with syntax highlighting
     ("" "fancyvrb" t)        ;; Enhanced verbatim text

     ;; Tables and typography
     ("" "booktabs" t)        ;; Professional-looking tables
     ("" "microtype" t)       ;; Typographic refinements
     ("" "geometry" t)        ;; Page layout and margins

     ;; Language and typography
     ("" "babel" t)           ;; Language-specific typography
     ("" "csquotes" t)        ;; Context-sensitive quotation marks

     ;; Bibliography
     ("" "natbib" t)          ;; Citation management
     ("" "biblatex" t)))      ;; Modern bibliography management

  ;; Configure the PDF compilation process
  ;; Using XeLaTeX for better Unicode and font support
  (customize-set-variable 'org-latex-pdf-process
   '("xelatex -interaction nonstopmode -output-directory %o %f"  ;; First pass
     "xelatex -interaction nonstopmode -output-directory %o %f"  ;; Second pass for references
     "xelatex -interaction nonstopmode -output-directory %o %f")) ;; Third pass for citations

  ;; Enable more languages for code blocks (babel)
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

  ;; Code block appearance
  (customize-set-variable 'org-src-block-faces
   '(("emacs-lisp" (:background "#f8f8f8" :extend t))
     ("python" (:background "#f8f8f8" :extend t))
     ("js" (:background "#f8f8f8" :extend t))
     ("typescript" (:background "#f8f8f8" :extend t))
     ("html" (:background "#f8f8f8" :extend t))
     ("css" (:background "#f8f8f8" :extend t))
     ("shell" (:background "#f8f8f8" :extend t)))))

;; Keybindings for source blocks
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-s") 'org-edit-src-code)  ;; Edit in dedicated buffer
  (define-key org-mode-map (kbd "C-c C-v") 'org-babel-expand-src-block)  ;; Show options
  (define-key org-mode-map (kbd "C-c C-c") 'org-babel-execute-src-block)  ;; Execute
  (define-key org-mode-map (kbd "C-c C-o") 'org-babel-open-src-block-result)  ;; View results
  (define-key org-mode-map (kbd "C-c C-r") 'org-babel-remove-result-one-or-many)  ;; Remove results
  (define-key org-mode-map (kbd "C-c C-u") 'org-babel-goto-src-block-head)  ;; Go to header
  (define-key org-mode-map (kbd "C-c C-i") 'org-babel-view-src-block-info)  ;; View info
  (define-key org-mode-map (kbd "C-c C-t") 'org-babel-tangle)  ;; Extract to files
  (define-key org-mode-map (kbd "C-c C-j") 'org-babel-insert-header-arg)  ;; Add header arg
  (define-key org-mode-map (kbd "C-c C-k") 'org-babel-load-in-session)  ;; Load in session
  (define-key org-mode-map (kbd "C-c C-l") 'org-babel-lob-ingest)  ;; Load library
  (define-key org-mode-map (kbd "C-c C-n") 'org-babel-next-src-block)  ;; Next block
  (define-key org-mode-map (kbd "C-c C-p") 'org-babel-previous-src-block))  ;; Previous block

;; Org Bullets: Show Org heading bullets as UTF-8 characters
;; Replaces the default asterisk bullets in Org headings with attractive
;; Unicode symbols, improving the visual appearance of Org documents.
;; GitHub: https://github.com/emacsorphanage/org-bullets
(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

;; Org Modern: Modern Org appearance with better styling
;; Provides a modern, clean appearance for Org mode with better typography,
;; improved table styling, and enhanced visual elements for a polished look.
;; GitHub: https://github.com/minad/org-modern
(use-package org-modern
  :ensure t
  :hook (org-mode . org-modern-mode))

;; Make Org text more readable with variable-pitch font for prose
(add-hook 'org-mode-hook #'variable-pitch-mode)

;; Org Download: Drag and drop images to Org mode files
;; Enables drag-and-drop image insertion into Org documents with automatic
;; image saving and link creation, streamlining multimedia document creation.
;; GitHub: https://github.com/abo-abo/org-download
(use-package org-download
  :ensure t
  :hook (org-mode . org-download-enable))

;; Indent content to match heading level
(add-hook 'org-mode-hook #'org-indent-mode)

;; Graphviz DOT mode: Major mode for editing and previewing .dot and .gv files
;; Integrates with org-babel for rendering diagrams in Org documents
(use-package graphviz-dot-mode
  :ensure t
  :mode (("\\.dot\\'" . graphviz-dot-mode)
         ("\\.gv\\'" . graphviz-dot-mode))
  :init
  (setq default-tab-width 4))

;; --- Org-roam Configuration ---
;; Org-roam is a plain-text personal knowledge management system.
;; It helps you create a network of notes, where each note is a node
;; and links between notes create a graph of your knowledge.

;; Core org-roam functionality
;; This provides the main note-taking and linking features
(use-package org-roam
  :ensure t
  :init
  ;; Set the directory where your notes will be stored
  (setq org-roam-directory "~/org/roam")
  :custom
  ;; Enable completion in all org buffers
  (org-roam-completion-everywhere t)
  ;; Show titles and tags in completion interfaces
  (org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  ;; Templates for creating new notes
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
  ;; Only track .org files in the roam directory
  (org-roam-file-extensions '("org"))
  ;; Skip commented headings when building the graph
  (org-roam-db-node-include-function (lambda () (not (org-in-commented-heading-p))))
  ;; Database settings
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
  ;; Protocol settings
  (org-roam-protocol-store-links t)  ;; Store links in capture
  (org-roam-protocol-store-html t)   ;; Store HTML content if available
  (org-roam-protocol-store-images t)  ;; Store images if available
  ;; Protocol templates
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
  ;; Daily notes settings
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
  ;; Migration settings
  (org-roam-migrate-auto-backup t)  ;; Create backup before migration
  (org-roam-migrate-backup-directory "~/org/roam/backups/")  ;; Backup location
  (org-roam-migrate-file-naming-scheme 'title)  ;; Use title for filenames
  (org-roam-migrate-file-extension ".org")  ;; File extension
  (org-roam-migrate-link-style 'wiki)  ;; Use wiki-style links
  (org-roam-migrate-link-format "[[%s]]")  ;; Link format
  (org-roam-migrate-tag-style 'org)  ;; Use org-style tags
  (org-roam-migrate-tag-format ":%s:")  ;; Tag format
  :bind
  ;; Keybindings for common org-roam operations
  (("C-c n f" . org-roam-node-find)     ;; Find or create a note
   ("C-c n i" . org-roam-node-insert)   ;; Insert a link to a note
   ("C-c n c" . org-roam-capture)       ;; Create a new note using templates
   ("C-c n j" . org-roam-dailies-capture-today)  ;; Create a daily note
   ("C-c n y" . org-roam-dailies-capture-yesterday)  ;; Capture to yesterday's file
   ("C-c n t" . org-roam-dailies-capture-tomorrow)  ;; Capture to tomorrow's file
   ("C-c n d" . org-roam-dailies-find-date)  ;; Find daily note by date
   ("C-c n p" . org-roam-dailies-find-previous-note)  ;; Find previous daily note
   ("C-c n n" . org-roam-dailies-find-next-note)  ;; Find next daily note
   ("C-c n m" . org-roam-migrate-wizard)  ;; Start migration wizard
   ("C-c n r" . org-roam-migrate-reorganize)  ;; Reorganize notes
   ("C-c n b" . org-roam-migrate-backup)  ;; Create backup
   ("C-c n v" . org-roam-migrate-validate))  ;; Validate notes
  :config
  ;; Enable database autosync
  (org-roam-db-autosync-mode))

;; Visual graph interface for org-roam
;; Provides an interactive web interface to explore your notes
(use-package org-roam-ui
  :ensure t
  :after org-roam
  :custom
  ;; Set the port for the web interface (http://localhost:35901)
  (org-roam-ui-port 35901)
  ;; Enable the graph view by default
  (org-roam-ui-sync-theme t)
  ;; Update the graph in real-time
  (org-roam-ui-follow t)
  ;; Show node labels
  (org-roam-ui-node-display-template "${title:100}")
  :bind
  ;; Keybinding to open the graph interface
  (("C-c n g" . org-roam-ui-mode)))

;; PDF Tools for viewing and managing PDFs
;; Required for org-roam-bibtex PDF integration
(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install)
  :custom
  (pdf-view-display-size 'fit-page)
  (pdf-view-use-scaling t)
  (pdf-view-use-imagemagick t)
  ;; Search and Navigation
  (pdf-isearch-minor-mode t)  ;; Enable isearch in PDFs
  (pdf-view-auto-slice-minor-mode t)  ;; Auto-slice large pages
  (pdf-view-midnight-minor-mode t)  ;; Dark mode toggle
  (pdf-view-printer-minor-mode t)  ;; Printer-friendly mode
  ;; Annotations and Links
  (pdf-annot-activate-created-annotations t)  ;; Auto-activate new annotations
  (pdf-annot-minor-mode t)  ;; Enable annotation mode
  (pdf-links-minor-mode t)  ;; Enable link following
  (pdf-outline-minor-mode t)  ;; Enable outline navigation
  :bind
  ;; PDF navigation and features
  (("C-c C-p" . pdf-view-scroll-up-or-next-page)  ;; Scroll up/next page
   ("C-c C-n" . pdf-view-scroll-down-or-previous-page)  ;; Scroll down/previous page
   ("C-c C-f" . pdf-view-fit-page-to-window)  ;; Fit page to window
   ("C-c C-w" . pdf-view-fit-width-to-window)  ;; Fit width to window
   ("C-c C-m" . pdf-view-midnight-minor-mode)  ;; Toggle dark mode
   ("C-c C-a" . pdf-annot-add-annotation)  ;; Add annotation
   ("C-c C-l" . pdf-links-action-perform)  ;; Follow link
   ("C-c C-o" . pdf-outline)))  ;; Show outline

;; BibTeX integration for org-roam
;; Manages academic references and creates notes from papers
(use-package org-roam-bibtex
  :ensure t
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :custom
  ;; Set the BibTeX file location
  (org-roam-bibtex-bibliography-path "~/org/bib/references.bib")
  ;; Template for creating notes from BibTeX entries
  (org-roam-bibtex-note-templates
   '(("a" "article" plain
      "* ${title}\n:PROPERTIES:\n:ROAM_REFS: @${=key=}\n:END:\n\n%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${citekey}.org"
                         "#+title: ${title}\n#+filetags: :article:\n#+date: %U\n\n")
      :unnarrowed t)
     ("b" "book" plain
      "* ${title}\n:PROPERTIES:\n:ROAM_REFS: @${=key=}\n:END:\n\n%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${citekey}.org"
                         "#+title: ${title}\n#+filetags: :book:\n#+date: %U\n\n")
      :unnarrowed t)))
  ;; PDF Integration
  (org-roam-bibtex-pdf-handler-function 'org-roam-bibtex-pdf-handler)  ;; PDF handling
  (org-roam-bibtex-pdf-extension ".pdf")  ;; PDF file extension
  ;; Export Settings
  (org-roam-bibtex-export-citation t)  ;; Include citations in exports
  (org-roam-bibtex-export-reference t)  ;; Include references in exports
  ;; Note Organization
  (org-roam-bibtex-note-tags-function 'org-roam-bibtex-note-tags)  ;; Custom tag function
  (org-roam-bibtex-note-title-template "${title}")  ;; Note title template
  :bind
  ;; Keybindings for BibTeX operations
  (("C-c n b" . org-roam-bibtex-insert-citation)  ;; Insert citation
   ("C-c n r" . org-roam-bibtex-insert-reference)  ;; Insert reference
   ("C-c n e" . org-roam-bibtex-open-pdf)))  ;; Open PDF if available

;; Automatic timestamp management
;; Tracks creation, modification, and access times for notes
(use-package org-roam-timestamps
  :ensure t
  :after org-roam
  :custom
  ;; Timestamp properties
  (org-roam-timestamps-properties
   '("CREATED" "MODIFIED" "ACCESSED" "REVIEWED" "PUBLISHED" "ARCHIVED"))  ;; Properties to track
  ;; Timestamp formats
  (org-roam-timestamps-format "%Y-%m-%d %H:%M:%S")  ;; Format for timestamps
  ;; Automatic updates
  (org-roam-timestamps-update-on-save t)  ;; Update on save
  (org-roam-timestamps-update-on-access t)  ;; Update on access
  (org-roam-timestamps-update-on-create t)  ;; Update on create
  ;; Timestamp locations
  (org-roam-timestamps-property-location 'head)  ;; Add to file header
  (org-roam-timestamps-property-position 'top)  ;; Add at top of file
  ;; Timestamp templates
  (org-roam-timestamps-templates
   '(("CREATED" . "Created: %s by %u")           ;; With user
     ("MODIFIED" . "Last modified: %s by %u")     ;; With user
     ("ACCESSED" . "Last accessed: %s by %u")     ;; With user
     ("REVIEWED" . "Last reviewed: %s by %u")     ;; With user
     ("PUBLISHED" . "Published: %s by %u")        ;; With user
     ("ARCHIVED" . "Archived: %s by %u")))        ;; With user
  :config
  ;; Enable timestamps
  (org-roam-timestamps-mode))

(provide 'nh-org)
;;; nh-org.el ends here
