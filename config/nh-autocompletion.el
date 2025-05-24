;;; nh-autocompletion.el --- Autocompletion configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for autocompletion and related packages.

;;; Code:

;; Ivy: Flexible minibuffer completion
(use-package ivy
  :ensure t
  :diminish
  :custom
  (ivy-use-virtual-buffers t)
  (enable-recursive-minibuffers t)
  (ivy-count-format "(%d/%d) ")
  :config
  (ivy-mode 1)
  (setq ivy-display-style 'fancy)
  (setq ivy-count-format "") ;; Hide candidate count
  (setq ivy-height 15)       ;; Show up to 15 candidates
  ;; Swap RET and C-j: RET does ivy-alt-done, C-j does ivy-done
  (define-key ivy-minibuffer-map (kbd "RET") 'ivy-alt-done)
  (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-done)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)) ;; Resume last Ivy session

;; Ivy-enhanced versions of common Emacs commands
(use-package counsel
  :ensure t
  :after ivy
  :config
  (counsel-mode 1)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "s-x") 'counsel-M-x)
  (global-set-key (kbd "C-c r") 'counsel-recentf) ;; Quick access to recent files
  (setq counsel-find-file-at-point t) ;; Enable preview for counsel-find-file
  (let ((nh/counsel-ignore-patterns
         '("\\.elc\\'"      ; Byte-compiled Emacs Lisp files
           "\\`#.*#\\'"     ; Emacs auto-save files
           "~\\'"           ; Emacs backup files
           )))
    (setq counsel-find-file-ignore-regexp
          (mapconcat #'identity nh/counsel-ignore-patterns "\\|"))))

;; Swiper: Ivy-powered in-buffer search
(use-package swiper
  :ensure t
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper)
         ("C-c C-r" . ivy-resume)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-x b" . ivy-switch-buffer))
  :config
  (setq ivy-count-format "")      ;; Hide candidate count
  (setq counsel-ag-base-command "ag -U --nocolor --nogroup %s -- .")
  (setq swiper-goto-start-of-match t)) ;; Highlight line number in Swiper

;; Add extra info to Ivy candidates
(use-package ivy-rich
  :ensure t
  :config
  (ivy-rich-mode 1)
  ;; Remove ffip transformer from ivy-rich to prevent interference with icons
  (with-eval-after-load 'ivy-rich
    (setq ivy-rich-display-transformers-list
          (assq-delete-all 'find-file-in-project ivy-rich-display-transformers-list))))

;; Smarter sorting and filtering
(use-package ivy-prescient
  :ensure t
  :after ivy
  :config
  (ivy-prescient-mode 1)
  (prescient-persist-mode 1))

;; Pretty icons in Ivy
(use-package all-the-icons-ivy-rich
  :ensure t
  :after (ivy-rich all-the-icons)
  :init
  (all-the-icons-ivy-rich-mode 1))

;; Company Mode: Modular text completion framework
;; https://github.com/company-mode/company-mode
;;
;; This block sets up company-mode for global autocompletion, with modern defaults
;; and advanced helpers for dynamic backend management. The helpers allow you to
;; add, merge, and set company backends per buffer or mode, which is useful for
;; language-specific or project-specific completion sources.
(use-package company
  :ensure t
  :diminish company-mode
  :init
  ;; === Dynamic Backend Helpers ===
  ;; These functions let you add or merge company backends globally or buffer-locally.
  ;; Useful for language-specific or context-specific completion tweaks.
  (defun nh/company-backend-in-backends (b)
    "Check if backend B is already in `company-backends`."
    (cl-some (lambda (backend)
               (if (listp backend)
                   (member b backend)
                 (eq b backend)))
             company-backends))

  (defun nh/company-push-backend (b &optional no-merge)
    "Add backend B to `company-backends` if not present.
If NO-MERGE is non-nil, don't merge additional backends."
    (unless (nh/company-backend-in-backends b)
      (add-to-list 'company-backends b))
    (unless no-merge
      (nh/company-merge-backends)))

  (defun nh/company-push-backend-local (b &optional no-merge)
    "Add backend B to buffer-local `company-backends`."
    (setq-local company-backends (copy-sequence company-backends))
    (nh/company-push-backend b no-merge))

  (defun nh/company-set-local-backends (backends &optional no-merge)
    "Set buffer-local `company-backends` to BACKENDS."
    (setq-local company-backends backends)
    (unless no-merge
      (nh/company-merge-backends)))

  (defun nh/company-merge-backend-with-company-backends (backend-to-merge)
    "Merge BACKEND-TO-MERGE with every backend in `company-backends`.
This ensures the merged backend (e.g., company-dabbrev-code) is always available
as a fallback for all completions."
    (let ((blist (make-list (length company-backends) backend-to-merge)))
      (setq company-backends
            (cl-mapcar (lambda (backend b)
                         (if (and (listp backend) (member b backend))
                             backend
                           (append (if (consp backend) backend (list backend))
                                   (if (and (listp backend) (member :with backend))
                                       `(,b)
                                     `(:with ,b)))))
                       company-backends blist))))

  (defun nh/company-merge-backends ()
    "Merge common backends (e.g., dabbrev-code) into all company backends."
    (nh/company-merge-backend-with-company-backends 'company-dabbrev-code))

  :custom
  ;; Delay before suggestions popup (in seconds)
  (company-idle-delay 0.1)
  ;; Minimum prefix length before popup
  (company-minimum-prefix-length 1)
  ;; Align annotations to the right edge
  (company-tooltip-align-annotations t)
  ;; Show numbers for quick selection (M-1, M-2, ...)
  (company-show-numbers t)
  ;; Wrap around when cycling candidates
  (company-selection-wrap-around t)
  ;; Don't downcase dabbrev completions
  (company-dabbrev-downcase nil)
  ;; Ignore case for dabbrev completions
  (company-dabbrev-ignore-case t)
  ;; Only use dabbrev from same major mode
  (company-dabbrev-other-buffers nil)
  ;; Delay before echoing candidate info
  (company-echo-delay 1)
  ;; Use the default frontends (popup and echo area)
  (company-frontends
    '(company-pseudo-tooltip-frontend
      company-echo-metadata-frontend))
  :bind
  ;; Keybindings for company popup navigation and selection
  (:map company-active-map
        ([tab] . company-complete-common-or-cycle)
        ("TAB" . company-complete-common-or-cycle)
        ("<backtab>" . company-select-previous)
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ("RET" . company-complete-selection)
        ("<return>" . company-complete-selection))
  :config
  ;; Prefer candidates that match the case of your input
  (add-to-list 'company-transformers 'company-sort-prefer-same-case-prefix)
  ;; Merge dabbrev-code into all backends by default for fallback completions
  (nh/company-merge-backends)
  (global-company-mode 1))

;; Company-Box: Modern company popup with icons and documentation
;; https://github.com/sebastiencs/company-box
(use-package company-box
  :hook (company-mode . company-box-mode)
  :custom
  ;; Delay before showing documentation popup (seconds)
  (company-box-doc-delay 0.2)
  (company-box-icons-alist 'company-box-icons-all-the-icons) ; Use all-the-icons set
  (company-box-scrollbar nil) ; Hide the scrollbar
  (company-box-show-single-candidate t) ; Show box even for one candidate
  (company-box-max-candidates 50) ; Limit number of candidates
  )

(provide 'nh-autocompletion)
;;; nh-autocompletion.el ends here
