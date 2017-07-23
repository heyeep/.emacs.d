;;;; -*- lexical-binding: t; -*-

;;; Ivy Mode
(use-package ivy
  :ensure t
  :config
  (ivy-mode))

(use-package counsel
  :ensure t
  :config
  ;; Disable for now while trying grizzl.
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "s-x") 'counsel-M-x))

(use-package swiper
  :ensure t
  :diminish ivy-mode
  :config
  (setq ivy-initial-inputs-alist nil)

  ;; swapping behavior
  (define-key ivy-minibuffer-map (kbd "RET") 'ivy-alt-done)
  (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-done)

  ;; default: "ag --nocolor --nogroup %s -- ."
  (setq counsel-ag-base-command "ag -U --nocolor --nogroup %s -- .")
  (setq ivy-count-format "")
  (setq ivy-height 15))

(use-package smex
  :ensure t)

;;; Projectile
(use-package projectile
  :ensure t
  :commands (projectile-project-p
             projectile-project-root
             projectile-find-file
             projectile-switch-project
             projectile-switch-to-buffer
             projectile-ag
             projectile-recentf
             jojo/projectile-find)
  :diminish projectile-mode
  :config
  (setq projectile-enable-caching t)
  (projectile-mode)
  (setq projectile-completion-system 'ivy))

;;; Company

(use-package company
  :ensure t
  :init
  (defun jojo/company-visible-and-explicit-action-p ()
    "Determine if tooltip is visible and user explicit action took place."
    (and (company-tooltip-visible-p)
         (company-explicit-action-p)))
  (defun company-ac-setup ()
    "Sets up company to behave similarly to auto-complete mode."
    (setq company-require-match nil)
    (setq company-tooltip-idle-delay .25)
    (setq company-auto-complete #'jojo/company-visible-and-explicit-action-p)
    (setq company-frontends
          '(company-echo-metadata-frontend
            company-pseudo-tooltip-unless-just-one-frontend-with-delay
            company-preview-frontend))
    (define-key company-active-map [tab]
      'company-select-next-if-tooltip-visible-or-complete-selection)
    (define-key company-active-map (kbd "TAB")
      'company-select-next-if-tooltip-visible-or-complete-selection))
  (defun jojo/company-set-prefix-length (len)
    "Changing prefix length locally."
    (make-local-variable 'company-minimum-prefix-length)
    (setq company-minimum-prefix-length len))
  (defun jojo/company-set-delay (delay)
    "Changing delay length locally."
    (make-local-variable 'company-idle-delay)
    (setq company-idle-delay delay))
  (defun jojo/company-set-clang-args (clang-args)
    "Set up clang arguments locally."
    (make-local-variable 'company-clang-arguments)
    (setq company-clang-arguments clang-args))
  (defun jojo/company-backend-in-backends (b)
    "Check if backend b is already in company-backends.
We need to do this check because each backend has additional symbols attached.
Ex. company-clang :with company-yasnippet."
    (let ((in-backend nil))
      (dolist (backend company-backends)
        (when (member b backend)
          (setq in-backend t)))
      in-backend))
  (defun jojo/company-push-backend (b &optional no-merge)
    "Adds backend b to company mode if it's not already in the list of backends.
If `no-merge' is non-nil, don't merge additional backends."
    (unless (jojo/company-backend-in-backends b)
      (add-to-list 'company-backends b))
    (unless no-merge
      (jojo/company-merge-backends)))
  (defun jojo/company-push-backend-local (b &optional no-merge)
    "Push backend into local backends.
If `no-merge' is non-nil, don't merge additional backends."
    (make-local-variable 'company-backends)
    (jojo/company-push-backend b no-merge))
  (defun jojo/company-set-local-backends (backends &optional no-merge)
    "Set backends locally.
If `no-merge' is non-nill, don't merge additional backends."
    (make-local-variable 'company-backends)
    (setq company-backends backends)
    (unless no-merge
      (jojo/company-merge-backends)))
  :config
  (setq company-echo-delay 1)
  (setq company-minimum-prefix-length 1)
  ;; Add additional backend support for all company backends.
  ;; https://github.com/syl20bnr/spacemacs/pull/179
  ;; https://stackoverflow.com/questions/134887/when-to-use-quote-in-lisp
  (defun merge-backend-with-company-backends (backend-to-merge)
    "Merges a backend with every backend in company-backends.
The backend will only be merged if it's not already being used in the current backend.
We do this because so that the backend that we're merging
will always be part of the completion candidates.
For example, merging company-yasnippet to company-capf
will yield (company-capf :with company-yasnippet)."
    ;; create a list of backend-to-merge with a count equal to company-backends
    ;; this is so mapcar* can iterate over both lists equally
    ;; ex. if we have (company-capf company-xcode),
    ;; then the list is (company-yasnippet company-yasnippet)
    (setq blist (make-list (cl-list-length company-backends) backend-to-merge))
    ;; b will be backend-to-merge
    ;; backend will be a backend from company-backends
    (setq company-backends (cl-mapcar (lambda (backend b)
                                        (if (and (listp backend) (member b backend))
                                            backend
                                          (append (if (consp backend)
                                                      backend
                                                    (list backend))
                                                  (if (and (listp backend)
                                                           (member :with backend))
                                                      `(,b)
                                                    `(:with ,b)))))
                                      company-backends blist)))
  (defun jojo/company-merge-backends ()
    "Merge common backends."
    (merge-backend-with-company-backends 'company-dabbrev-code))
  (jojo/company-merge-backends)
  ;; if the completion is JoJo, typing jojo will get to it
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-ignore-case t) ; default is keep-prefix
  ;; Dabbrev same major mode buffers.
  (setq company-dabbrev-other-buffers nil)
  ;; use tab to cycle selection
  ;; https://github.com/company-mode/company-mode/issues/216
  ;; https://github.com/company-mode/company-mode/issues/246#issuecomment-68538735
  (setq company-auto-complete nil)
  (define-key company-active-map [backtab] 'company-select-previous)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
  (define-key company-active-map [S-tab] 'company-select-previous)
  (define-key company-active-map [S-iso-lefttab] 'company-select-previous)
  (define-key company-active-map [(shift tab)] 'company-select-previous)
  (define-key company-active-map [tab] 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "RET")
    'jojo/company-complete-selection-or-abort-if-same-unless-yas)
  (define-key company-active-map [return]
    'jojo/company-complete-selection-or-abort-if-same-unless-yas)
  (defun jojo/company-complete-selection-or-abort-if-same-unless-yas ()
    "Complete selection or abort if prefix matches selection.
If backend is yasnippet, complete normally."
    (interactive)
    (if (and
         ;; Completion is not from Yasnippet.
         (not (eq 'company-yasnippet
                  (get-text-property 0 'company-backend
                                     (nth company-selection company-candidates))))
         ;; Completion result is the same as the prefix.
         (string-equal company-prefix
                       (nth company-selection company-candidates)))
        (jojo/company-abort-and-newline)
      (company-complete-selection)))
  (defun jojo/company-abort-and-newline ()
    "Cancel the company selection and then go to next line."
    (interactive)
    (company-abort)
    (newline-and-indent))
  (define-key company-active-map (kbd "<S-return>") 'jojo/company-abort-and-newline)
  ;; loop completion selections
  (setq company-selection-wrap-around t)
  (setq company-idle-delay .1)
  (company-ac-setup)
  (defun jojo/setup-company-transformers (&optional reset)
    "Push list of transformers to `company-transformers'.
If `reset', set `company-transformers' to nil."
    (if reset
        (setq company-transformers nil)
      (push #'company-sort-prefer-same-case-prefix company-transformers)))
  (jojo/setup-company-transformers)
  (global-company-mode))
;; documentation popup for company
(use-package company-quickhelp
  :ensure t
  :commands (company-quickhelp-mode)
  :init
  (defun jojo/company-quickhelp-hook ()
    "Setting up company-quickhelp."
    (company-quickhelp-mode 1))
  (add-hook 'company-mode-hook #'jojo/company-quickhelp-hook)
  :config
  (setq company-quickhelp-delay 2.3))

;; Flycheck
(use-package flycheck
  ;; :load-path "~/.emacs.d/fork/flycheck/"
  :ensure t
  :diminish flycheck-mode
  :commands (flycheck-mode)
  :init
  (setq flycheck-idle-change-delay 2)
  (setq-default flycheck-emacs-lisp-load-path 'inherit)
  (add-hook 'prog-mode-hook #'flycheck-mode)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(use-package flycheck-pos-tip
  :ensure t
  :after flycheck
  :config
  (flycheck-pos-tip-mode))

(use-package dummy-h-mode
  :ensure t
  :init
  :mode ("\\.h$" . dummy-h-mode))

;; YCMD
(use-package company-ycmd
  :ensure t
  :commands (ycmd-mode)
  :init
  (add-hook 'ycmd-mode-hook 'ycmd-eldoc-setup)
  (defun jojo/ycmd-base-setup ()
    "Base setup for ycmd."
    (set-variable 'ycmd-server-command '("python" "/Users/hiep/.emacs.d/fork/ycmd/ycmd"))
    (setq ycmd-extra-conf-handler 'ignore) ;; Only use global config
    (jojo/company-push-backend 'company-ycmd)
    (ycmd-mode 1))

  (mapcar
   (lambda (x)
     (add-hook x #'jojo/ycmd-base-setup))
   '(c-mode-hook c++-mode-hook))
  :config
  (setq ycmd-min-num-chars-for-completion 1)
  (setq ycmd-force-semantic-completion nil)
  (setq ycmd-tag-files 'auto)
  (setq request-message-level -1))

(use-package flycheck-ycmd
  :ensure t
  :commands
  (flycheck-ycmd-setup)
  :init
  (mapcar
   (lambda (x)
     (add-hook x #'flycheck-ycmd-setup))
   '(c-mode-hook c++-mode-hook))
  :config
  (when (not (display-graphic-p))
    (setq flycheck-indication-mode nil)))

;; Rtags
(use-package rtags
  :ensure t
  :commands (rtags-start-process-unless-running rtags-install)
  :config
  (setq rtags-jump-to-first-match nil)
  (setq rtags-tracking t)
  (setq rtags-tracking-timer-interval .3)
  (setq rtags-display-result-backend 'default)
  (setq rtags-autostart-diagnostics t))

(provide 'yzm-autocompletion)
