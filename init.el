;;;; -*- lexical-binding: t; -*-



;; (package-initialize)

(setq gc-cons-threshold 100000000) ; 100 mb
(add-hook 'focus-out-hook 'garbage-collect)

;; Separate package directories according to Emacs version.
;; Bytecode compiled in different Emacs versions are not
;; guaranteed to work with another.
(setq package-user-dir
      (format "%selpa/%s/" user-emacs-directory emacs-major-version))

(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))

;; M-x list-packages U x to upgrade packages.
(setq package-list '(diminish))

;; Disable in favor of `use-package'.
(setq package-enable-at-startup nil)

;; Package Repositories
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))

;; Activate all packages (in particular autoloads).
(package-initialize)

;; Bootstrap `use-package'.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish) ; for :diminish
(require 'bind-key) ; for :bind

;; Install package if not existing.
(setq use-package-always-ensure nil)

;; Check loading times with `use-package'.
(setq use-package-verbose t)

;; Fetch the list of packages when unavailable.
(when (not package-archive-contents)
  (package-refresh-contents))

;; Install any missing packages.
(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))

(use-package dash :ensure t)
(use-package s :ensure t)

;; Start daemon automatically.
(add-hook 'after-init-hook (lambda ()
                             (load "server") ;; server-running-p is not autoloaded.
                             (unless (server-running-p)
                               (server-start))))
(use-package gotham-theme :defer :ensure t)

(use-package spacemacs-theme
  :defer
  :ensure t
  :init
  (setq spacemacs-theme-comment-bg nil))

;; https://stackoverflow.com/questions/9840558/why-cant-emacs-24-find-a-custom-theme-i-added
;; Add wildcard matching to themes in elpa folder.
(-each
    (-map
     (lambda (item)
       (format "~/.emacs.d/elpa/%s" item))
     (-filter
      (lambda (item) (s-contains? "theme" item))
      (directory-files "~/.emacs.d/elpa/")))
  (lambda (item)
    (add-to-list 'custom-theme-load-path item)))

(use-package theme-changer
  :ensure t
  :init
  ;; Dallas
  (setq calendar-latitude 32.85)
  (setq calendar-longitude -96.85)
  ;; Japan
  ;; (setq calendar-latitude 36.3147)
  ;; (setq calendar-longitude 139.8)
  :config
  (defun jojo/make-modeline-taller ()
    "Make the mode line taller."
    (dolist (sym '(mode-line mode-line-inactive))
      (set-face-attribute
       sym nil
       :box `(:line-width 5 :color ,(face-attribute `,sym :background)))))

  (defun is-daytime()
    "Figuring out day or night."
    (let*
        ((today-times (sunrise-sunset-times (today)))
         (sunrise-today (first today-times))
         (sunset-today (second today-times)))
      (daytime-p sunrise-today sunset-today)))

  (defun jojo/update-theme (&rest _args)
    "Update various UI elements when theme changes"
    (jojo/make-modeline-taller)
    (eval-after-load 'org-mode
      (lambda ()
        (jojo/customize-org-ui)))
    (when (fboundp 'org-reload)
      (eval-after-load 'org-faces
        (lambda ()
          (set-face-background 'org-hide (face-attribute 'default :background))
          (set-face-foreground 'org-hide (face-attribute 'default :background))))
      (when (let (has-org-mode)
              (dolist (b (buffer-list) has-org-mode)
                (with-current-buffer b
                  (when (eq major-mode 'org-mode)
                    (setq has-org-mode t)))))
        (org-reload)))
    (eval-after-load 'company
      (lambda ()
        (set-face-attribute
         'company-preview
         nil
         :background (face-attribute 'company-preview-common :background)))))

  (advice-add 'change-theme :after #'jojo/update-theme)
  (set-frame-parameter nil 'background-mode 'light)
  ;;
  ;; Gotham Theme: James' default
  ;;   (change-theme 'gotham 'gotham))
  ;; SpaceMacs Theme: Light/Dark
  ;;   (change-theme 'spacemacs-light 'spacemacs-dark))
  ;; SpaceMacs Theme: Light/Light
  ;;   (change-theme 'spacemacs-light 'spacemacs-light))
  ;; Solarized Theme: Light/Dark | Preferred
  (change-theme 'solarized-light 'solarized-dark))
;; Solarized Theme: Light/Light
;;   (change-theme 'solarized-light 'solarized-light))

                                        ; Diminish modeline clutter.
(when (require 'diminish nil 'noerror)
  (diminish 'subword-mode)
  (diminish 'visual-line-mode)
  (diminish 'abbrev-mode)
  (eval-after-load "eldoc"
    '(diminish 'eldoc-mode))
  (eval-after-load "hideshow"
    '(diminish 'hs-minor-mode))
  (eval-after-load "autorevert"
    '(diminish 'auto-revert-mode)))

;; Disable startup screen.
(setq inhibit-startup-screen t)

;; Mute system sound.
(setq ring-bell-function #'ignore)

;; Set title of window to current file.
(setq frame-title-format '("%f"))

(use-package nlinum
  :ensure t
  :init
  (dolist (hook '(prog-mode-hook
                  nxml-mode-hook))
    (add-hook hook #'nlinum-mode)))

;; https://stackoverflow.com/questions/3631220/fix-to-get-smooth-scrolling-in-emacs
(setq scroll-margin 5
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

;; Wrap line when it reaches end.
(setq-default truncate-lines 1)
(global-visual-line-mode 0)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

(setq kill-whole-line t)
;; yes or no to y or n
(fset 'yes-or-no-p 'y-or-n-p)
;; Make the column number show up.
(column-number-mode 1)

;; emacs 24+ auto indents by default if `electric-indent-mode' is on
;; so disable automatic indent by default
;; but enable it in all programming modes.
(electric-indent-mode 0)

(dolist (mode '(prog-mode-hook
                yaml-mode-hook
                css-mode-hook
                html-mode-hook
                nxml-mode-hook))
  (add-hook mode (lambda ()
                   (interactive)
                   (electric-indent-local-mode 1))))

;; Indenting
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq c-basic-offset 4)

(add-hook 'prog-mode-hook 'eldoc-mode)

;; Reverting buffers
(global-auto-revert-mode t) ; automatically reload buffers on change

;; Highlight matching parentheses.
(use-package paren
  :ensure nil
  :config
  (show-paren-mode t))

;; No Autosave by default
(setq auto-save-default nil)

;; Don't make backups of files in version control.
(setq vc-make-backup-files nil)

;; Same filenames get the directory name inserted also.
(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator "|")
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*"))

;; Set this variable to a non-nil value to speed up display of characters
;; using large fonts, at the price of a larger memory footprint of the
;; Emacs session.
(setq inhibit-compacting-font-caches t)

(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))

(use-package magit
  :ensure t
  :commands (magit-toplevel magit-status magit-blame magit-log)
  :config
  (setq magit-repository-directories '("~/Code"))
  (setq magit-refresh-status-buffer nil)
  (setq magit-completing-read-function 'ivy-completing-read)
  (setq vc-handled-backends (delq 'Git vc-handled-backends)))

;; Don't let osx swallow Meta key.
(setq mac-pass-command-to-system nil)

;; Command -> Meta
;; Option -> Super
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)

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

(use-package neotree
  :ensure t
  :commands (neo-global--window-exists-p
             neotree-dir
             neotree-toggle
             neotree-enter
             neotree-hide)
  :init
  (setq neo-persist-show nil)
  (setq neo-window-fixed-size nil)
  (setq neo-create-file-auto-open t)
  (setq neo-smart-open nil)
  (setq neo-mode-line-type 'neotree)
  (setq neo-show-hidden-files t)
  (setq neo-mode-line-type 'none)
  (defun neotree-resize-window (&rest _args)
    "Resize neotree window.
https://github.com/jaypei/emacs-neotree/pull/110"
    (interactive)
    (neo-buffer--with-resizable-window
     (let ((fit-window-to-buffer-horizontally t))
       (fit-window-to-buffer))))

  (defun neotree-reopen (&rest _args)
    "Close and reopen neotree returning to the current window after.
`neotree-resize-window' doesn't work on file open (neotree window isn't live anymore).
Hiding and showing again will resize the neotree window properly."
    (interactive)
    (save-selected-window
      (neotree-hide)
      (neotree-show)))

  (defun neotree-resize-window-with-timer (&rest args)
    "Resize neotree window after waiting for redisplay."
    (when (neo-global--window-exists-p)
      (redisplay)
      (run-with-timer .1 nil #'neotree-resize-window args)))

  ;; When opening a file from neotree.
  (advice-add 'neo-open-file :after #'neotree-reopen)
  ;; When going up the directory stack.
  (advice-add 'neotree-change-root :after #'neotree-resize-window)
  ;; When opening a directory node.
  (advice-add 'neo-open-dir :after #'neotree-resize-window)
  ;; When showing neotree with `neotree-show'.
  (advice-add 'neotree-show :after #'neotree-resize-window)
  ;; Inside neotree-projectile
  (advice-add 'neotree-dir :after #'neotree-resize-window)
  ;; Inside neotree-projectile
  (advice-add 'neotree-find :after #'neotree-resize-window)

  (defun jojo/neotree-around-reopen (orig-fun &rest args)
    "Close neotree before calling advised function.
Reopen neotree after advised function runs."
    (interactive)
    (if (not (neo-global--window-exists-p))
        (apply orig-fun args)
      (save-selected-window
        (neotree-hide)
        (let ((res (apply orig-fun args)))
          (neotree-show)
          res))))

  (defun neotree-projectile ()
    "Open neotree with projectile as root and open node for current file.
If projectile unavailable or not in a project, open node at file path.
If file path is not available, open $HOME."
    (interactive)
    (if (neo-global--window-exists-p)
        (call-interactively 'neotree-hide)
      (let ((file-name (buffer-file-name)))
        (if (and (not file-name)
                 (let ((buffer-name (buffer-name)))
                   (cond
                    ((equal buffer-name "*cider-repl server*") nil)
                    (t t))))
            (neotree-dir "~/")
          (let ((dir-name (if (and (fboundp 'projectile-project-p)
                                   (projectile-project-p))
                              (projectile-project-root)
                            (file-name-directory file-name))))
            (neotree-dir dir-name)
            (neotree-find file-name))))))
  :config
  ;; Upon resizing window
  (advice-add #'toggle-frame-maximized
              :after #'neotree-resize-window-with-timer)

  ;; Try to use icons from `all-the-icons',
  ;; if font is not installed, we use fallback.
  (if (null (find-font (font-spec :name "github-octicons")))
      (setq neo-theme 'nerd)
    (use-package all-the-icons :ensure t)
    (setq neo-theme 'icons))
  )

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

(use-package lisp-mode
  :ensure nil
  :config
  ;; https://github.com/jwiegley/use-package/issues/152
  ;; Edebug a defun or defmacro
  (defvar modi/fns-in-edebug nil
    "List of functions for which `edebug' is instrumented.")

  (defconst modi/fns-regexp (concat "(\\s-*"
                                    "\\(defun\\|defmacro\\)\\s-+"
                                    "\\(?1:\\(\\w\\|\\s_\\)+\\)\\_>")
    "Regexp to find defun or defmacro definition.")

  (defun modi/toggle-edebug-defun ()
    (interactive)
    (let (fn)
      (save-mark-and-excursion
        (search-backward-regexp modi/fns-regexp)
        (setq fn (match-string 1))
        (mark-sexp)
        (narrow-to-region (point) (mark))
        (if (member fn modi/fns-in-edebug)
            ;; If the function is already being edebugged, uninstrument it
            (progn
              (setq modi/fns-in-edebug (delete fn modi/fns-in-edebug))
              (eval-region (point) (mark))
              (setq-default eval-expression-print-length 12)
              (setq-default eval-expression-print-level  4)
              (message "Edebug disabled: %s" fn))
          ;; If the function is not being edebugged, instrument it
          (progn
            (add-to-list 'modi/fns-in-edebug fn)
            (setq-default eval-expression-print-length nil)
            (setq-default eval-expression-print-level  nil)
            (edebug-defun)
            (message "Edebug: %s" fn)))
        (widen)))))

(use-package elisp-slime-nav
  :ensure t
  :diminish elisp-slime-nav-mode
  :commands (turn-on-elisp-slime-nav-mode)
  :init
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook 'turn-on-elisp-slime-nav-mode))
  :config
  (turn-on-elisp-slime-nav-mode))

(use-package eval-sexp-fu
  :ensure t
  :commands (eval-sexp-fu-flash-mode)
  :init
  (add-hook 'cider-mode-hook 'eval-sexp-fu-flash-mode)
  (add-hook 'clojure-mode-hook 'eval-sexp-fu-flash-mode)
  (add-hook 'emacs-lisp-mode-hook 'eval-sexp-fu-flash-mode)
  :config
  (eval-sexp-fu-flash-mode 1))

(use-package elixir-mode
  :ensure t
  :mode
  ("\\.elixir\\'" . elixir-mode)
  ("\\.ex\\'" . elixir-mode)
  ("\\.exs\\'" . elixir-mode)
  :init
  (add-hook 'elixir-mode-hook
            (lambda ()
              (setq tab-width 2)
              (after-evil
               (setq evil-shift-width 2)))))

(use-package alchemist
  :ensure t
  :commands alchemist-mode
  :init
  (add-hook 'elixir-mode-hook #'alchemist-mode)
  (add-hook 'alchemist-mode-hook
            (lambda ()
              (jojo/company-merge-backends)) t)
  :config
  (setq alchemist-test-ask-about-save nil)
  (setq alchemist-goto-elixir-source-dir "~/.source/elixir/elixir-1.4.1")
  (setq alchemist-goto-erlang-source-dir "~/.source/erlang/otp_src_19.2")

  ;; Erlang synergy
  (defun jojo/elixir-erlang-pop-back ()
    "Pop back definition function for Erlang mode."
    (interactive)
    (if (ring-empty-p erl-find-history-ring)
        (alchemist-goto-jump-back)
      (erl-find-source-unwind)))

  (defun jojo/alchemist-erlang-mode-hook ()
    (define-key erlang-mode-map (kbd "M-,") 'jojo/elixir-erlang-pop-back))

  (add-hook 'erlang-mode-hook 'jojo/alchemist-erlang-mode-hook))

(use-package web-mode
  :ensure t
  :mode
  ("\\.phtml\\'" . web-mode)
  ("\\.tpl\\.php\\'" . web-mode)
  ("\\.blade\\.php\\'" . web-mode)
  ("/\\(views\\|html\\|theme\\|templates\\)/.*\\.php\\'" . web-mode)
  ("\\.[agj]sp\\'" . web-mode)
  ("\\.as[cp]x\\'" . web-mode)
  ("\\.erb\\'" . web-mode)
  ("\\.mustache\\'" . web-mode)
  ("\\.djhtml\\'" . web-mode)
  ("\\.jsp\\'" . web-mode)
  ("\\.eex\\'" . web-mode)
  :init
  (add-hook 'web-mode-hook
            (lambda ()
              (when t
                (setq-local web-mode-markup-indent-offset 2)
                (setq-local web-mode-css-indent-offset 2)
                (setq-local web-mode-code-indent-offset 2)))))

;; colors for various 'color codes' aka hex strings
(use-package rainbow-mode
  :ensure t
  :commands (rainbow-mode)
  :init
  (add-hook 'css-mode-hook
            (lambda ()
              ;; This 2 spaces check could go in a css mode package.
              ;; Adding it here for now out of laziness.
              (when t
                (setq css-indent-offset 2))
              (rainbow-mode)))
  :diminish rainbow-mode
  :config
  (rainbow-mode 1))

(use-package js2-mode
  :ensure t
  :mode
  ("\\.js\\'" . js2-mode)
  ;; ("\\.jsx?\\'" . js2-jsx-mode)
  :interpreter
  ("node" . js2-mode)
  ;; ("node" . js2-jsx-mode)
  :config
  (setq js2-highlight-level 3)
  ;; (use-package xref-js2) Look into this for Emacs 25.

  ;; https://www.reddit.com/r/emacs/comments/4xiaym/packages_for_javascript/
  ;; js2-mode
  ;; js2-refactor for refactorings
  ;; xref-js2 for navigation to references/definitions
  ;; jade for a REPL, inspector & stepping debugger
  ;; company-tern for autocompletion

  (add-hook 'js2-mode-hook (lambda ()
                             (when t
                               (setq js2-basic-offset 2)))))

(use-package ac-js2
  :ensure t
  :commands
  (ac-js2-mode)
  :init
  (defun jojo/ac-js2-hook ()
    "Sets up ac-js2."
    (ac-js2-mode)
    (jojo/company-push-backend-local 'ac-js2-company))
  (add-hook 'js2-mode-hook #'jojo/ac-js2-hook))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  ;; Double undo limit
  (setq undo-limit 160000)
  (setq undo-strong-limit 240000)
  (setq undo-outer-limit 24000000)
  (global-undo-tree-mode))

;; Figure out how to bind 'ff-find-other-file

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

(use-package irony
  ;; Run ~/.emacs.d/tools/irony_setup.sh
  :ensure t
  :commands (irony-mode irony-install-server)
  :init
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package company-irony
  :ensure t
  :commands (company-irony)
  :init
  (setq company-irony-ignore-case t)
  (defun jojo/irony-mode-hook ()
    "Hook for irony mode."
    (jojo/company-push-backend-local '(company-irony-c-headers company-irony))
    (jojo/company-set-delay 0)
    (jojo/company-set-prefix-length 1))
  (add-hook 'irony-mode-hook #'jojo/irony-mode-hook))

(use-package company-irony-c-headers
  :ensure t
  :commands (company-irony-c-headers))

(use-package flycheck-irony
  :ensure t
  :commands (flycheck-irony-setup)
  :init
  (add-hook 'irony-mode-hook
            (lambda ()
              (eval-after-load 'flycheck
                '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))))

(setq mouse-wheel-scroll-amount '(1))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)

(setq mac-mouse-wheel-mode nil)
(setq mac-mouse-wheel-smooth-scroll nil)

(use-package lua-mode
  :ensure t
  :mode ("\\.lua\\'" . lua-mode)
  :interpreter ("lua" . lua-mode)
  :config
  (setq lua-indent-level 2)

  (defun jojo/lua-run-test-suite ()
    "Run test_suite.lua."
    (interactive)
    (let ((default-directory (locate-dominating-file
                              (file-name-directory buffer-file-name)
                              "test_suite.lua")))
      (compilation-start
       "lua test_suite.lua -v"
       'compilation-mode
       (lambda (_mode-name)
         "*lua test results*")
       t)))

  (defun jojo/lua-run-test-file ()
    "Run test file using buffer as file."
    (interactive)
    (if-let ((buffer-file (buffer-file-name)))
        (let ((default-directory (locate-dominating-file
                                  (file-name-directory buffer-file-name)
                                  "main.lua")))
          (compilation-start (format "lua %s -v" buffer-file)
                             'compilation-mode
                             (lambda (_mode-name)
                               "*lua test results*")
                             t))
      (message "`buffer-file-name' is nil.")))

  (defun jojo/lua-run-test-at-point ()
    "Run test at point."
    (interactive)
    (if-let ((buffer-file (buffer-file-name)))
        (let ((function-name
               (let ((current-line (thing-at-point 'line t)))
                 (progn
                   (unless (string-match-p "function" current-line)
                     (search-backward "function"))
                   (let ((new-current-line (s-trim (thing-at-point 'line t))))
                     (s-trim
                      (s-chop-suffix
                       "()"
                       (s-chop-prefix "function" new-current-line))))))))
          (if function-name
              (let ((default-directory (locate-dominating-file
                                        (file-name-directory buffer-file-name)
                                        "main.lua")))
                (compilation-start
                 (format "lua %s %s -v" buffer-file (s-replace ":" "." function-name))
                 'compilation-mode
                 (lambda (_mode-name)
                   "*lua test results*")
                 t))
            (message "Couldn't find `function-name'.")))
      (message "`buffer-file-name' is nil.")))

  (defun pd/love-run ()
    "Run pdrun script in root of project."
    (interactive)
    (let ((default-directory
            (locate-dominating-file default-directory "pdrun")))
      (with-current-buffer
          (get-buffer-create "*compilation*")
        (compilation-start "./pdrun" 'compilation-mode
                           (lambda (_mode-name)
                             "*pdrun*")
                           t)))))

(use-package company-lua
  :ensure t
  :commands (company-lua)
  :init
  (add-hook 'lua-mode-hook
            (lambda ()
              (setq company-lua-interpreter 'love)
              (jojo/company-push-backend-local 'company-lua))))

(use-package love-minor-mode
  :ensure t
  :commands (love/possibly-enable-mode)
  :init
  (defcustom love/documentation-url
    "https://love2d.org/wiki/"
    "URL pointing to the Love wiki."
    :type 'string
    :group 'lua)

  (defun love/search-documentation ()
    "Search Love documentation for the word at the point."
    (interactive)
    (let ((url (concat love/documentation-url (lua-funcname-at-point))))
      (funcall lua-documentation-function url)))

  (add-hook 'lua-mode-hook
            (lambda ()
              (love/possibly-enable-mode))))

(use-package smartparens
  :ensure t
  :config
  (use-package smartparens-config :ensure nil)
  (smartparens-global-mode 1))

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key [f8] 'neotree-toggle)

;; Right Alt -> Control
(setq ns-right-option-modifier 'control)

(defun indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (unless indent-tabs-mode
    (untabify (point-min) (point-max))))

(defun indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (unless indent-tabs-mode
            (untabify (point-min) (point-max)))
          (message "Indented selected region."))
      (progn
        (indent-buffer)
        (message "Indented buffer.")))))

(use-package malinka
  :ensure t
  :commands (malinka-mode)
  :init
  (add-hook 'c++-mode-hook 'malinka-mode)
  :config
  (setq malinka-idle-project-check-seconds 3)

  (malinka-define-project
   :name "flappy2"
   :root-directory "~/Code/flappyworld/c/flappy"
   :build-directory "~/Code/flappyworld/c/flappy/build"
   :configure-cmd "cmake .. -DCMAKE_BUILD_TYPE=Debug -DHEADLESS=1"
   :compile-cmd "make -j4"))

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

(use-package rtags
  :ensure t
  :commands (rtags-start-process-unless-running rtags-install)
  :config
  (setq rtags-jump-to-first-match nil)
  (setq rtags-tracking t)
  (setq rtags-tracking-timer-interval .3)
  (setq rtags-display-result-backend 'default)
  (setq rtags-autostart-diagnostics t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("12ab69d8db952a79aa079da812c147ae551f6427448f2564e29f910bcef7bf93" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "1db337246ebc9c083be0d728f8d20913a0f46edc0a00277746ba411c149d7fe5" "f5512c02e0a6887e987a816918b7a684d558716262ac7ee2dd0437ab913eaec6" "8abee8a14e028101f90a2d314f1b03bed1cde7fd3f1eb945ada6ffc15b1d7d65" "ecb9fe1d5b165a35499191a909b2b5710a52935614058b327a39bfbbb07c7dc8" "4f2ede02b3324c2f788f4e0bad77f7ebc1874eff7971d2a2c9b9724a50fb3f65" "98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" "5dc0ae2d193460de979a463b907b4b2c6d2c9c4657b2e9e66b8898d2592e3de5" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" default)))
 '(package-selected-packages
   (quote
    (flycheck-rtags irony-eldoc clang-format company-ycmd flycheck-ycmd malinka ample-zen-theme zenburn-theme tango-2-theme tango-plus-theme heroku-theme material-theme smartparens solarized-theme eval-sexp-fu projectile magit exec-path-from-shell nlinum s dash use-package))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
