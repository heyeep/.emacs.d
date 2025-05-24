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
(use-package company
  :ensure t
  :diminish company-mode
  :init
  (global-company-mode 1)
  :custom
  (company-idle-delay 0.1)
  (company-minimum-prefix-length 2)
  (company-tooltip-align-annotations t)
  (company-show-numbers t)
  (company-selection-wrap-around t))

(provide 'nh-autocompletion)
;;; nh-autocompletion.el ends here
