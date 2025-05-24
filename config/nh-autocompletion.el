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
  (setq ivy-format-function #'ivy-format-function-fancy)
  (setq ivy-display-style 'fancy)
  (setq ivy-count-format "")      ;; Hide candidate count
  (setq ivy-height 15)             ;; Show up to 15 candidates
  ;; Swap RET and C-j: RET does ivy-alt-done, C-j does ivy-done
  (define-key ivy-minibuffer-map (kbd "RET") 'ivy-alt-done)
  (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-done)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)) ;; Resume last Ivy session

;; Counsel: Ivy-enhanced versions of common Emacs commands
(use-package counsel
  :ensure t
  :after ivy
  :config
  (counsel-mode 1)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "s-x") 'counsel-M-x)
  (global-set-key (kbd "C-c r") 'counsel-recentf) ;; Quick access to recent files
  (setq counsel-find-file-at-point t)) ;; Enable preview for counsel-find-file

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

(provide 'nh-autocompletion)
;;; nh-autocompletion.el ends here
