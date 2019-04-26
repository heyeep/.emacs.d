;;;; -*- lexical-binding: t; -*-

(use-package htmlize
  :commands (htmlize-buffer
             htmlize-region
             htmlize-file
             htmlize-many-files
             htmlize-many-files-dired)
  :ensure t)

(use-package org
  :ensure org-plus-contrib
  :mode ("\\.org\\'" . org-mode)

  :init
  (defun +set-up-writing-conditionally ()
    "Set up writing conditionally."
    (when (string-equal (buffer-name) "M.org.gpg")
      (turn-on-auto-fill)
      (set-fill-column 80)
      (define-key evil-normal-state-local-map
        (kbd "<return>") #'org-tree-to-indirect-buffer)
      (define-key evil-normal-state-local-map
        (kbd "RET") #'org-tree-to-indirect-buffer)
      (define-key evil-normal-state-local-map
        [return] #'org-tree-to-indirect-buffer)))

  (defun +customize-org-ui ()
    "Customize various Org Mode UI Elements"
    (set-face-attribute 'org-document-title nil :weight 'bold :height 1.4)
    (set-face-attribute 'org-level-1 nil :inherit 'outline-1 :height 1.3 :weight 'bold)
    (set-face-attribute 'org-level-2 nil :inherit 'outline-2 :height 1.2 :weight 'bold)
    (set-face-attribute 'org-level-3 nil :inherit 'outline-3 :height 1.1)
    (set-face-attribute 'org-level-4 nil :inherit 'outline-4 :height 1.0))

  (add-hook 'org-mode-hook
            (lambda ()
              (+set-up-writing-conditionally)
              (+customize-org-ui)))
  :config
  (require 'ox-odt) ;; Open Document Format

  (require 'ob-dot) ;; Graphviz
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

  (require 'org-notmuch)
  (with-eval-after-load 'evil
    (evil-define-key 'normal org-mode-map
      (kbd "M-.") 'org-open-at-point
      [tab] #'indent-org-block-automatically-or-cycle
      (kbd "TAB") #'indent-org-block-automatically-or-cycle))
    (setq evil-shift-width 4))
  ;; (defun indent-org-block-automatically-or-cycle ()
  ;;   "Indent source code in source blocks."
  ;;   (interactive)
  ;;   (if (org-in-src-block-p)
  ;;       (progn
  ;;         (org-edit-special)
  ;;         (indent-region (point-min) (point-max))
  ;;         (org-edit-src-exit))
  ;;     (call-interactively #'org-cycle)))

;;   (setq org-export-backends '(ascii html icalendar latex md))
;;   (setq org-src-fontify-natively t)
;;   (setq org-src-preserve-indentation nil
;;         org-edit-src-content-indentation 0)
;; ;;  (setq org-src-tab-acts-natively t)
;;   (setq org-hide-leading-stars t)
;;   (setq org-hide-emphasis-markers t)
;;   (setq org-goto-interface 'outline-path-completion
;;         org-goto-max-level 10)
;;   (setq org-image-actual-width nil))

(use-package graphviz-dot-mode
  :ensure t
  :mode
  ("\\.dot\\'" . graphviz-dot-mode)
  ("\\.gv\\'" . graphviz-dot-mode)
  :init
  (setq default-tab-width 4))

(provide 'hpd-org)
