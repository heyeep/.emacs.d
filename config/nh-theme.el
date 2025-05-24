;;; nh-theme.el --- Theme configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for themes and visual appearance.

;;; Code:

(provide 'nh-theme)

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

(set-face-attribute 'default nil :font "Inconsolata for Powerline" :height 128)

;; Diminish modeline clutter.
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

;; Circadian: Switch Solarized theme based on sunrise/sunset
(use-package circadian
  :ensure t
  :config
  (setq circadian-themes '((:sunrise . solarized-light)
                           (:sunset  . solarized-dark)))
  (setq calendar-latitude 37.8044)
  (setq calendar-longitude -122.2711)
  (circadian-setup))

(defun nh/update-theme ()
  "Update various UI elements when theme changes."
  ;; Make modeline taller
  (dolist (sym '(mode-line mode-line-inactive))
    (set-face-attribute
     sym nil
     :box `(:line-width 5 :color ,(face-attribute sym :background))))
  ;; Org-mode tweaks
  (with-eval-after-load 'org-faces
    (set-face-background 'org-hide (face-attribute 'default :background))
    (set-face-foreground 'org-hide (face-attribute 'default :background)))
  ;; Company-mode tweaks
  (with-eval-after-load 'company
    (set-face-attribute
     'company-preview
     nil
     :background (face-attribute 'company-preview-common :background))))

(add-hook 'after-load-theme-hook #'nh/update-theme)

;; Rainbow delimiters for all Lisp modes
(use-package rainbow-delimiters
  :ensure t
  :hook ((emacs-lisp-mode lisp-mode lisp-interaction-mode scheme-mode) . rainbow-delimiters-mode))

;;; nh-theme.el ends here 
