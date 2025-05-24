;;; nh-theme.el --- Theme configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for themes and visual appearance.

;;; Code:

(provide 'nh-theme)

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

(set-face-attribute 'default nil :font "Inconsolata for Powerline" :height 128)

;; Solarized Theme: Required for Circadian theme switching
(use-package solarized-theme :defer :ensure t
  :config
  (setq solarized-distinct-fringe-background t)
  (setq solarized-use-less-bold t))

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

;; Uniquify: Make buffer names unique by appending directory names
(use-package uniquify
  :ensure nil  ;; Built-in package, no need to install
  :config
  (setq uniquify-buffer-name-style 'reverse)  ;; Show directory after filename
  (setq uniquify-separator "|")              ;; Use | as separator
  (setq uniquify-after-kill-buffer-p t)       ;; Rename buffers after killing
  (setq uniquify-ignore-buffers-re "^\\*")   ;; Ignore special buffers
)

;; highlight-symbol: Highlight occurrences of the symbol at point in code
(use-package highlight-symbol
  :ensure t
  :diminish highlight-symbol-mode
  :defer 5
  :custom
  (highlight-symbol-idle-delay 0.5)
  :config
  ;; Make highlight-symbol-face look like the standard highlight face
  (defun nh/highlight-symbol-face ()
    (set-face-attribute 'highlight-symbol-face nil
                        :background nil
                        :foreground nil
                        :inherit 'highlight))

  ;; Set face after theme changes
  (add-hook 'after-load-theme-hook #'nh/highlight-symbol-face)

  ;; Enable highlight-symbol-mode in all programming modes except typescript
  (defun nh/enable-highlight-symbol-mode ()
    (unless (member major-mode '(typescript-mode))
      (nh/highlight-symbol-face)
      (highlight-symbol-mode 1)))
  :hook
  (prog-mode . nh/enable-highlight-symbol-mode))

;;; nh-theme.el ends here 
