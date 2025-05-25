;;; nh-theme.el --- Theme configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for themes and visual appearance.

;;; Code:

(require 'nh-helpers)

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

(set-face-attribute 'default nil :font "Inconsolata for Powerline" :height 128)

;; Required for Circadian theme switching
(use-package solarized-theme
  :ensure t
  :init
  (setq solarized-distinct-fringe-background t)
  (setq solarized-use-less-bold t))

;; Switch Solarized theme based on sunrise/sunset
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
  ;; Make modeline taller, use a modern font, and add a subtle border.
  (dolist (sym '(mode-line mode-line-inactive))
    (set-face-attribute
     sym nil
     :height 120
     :font "Inconsolata for Powerline"
     :box `(:line-width 4 :color ,(face-attribute sym :background))))
  ;; Org-mode tweaks
  (with-eval-after-load 'org-faces
    (set-face-background 'org-hide (face-attribute 'default :background))
    (set-face-foreground 'org-hide (face-attribute 'default :background)))
  (set-face-attribute 'fringe nil
                      :background (face-attribute 'default :background))
  ;; Modernize line numbers (if using display-line-numbers-mode)
  (when (boundp 'line-number-current-line)
    (set-face-attribute 'line-number nil :inherit 'default :foreground 'unspecified :background 'unspecified)
    (set-face-attribute 'line-number-current-line nil :inherit 'default :foreground 'unspecified :background 'unspecified :weight 'bold)))

(add-hook 'after-load-theme-hook #'nh/update-theme)

;; Colorful and bold parentheses for all Lisp modes
(use-package rainbow-delimiters
  :ensure t
  :commands (rainbow-delimiters-mode)
  :init
  ;; Bold the parens for all depths
  (defun nh/bold-rainbow-parens ()
    "Make rainbow delimiters bold for all depths that exist."
    (let ((colors '("#7f8c8d" "#e74c3c" "#f1c40f" "#2ecc71" "#3498db" "#9b59b6" "#1abc9c" "#e67e22" "#e84393" "#636e72" "#fdcb6e" "#00b894")))
      (dotimes (i (length colors))
        (let ((face (intern (format "rainbow-delimiters-depth-%d-face" (1+ i)))))
          (when (facep face)
            (set-face-attribute face nil :bold t :foreground (nth i colors)))))))
  ;; Ensure bolding and colors are applied after theme changes
  (add-hook 'after-load-theme-hook #'nh/bold-rainbow-parens)
  ;; Enable rainbow-delimiters-mode in all Lisp-related modes
  (dolist (hook (nh/lisp-hooks))
    (add-hook hook #'rainbow-delimiters-mode))
  :config
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                      :foreground "red"
                      :background nil
                      :weight 'bold
                      :underline t)
  (nh/bold-rainbow-parens))

;; Emacs default
(use-package paren
  :ensure nil
  :config
  (show-paren-mode t))

;; Highlight all levels of parentheses around point for extra visual feedback
(use-package highlight-parentheses
  :ensure t
  :commands (highlight-parentheses-mode)
  :init
  ;; Enable highlight-parentheses-mode in all Lisp-related modes
  (dolist (hook (nh/lisp-hooks))
    (add-hook hook #'highlight-parentheses-mode)))

;;  Structural editing for parentheses and more
(use-package smartparens
  :ensure t
  :config
  ;; Load the default smartparens config
  (require 'smartparens-config)
  ;; Enable Smartparens globally
  (smartparens-global-mode 1)
  ;; Highlight matching pairs
  (show-smartparens-global-mode 1)
  ;; Don't autopair single quotes (common in Lisp, Python, etc.)
  (sp-pair "'" nil :actions :rem)
  ;; Recommended: strict mode in Lisp modes for structural editing
  (dolist (hook (nh/lisp-hooks))
    (add-hook hook #'smartparens-strict-mode))
  ;; Keybindings for common structural editing actions
  (define-key smartparens-mode-map (kbd "C-M-f") 'sp-forward-sexp)
  (define-key smartparens-mode-map (kbd "C-M-b") 'sp-backward-sexp)
  (define-key smartparens-mode-map (kbd "C-M-d") 'sp-down-sexp)
  (define-key smartparens-mode-map (kbd "C-M-a") 'sp-backward-down-sexp)
  (define-key smartparens-mode-map (kbd "C-S-d") 'sp-beginning-of-sexp)
  (define-key smartparens-mode-map (kbd "C-S-a") 'sp-end-of-sexp)
  (define-key smartparens-mode-map (kbd "C-M-e") 'sp-up-sexp)
  (define-key smartparens-mode-map (kbd "C-M-u") 'sp-backward-up-sexp)
  (define-key smartparens-mode-map (kbd "C-M-t") 'sp-transpose-sexp)
  (define-key smartparens-mode-map (kbd "C-M-n") 'sp-next-sexp)
  (define-key smartparens-mode-map (kbd "C-M-p") 'sp-previous-sexp)
  (define-key smartparens-mode-map (kbd "C-M-k") 'sp-kill-sexp)
  (define-key smartparens-mode-map (kbd "C-M-w") 'sp-copy-sexp)
  (define-key smartparens-mode-map (kbd "C-M-<backspace>") 'sp-splice-sexp)
  (define-key smartparens-mode-map (kbd "C-M-<delete>") 'sp-splice-sexp-killing-forward)
  (define-key smartparens-mode-map (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward))

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

;; Make buffer names unique by appending directory names
(use-package uniquify
  :ensure nil  ;; Built-in package, no need to install
  :config
  (setq uniquify-buffer-name-style 'reverse)  ;; Show directory after filename
  (setq uniquify-separator "|")              ;; Use | as separator
  (setq uniquify-after-kill-buffer-p t)       ;; Rename buffers after killing
  (setq uniquify-ignore-buffers-re "^\\*")   ;; Ignore special buffers
)

;; Highlight occurrences of the symbol at point in code
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

(use-package gotham-theme :defer :ensure t)
(use-package spacemacs-theme :defer :ensure t)

(provide 'nh-theme)

;;; nh-theme.el ends here 
