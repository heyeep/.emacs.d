;;; nh-theme.el --- Theme configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for themes and visual appearance.

;;; Code:

(require 'nh-helpers)

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

(set-face-attribute 'default nil :font "Inconsolata for Powerline" :height 128)


;; Gotham Theme: A very dark Emacs theme
;; Dark theme inspired by Batman's Gotham City with muted colors and excellent
;; contrast for comfortable coding in low-light environments.
;; GitHub: https://github.com/wasamasa/gotham-theme
(use-package gotham-theme :defer :ensure t)

;; Spacemacs Theme: Color themes megapack for Emacs
;; Collection of beautiful themes inspired by Spacemacs with both dark and light
;; variants, providing modern aesthetics and excellent syntax highlighting.
;; GitHub: https://github.com/nashamri/spacemacs-theme
(use-package spacemacs-theme :defer :ensure t)

;; Solarized Theme: The Solarized colour theme
;; Precision color scheme with carefully balanced colors for both dark and light
;; backgrounds, designed to reduce eye strain and improve readability.
;; GitHub: https://github.com/bbatsov/solarized-emacs
(use-package solarized-theme
  :ensure t
  :defer t)


;; Circadian: Theme-switching based on daytime
;; Automatically switches between light and dark themes based on sunrise and
;; sunset times for your location, providing optimal viewing comfort throughout the day.
;; GitHub: https://github.com/guidoschmidt/circadian.el
(use-package circadian
  :ensure t
  :after solarized-theme
  :config
  ;; Set your location for sunrise/sunset calculations
  (setq calendar-location-name "San Francisco, CA")
  (setq calendar-latitude 37.7749)
  (setq calendar-longitude -122.4194)

  ;; Configure themes for day/night
  (setq circadian-themes '(("8:00" . solarized-light)
                           ("19:30" . solarized-dark)))
  ;; Enable circadian mode
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

;; Rainbow Delimiters: Color-coding for parentheses and brackets
;; Highlights matching parentheses, brackets, and braces with different colors
;; based on nesting depth, making code structure more visually apparent.
;; GitHub: https://github.com/Fanael/rainbow-delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Paren: Built-in parentheses highlighting
;; Built-in package for highlighting matching parentheses with customizable
;; styles and colors to improve code readability and bracket matching.
(use-package paren
  :ensure nil
  :config
  (show-paren-mode 1)
  (setq show-paren-delay 0)
  (setq show-paren-style 'expression))

;; Highlight Parentheses: Highlight surrounding parentheses
;; Highlights all levels of parentheses around point with different colors,
;; providing continuous visual feedback about code structure and nesting.
;; GitHub: https://github.com/tsdh/highlight-parentheses.el
(use-package highlight-parentheses
  :ensure t
  :hook (prog-mode . highlight-parentheses-mode))

;; Smartparens: Minor mode for dealing with pairs in Emacs
;; Intelligent handling of parentheses, brackets, quotes, and other paired
;; characters with automatic insertion, deletion, and navigation commands.
;; GitHub: https://github.com/Fuco1/smartparens
(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config)
  (sp-use-paredit-bindings))

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

;; Uniquify: Unique buffer names by directory
;; Built-in package that makes buffer names unique by appending directory names
;; when multiple files with the same name are open, improving buffer identification.
(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*"))

;; Highlight Symbol: Automatic highlighting of symbol at point
;; Automatically highlights all occurrences of the symbol at point throughout
;; the buffer, helping to track variable usage and code flow.
;; GitHub: https://github.com/nschum/highlight-symbol.el
(use-package highlight-symbol
  :ensure t
  :diminish highlight-symbol-mode
  :config
  (setq highlight-symbol-idle-delay 0.5)
  (setq highlight-symbol-on-navigation-p t)

  (defun nh/enable-highlight-symbol-mode ()
    "Enable highlight-symbol-mode in programming modes."
    (highlight-symbol-mode 1))

  :hook
  (prog-mode . nh/enable-highlight-symbol-mode))

(provide 'nh-theme)

;;; nh-theme.el ends here
