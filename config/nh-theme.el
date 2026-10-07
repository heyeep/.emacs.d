;;; nh-theme.el --- theme -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'nh-helpers)

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; macOS: let the title bar take the theme's colors.
(when (memq window-system '(mac ns))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))

(set-face-attribute 'default nil
                    :font (font-spec :family "Iosevka Etoile"
                                     :size 12
                                     :weight 'normal
                                     ))

;; gotham-theme: a very dark, low-contrast theme.
;; GitHub: https://github.com/wasamasa/gotham-theme
(use-package gotham-theme :defer :ensure t)

;; spacemacs-theme: the light and dark themes from Spacemacs.
;; GitHub: https://github.com/nashamri/spacemacs-theme
(use-package spacemacs-theme :defer :ensure t)

;; solarized-theme: the Solarized light and dark themes.
;; GitHub: https://github.com/bbatsov/solarized-emacs
(use-package solarized-theme
  :ensure t
  :init
  (setq solarized-distinct-fringe-background t)
  (setq solarized-use-less-bold t))

;; modus-themes: high-contrast light and dark themes.
;; GitHub: https://github.com/protesilaos/modus-themes
(use-package modus-themes
  :ensure t
  :config
  ;; These must be set before a modus theme loads.
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-mixed-fonts t
        modus-themes-variable-pitch-ui t
        modus-themes-fringes nil
        modus-themes-org-blocks 'gray-background
        modus-themes-paren-match '(bold intense)
        modus-themes-region '(bg-only accented)
        modus-themes-hl-line '(accented)
        modus-themes-completions
        '((matches . (extrabold underline))
          (selection . (semibold italic)))
        modus-themes-mode-line '(accented 3d padded moody)
        modus-themes-diffs 'desaturated
        modus-themes-markup '(bold italic)
        modus-themes-subtle-line-numbers t)

  (setq modus-themes-common-palette-overrides
        '(;; Give the fringe the normal background.
          (fringe unspecified)
          (bg-line-number-inactive unspecified)
          (bg-line-number-active bg-hover)
          (fg-line-number-inactive fg-dim)
          (fg-line-number-active fg-main)
          (fg-completion-match-0 blue)
          (fg-completion-match-1 magenta-warmer)
          (fg-completion-match-2 cyan)
          (fg-completion-match-3 red)
          (bg-completion-match-0 bg-blue-nuanced)
          (bg-completion-match-1 bg-magenta-nuanced)
          (bg-completion-match-2 bg-cyan-nuanced)
          (bg-completion-match-3 bg-red-nuanced))))


;; circadian: switches between light and dark themes at sunrise and sunset.
;; GitHub: https://github.com/guidoschmidt/circadian.el
(use-package circadian
  :ensure t
  :config
  (setq circadian-themes '((:sunrise . modus-operandi-tinted)
                           (:sunset  . modus-operandi-tinted)))
  (setq calendar-latitude 37.8044)
  (setq calendar-longitude -122.2711)
  (circadian-setup))

;; solaire-mode: gives file buffers a slightly different background from popups and sidebars.
;; GitHub: https://github.com/hlissner/emacs-solaire-mode
(use-package solaire-mode
  :ensure t
  :config
  (solaire-global-mode +1))

(defun nh/modus-themes-solaire-faces (&rest _)
  "Set custom solaire-mode faces for modus themes."
  (modus-themes-with-colors
    (custom-set-faces
     `(solaire-default-face ((,c :inherit default :background ,bg-dim :foreground ,fg-dim)))
     `(solaire-line-number-face ((,c :inherit solaire-default-face :foreground ,fg-dim)))
     `(solaire-hl-line-face ((,c :background ,bg-active)))
     `(solaire-org-hide-face ((,c :background ,bg-dim :foreground ,bg-dim))))))

;; Circadian calls plain `load-theme', which skips the modus-themes hook.
(add-hook 'enable-theme-functions
          (lambda (theme)
            (when (string-prefix-p "modus-" (symbol-name theme))
              (nh/modus-themes-solaire-faces))))

(defun nh/update-theme ()
  "Update various UI elements when theme change."
  ;; Dark title bar for dark themes, light for light ones.
  (when (memq window-system '(mac ns))
    (let* ((bg-color (face-attribute 'default :background))
           (is-dark (< (apply '+ (color-values bg-color))
                      (* 0.5 (apply '+ (color-values "white"))))))
      (modify-all-frames-parameters
       (list (cons 'ns-appearance (if is-dark 'dark 'light))))))
  ;; A border in the mode line's own color makes it taller.
  (dolist (sym '(mode-line mode-line-inactive))
    (set-face-attribute
     sym nil
     :height 120
     :font "Iosevka Etoile"
     :box `(:line-width 4 :color ,(face-attribute sym :background))))
  (with-eval-after-load 'org-faces
    (set-face-background 'org-hide (face-attribute 'default :background))
    (set-face-foreground 'org-hide (face-attribute 'default :background)))
  ;; Themes often shade the fringe; keep it the same as the text area.
  (set-face-attribute 'fringe nil
                      :inherit 'default
                      :background 'unspecified
                      :foreground 'unspecified)
  (when (fboundp 'display-line-numbers-mode)
    (let ((subtle-fg (face-attribute 'shadow :foreground))
          (highlight-bg (face-attribute 'highlight :background)))
      (set-face-attribute 'line-number nil
                          :background (face-attribute 'default :background)
                          :foreground subtle-fg)
      (set-face-attribute 'line-number-current-line nil
                          :background highlight-bg
                          :foreground (face-attribute 'default :foreground)
                          :weight 'bold
                          :extend t)))
  (setq-default display-line-numbers-width-start t)
  )

(defvar after-load-theme-hook nil
  "Hook run after a theme is enabled.")

;; Emacs has no `after-load-theme-hook'; run it from the built-in one.
(add-hook 'enable-theme-functions
          (lambda (_theme) (run-hooks 'after-load-theme-hook)))

(add-hook 'after-load-theme-hook #'nh/update-theme)

;; iTerm2's window padding sits outside the Emacs frame, so ask the terminal
;; itself to use the theme background (OSC 11) and restore it on exit (OSC 111).
;; Terminals can't take a font from Emacs, so switch iTerm2 to an "Emacs"
;; profile (Iosevka Term) first; a profile switch also resets the background.
(defun nh/sync-terminal-background (&rest _)
  "Use the Emacs iTerm2 profile and the theme background in the terminal."
  (unless (display-graphic-p)
    (when (equal (getenv "TERM_PROGRAM") "iTerm.app")
      (send-string-to-terminal "\e]1337;SetProfile=Emacs\a"))
    (let ((bg (face-attribute 'default :background)))
      (when (string-match-p "\\`#[[:xdigit:]]\\{6\\}\\'" bg)
        (send-string-to-terminal (format "\e]11;%s\a" bg))))))

(defun nh/reset-terminal-background ()
  "Restore the terminal's own profile and background."
  (unless (display-graphic-p)
    (send-string-to-terminal "\e]111\a")
    (when (equal (getenv "TERM_PROGRAM") "iTerm.app")
      (send-string-to-terminal
       (format "\e]1337;SetProfile=%s\a" (or (getenv "ITERM_PROFILE") "Default"))))))

(add-hook 'enable-theme-functions #'nh/sync-terminal-background)
(add-hook 'suspend-resume-hook #'nh/sync-terminal-background)
(add-hook 'suspend-hook #'nh/reset-terminal-background)
(add-hook 'kill-emacs-hook #'nh/reset-terminal-background)
(nh/sync-terminal-background)

;; rainbow-delimiters: colors each level of nested parentheses differently.
;; GitHub: https://github.com/Fanael/rainbow-delimiters
(use-package rainbow-delimiters
  :ensure t
  :commands (rainbow-delimiters-mode)
  :init
  (defun nh/bold-rainbow-parens ()
    "Make rainbow delimiters bold for all depths that exist."
    (let ((colors '("#7f8c8d" "#e74c3c" "#f1c40f" "#2ecc71" "#3498db" "#9b59b6" "#1abc9c" "#e67e22" "#e84393" "#636e72" "#fdcb6e" "#00b894")))
      (dotimes (i (length colors))
        (let ((face (intern (format "rainbow-delimiters-depth-%d-face" (1+ i)))))
          (when (facep face)
            (set-face-attribute face nil :bold t :foreground (nth i colors)))))))
  ;; Theme changes reset these faces, so reapply them.
  (add-hook 'after-load-theme-hook #'nh/bold-rainbow-parens)
  (dolist (hook (nh/lisp-hooks))
    (add-hook hook #'rainbow-delimiters-mode))
  :config
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                      :foreground "red"
                      :background 'unspecified
                      :weight 'bold
                      :underline t)
  (nh/bold-rainbow-parens))

;; Disabled: paren, Emacs's built-in matching-paren highlight.
;; (use-package paren
;;   :ensure nil
;;   :config
;;   (show-paren-mode t))

;; highlight-parentheses: highlights every pair of parentheses around point.
;; GitHub: https://github.com/tsdh/highlight-parentheses.el
(use-package highlight-parentheses
  :ensure t
  :diminish t
  :commands (highlight-parentheses-mode)
  :init
  (dolist (hook (nh/lisp-hooks))
    (add-hook hook #'highlight-parentheses-mode)))

;; Disabled: smartparens, structural editing for paired characters.
;; GitHub: https://github.com/Fuco1/smartparens
;; (use-package smartparens
;;   :ensure t
;;   :config
;;   ;; Load the default smartparens config
;;   (require 'smartparens-config)
;;   ;; Enable Smartparens globally
;;   (smartparens-global-mode 1)
;;   ;; Highlight matching pairs
;;   (show-smartparens-global-mode 1)
;;   ;; Don't autopair single quotes (common in Lisp, Python, etc.)
;;   (sp-pair "'" nil :actions :rem)
;;   ;; Recommended: strict mode in Lisp modes for structural editing
;;   (dolist (hook (nh/lisp-hooks))
;;     (add-hook hook #'smartparens-strict-mode))
;;   ;; Keybindings for common structural editing actions

;; (define-key smartparens-mode-map (kbd "C-M-f") 'sp-forward-sexp)
;;   (define-key smartparens-mode-map (kbd "C-M-b") 'sp-backward-sexp)
;;   (define-key smartparens-mode-map (kbd "C-M-d") 'sp-down-sexp)
;;   (define-key smartparens-mode-map (kbd "C-M-a") 'sp-backward-down-sexp)
;;   (define-key smartparens-mode-map (kbd "C-S-d") 'sp-beginning-of-sexp)
;;   (define-key smartparens-mode-map (kbd "C-S-a") 'sp-end-of-sexp)
;;   (define-key smartparens-mode-map (kbd "C-M-e") 'sp-up-sexp)
;;   (define-key smartparens-mode-map (kbd "C-M-u") 'sp-backward-up-sexp)
;;   (define-key smartparens-mode-map (kbd "C-M-t") 'sp-transpose-sexp)
;;   (define-key smartparens-mode-map (kbd "C-M-n") 'sp-next-sexp)
;;   (define-key smartparens-mode-map (kbd "C-M-p") 'sp-previous-sexp)
;;   (define-key smartparens-mode-map (kbd "C-M-k") 'sp-kill-sexp)
;;   (define-key smartparens-mode-map (kbd "C-M-w") 'sp-copy-sexp)
;;   (define-key smartparens-mode-map (kbd "C-M-<backspace>") 'sp-splice-sexp)
;;   (define-key smartparens-mode-map (kbd "C-M-<delete>") 'sp-splice-sexp-killing-forward)
;;   (define-key smartparens-mode-map (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward))

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

;; uniquify: adds the folder name when two buffers share a file name.
(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator "|")
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*")
)

;; highlight-symbol: highlights other uses of the symbol at point.
;; GitHub: https://github.com/nschum/highlight-symbol.el
(use-package highlight-symbol
  :ensure t
  :diminish highlight-symbol-mode
  :defer 5
  :custom
  (highlight-symbol-idle-delay 0.5)
  :config
  (defun nh/highlight-symbol-face ()
    (set-face-attribute 'highlight-symbol-face nil
                        :background 'unspecified
                        :foreground 'unspecified
                        :inherit 'highlight))

  (add-hook 'after-load-theme-hook #'nh/highlight-symbol-face)

  (defun nh/enable-highlight-symbol-mode ()
    (unless (member major-mode '(typescript-mode))
      (nh/highlight-symbol-face)
      (highlight-symbol-mode 1)))
  :hook
  (prog-mode . nh/enable-highlight-symbol-mode))

;; spacious-padding: adds space around windows and the mode line.
;; GitHub: https://github.com/protesilaos/spacious-padding
(use-package spacious-padding
  :ensure t
  :config
  (setq spacious-padding-widths
        '(:internal-border-width 8
          :header-line-width 8
          :mode-line-width 4
          :tab-width 4
          :right-divider-width 8
          :scroll-bar-width 4))
  ;; Don't let spacious-padding affect the fringe
  (setq spacious-padding-subtle-mode-line nil)
  (spacious-padding-mode 1))

;; powerline: a mode line with angled separators, like Vim's powerline.
;; GitHub: https://github.com/milkypostman/powerline
(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))

(setq-default mode-line-buffer-identification
              '(:eval (propertize "%b" 'face 'mode-line-buffer-id)))

;; Drop the "Git:" prefix from the branch name.
(advice-add 'vc-git-mode-line-string :filter-return
            (lambda (str)
              (when str
                (replace-regexp-in-string "^Git[:\-]" "" str))))

(setq mode-line-percent-position '(-3 "%p"))
(setq mode-line-position-column-line-format '(" %l:%c"))

;; Show Top, Bot or a percentage for the position in the buffer.
(setq mode-line-position
      '((:eval (if (>= (point) (point-max))
                   " Bot"
                 (if (<= (point) (point-min))
                     " Top"
                   (format " %d%%" (/ (- (point) (point-min)) 0.01
                                     (- (point-max) (point-min)))))))))

;; Show the file encoding only when it isn't UTF-8.
(setq-default mode-line-mule-info
              '(:eval (if (and buffer-file-coding-system
                               (eq buffer-file-coding-system 'utf-8-unix))
                          ""  ; Hide for UTF-8
                        " %z")))  ; Show for other encodings

;; Show an orange dot for unsaved changes instead of **.
(setq-default mode-line-modified
              '(:eval (if (buffer-modified-p)
                          (propertize "●" 'face '(:foreground "orange"))
                        "")))

;; Show only the major mode, not minor modes.
(setq mode-line-modes
      (list (propertize "%[" 'help-echo "Recursive edit, type C-M-c to get out")
            '(:eval (propertize (format-mode-line mode-name)
                                'face 'mode-line-buffer-id
                                'help-echo "Major mode"))
            (propertize "%]" 'help-echo "Recursive edit, type C-M-c to get out")
            " "))

(use-package diminish
  :ensure t
  :config
  ;; Built-in modes have no use-package block to put :diminish in.
  (with-eval-after-load 'eldoc (diminish 'eldoc-mode))
  (with-eval-after-load 'autorevert (diminish 'auto-revert-mode))
  (with-eval-after-load 'outline (diminish 'outline-minor-mode)))

;; Give the header line the normal background.
(custom-set-faces
 '(header-line ((t (:inherit default :background unspecified)))))

(provide 'nh-theme)

;;; nh-theme.el ends here
