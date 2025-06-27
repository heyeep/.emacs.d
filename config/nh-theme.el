;;; nh-theme.el --- theme -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'nh-helpers)

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; Make title bar transparent and match the theme
(when (memq window-system '(mac ns))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . light)))

(set-face-attribute 'default nil
                    :font (font-spec :family "Iosevka Etoile"
                                     :size 12
                                     :weight 'normal
                                     ))  ; Force monospace

;; Gotham Theme: A very dark Emacs theme
;; Provides a dark, low-contrast color scheme inspired by Batman's Gotham City,
;; designed for comfortable long-term use with excellent syntax highlighting.
;; GitHub: https://github.com/wasamasa/gotham-theme
(use-package gotham-theme :defer :ensure t)

;; Spacemacs Theme: Color themes megapack for Emacs
;; Offers a collection of modern, colorful themes originally from Spacemacs,
;; providing both light and dark variants with vibrant syntax highlighting.
;; GitHub: https://github.com/nashamri/spacemacs-theme
(use-package spacemacs-theme :defer :ensure t)

;; Solarized Theme: The Solarized colour theme
;; Provides the popular Solarized color scheme with carefully balanced colors
;; designed to reduce eye strain and work well in both light and dark variants.
;; GitHub: https://github.com/bbatsov/solarized-emacs
(use-package solarized-theme
  :ensure t
  :init
  (setq solarized-distinct-fringe-background t)
  (setq solarized-use-less-bold t))

;; Modus Themes: Highly accessible themes for Emacs
;; Provides meticulously designed light and dark themes (modus-operandi and modus-vivendi)
;; that meet the highest accessibility standards (WCAG AAA) for color contrast.
;; GitHub: https://github.com/protesilaos/modus-themes
(use-package modus-themes
  :ensure t
  :config
  ;; Add customizations before loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-mixed-fonts t
        modus-themes-variable-pitch-ui t
        modus-themes-fringes nil  ; or 'subtle
        modus-themes-org-blocks 'gray-background
        modus-themes-paren-match '(bold intense)
        modus-themes-region '(bg-only accented)
        modus-themes-hl-line '(accented)
        modus-themes-completions
        '((matches . (extrabold underline))
          (selection . (semibold italic)))
        ;; Mode line customization
        modus-themes-mode-line '(accented 3d padded moody)
        ;; Diffs
        modus-themes-diffs 'desaturated
        ;; Markup (bold, italic in markdown, org, etc)
        modus-themes-markup '(bold italic)
        ;; Subtle line numbers
        modus-themes-subtle-line-numbers t)

  ;; Comprehensive palette overrides
  (setq modus-themes-common-palette-overrides
        '(;; Fix fringe to match background
          (fringe unspecified)
          ;; Line numbers
          (bg-line-number-inactive unspecified)
          (bg-line-number-active bg-hover)
          (fg-line-number-inactive fg-dim)
          (fg-line-number-active fg-main)
          ;; Completion matches - colorful highlighting
          (fg-completion-match-0 blue)
          (fg-completion-match-1 magenta-warmer)
          (fg-completion-match-2 cyan)
          (fg-completion-match-3 red)
          (bg-completion-match-0 bg-blue-nuanced)
          (bg-completion-match-1 bg-magenta-nuanced)
          (bg-completion-match-2 bg-cyan-nuanced)
          (bg-completion-match-3 bg-red-nuanced))))


;; Circadian: Theme-switching based on daytime
;; Automatically switches between light and dark themes based on sunrise and
;; sunset times, providing a natural rhythm that matches your daily schedule.
;; GitHub: https://github.com/guidoschmidt/circadian.el
(use-package circadian
  :ensure t
  :config
  (setq circadian-themes '((:sunrise . modus-operandi-tinted)
                           (:sunset  . modus-operandi-tinted)))
  (setq calendar-latitude 37.8044)
  (setq calendar-longitude -122.2711)
  (circadian-setup))

;; Solaire Mode: Distinguish "real" buffers from "special" buffers
;; Makes file-visiting buffers slightly brighter than special buffers like
;; sidebars, popup windows, and help buffers for better visual hierarchy.
;; GitHub: https://github.com/hlissner/emacs-solaire-mode
(use-package solaire-mode
  :ensure t
  :config
  ;; Enable solaire-mode in all buffers
  (solaire-global-mode +1))

;; Custom solaire-mode faces for modus themes
(defun nh/modus-themes-solaire-faces (&rest _)
  "Set custom solaire-mode faces for modus themes."
  (modus-themes-with-colors
    (custom-set-faces
     `(solaire-default-face ((,c :inherit default :background ,bg-dim :foreground ,fg-dim)))
     `(solaire-line-number-face ((,c :inherit solaire-default-face :foreground ,fg-dim)))
     `(solaire-hl-line-face ((,c :background ,bg-active)))
     `(solaire-org-hide-face ((,c :background ,bg-dim :foreground ,bg-dim))))))

(add-hook 'modus-themes-after-load-theme-hook #'nh/modus-themes-solaire-faces)

(defun nh/update-theme ()
  "Update various UI elements when theme change."
  ;; Update title bar appearance based on theme
  (when (memq window-system '(mac ns))
    (let* ((bg-color (face-attribute 'default :background))
           (is-dark (< (apply '+ (color-values bg-color))
                      (* 0.5 (apply '+ (color-values "white"))))))
      (modify-all-frames-parameters
       (list (cons 'ns-appearance (if is-dark 'dark 'light))))))
  ;; Make modeline taller, use a modern font, and add a subtle border.
  (dolist (sym '(mode-line mode-line-inactive))
    (set-face-attribute
     sym nil
     :height 120
     :font "Iosevka Etoile"
     :box `(:line-width 4 :color ,(face-attribute sym :background))))
  ;; Org-mode tweaks
  (with-eval-after-load 'org-faces
    (set-face-background 'org-hide (face-attribute 'default :background))
    (set-face-foreground 'org-hide (face-attribute 'default :background)))
  ;; Force fringe to inherit from default face
  (set-face-attribute 'fringe nil
                      :inherit 'default
                      :background 'unspecified
                      :foreground 'unspecified)
  ;; Fix line numbers to use the same background as default with subtle foreground
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
  ;; Make line numbers fill the gutter
  (setq-default display-line-numbers-width-start t)
  )

(add-hook 'after-load-theme-hook #'nh/update-theme)

;; Rainbow Delimiters: Color-coding for parentheses and brackets
;; Colors nested delimiters with different colors based on their depth, making
;; it easier to match parentheses and understand code structure in Lisp-like languages.
;; GitHub: https://github.com/Fanael/rainbow-delimiters
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
                      :background 'unspecified
                      :weight 'bold
                      :underline t)
  (nh/bold-rainbow-parens))

;; Paren: Built-in parentheses highlighting
;; Highlights matching parentheses when the cursor is positioned on them,
;; helping to identify matching pairs and catch syntax errors quickly.
;; (use-package paren
;;   :ensure nil
;;   :config
;;   (show-paren-mode t))

;; Highlight Parentheses: Highlight surrounding parentheses
;; Continuously highlights all parentheses around the cursor position with
;; different colors based on nesting level, providing constant visual feedback.
;; GitHub: https://github.com/tsdh/highlight-parentheses.el
(use-package highlight-parentheses
  :ensure t
  :diminish t
  :commands (highlight-parentheses-mode)
  :init
  ;; Enable highlight-parentheses-mode in all Lisp-related modes
  (dolist (hook (nh/lisp-hooks))
    (add-hook hook #'highlight-parentheses-mode)))

;; Smartparens: Minor mode for dealing with pairs in Emacs
;; Provides intelligent handling of paired characters like parentheses, quotes,
;; and brackets with structural editing commands for navigating and manipulating code.
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
;; Makes buffer names unique by adding directory paths when multiple buffers
;; have the same filename, eliminating confusion when editing similar files.
(use-package uniquify
  :ensure nil  ;; Built-in package, no need to install
  :config
  (setq uniquify-buffer-name-style 'reverse)  ;; Show directory after filename
  (setq uniquify-separator "|")              ;; Use | as separator
  (setq uniquify-after-kill-buffer-p t)       ;; Rename buffers after killing
  (setq uniquify-ignore-buffers-re "^\\*")   ;; Ignore special buffers
)

;; Highlight Symbol: Automatic highlighting of symbol at point
;; Automatically highlights all occurrences of the symbol at point throughout
;; the buffer, making it easy to see where variables and functions are used.
;; GitHub: https://github.com/nschum/highlight-symbol.el
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
                        :background 'unspecified
                        :foreground 'unspecified
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

;; Spacious Padding: Increase the padding/spacing of Emacs frames and windows
;; Provides a more comfortable reading experience by adding padding around
;; windows, mode lines, tab bars, and other UI elements for better visual clarity.
;; GitHub: https://github.com/protesilaos/spacious-padding
(use-package spacious-padding
  :ensure t
  :config
  ;; Enable spacious-padding-mode
  (spacious-padding-mode 1)
  ;; Configure the padding values after package is loaded
  (setq spacious-padding-widths
        '(:internal-border-width 8
          :header-line-width 8
          :mode-line-width 4
          :tab-width 4
          :right-divider-width 8
          :scroll-bar-width 4))
  ;; Don't let spacious-padding affect the fringe
  (setq spacious-padding-subtle-mode-line nil)
  ;; Re-enable after theme changes to ensure it persists
  :hook
  (after-load-theme . spacious-padding-mode))

;; Powerline: Emacs version of the Vim powerline
;; Provides a modern, customizable mode-line with angled separators and better
;; visual organization of mode-line information, inspired by Vim's powerline.
;; GitHub: https://github.com/milkypostman/powerline
(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))

;; Remove text clutter from modeline
(setq-default mode-line-buffer-identification
              '(:eval (propertize "%b" 'face 'mode-line-buffer-id)))

;; Shorter VC info (remove "Git:" prefix and simplify branch name)
(advice-add 'vc-git-mode-line-string :filter-return
            (lambda (str)
              (when str
                (replace-regexp-in-string "^Git[:\-]" "" str))))

;; Simplify position info - just show percentage
(setq mode-line-percent-position '(-3 "%p"))
(setq mode-line-position-column-line-format '(" %l:%c"))

;; Remove "of" from line number display
(setq mode-line-position
      '((:eval (if (>= (point) (point-max))
                   " Bot"
                 (if (<= (point) (point-min))
                     " Top"
                   (format " %d%%" (/ (- (point) (point-min)) 0.01
                                     (- (point-max) (point-min)))))))))

;; Hide encoding/EOL info unless it's not UTF-8
(setq-default mode-line-mule-info
              '(:eval (if (and buffer-file-coding-system
                               (eq buffer-file-coding-system 'utf-8-unix))
                          ""  ; Hide for UTF-8
                        " %z")))  ; Show for other encodings

;; Remove the modification indicator [**] and just use color
(setq-default mode-line-modified
              '(:eval (if (buffer-modified-p)
                          (propertize "●" 'face '(:foreground "orange"))
                        "")))

;; Hide all minor modes from the modeline completely
(setq mode-line-modes
      (list (propertize "%[" 'help-echo "Recursive edit, type C-M-c to get out")
            '(:eval (propertize (format-mode-line mode-name)
                                'face 'mode-line-buffer-id
                                'help-echo "Major mode"))
            (propertize "%]" 'help-echo "Recursive edit, type C-M-c to get out")
            " "))

;; Hide minor mode lighters (text indicators)
(use-package diminish
  :ensure t
  :config
  ;; Only diminish built-in modes that don't have use-package declarations
  (with-eval-after-load 'eldoc (diminish 'eldoc-mode))
  (with-eval-after-load 'autorevert (diminish 'auto-revert-mode))
  (with-eval-after-load 'outline (diminish 'outline-minor-mode)))

;; Force header-line face via custom-set-faces as last resort
(custom-set-faces
 '(header-line ((t (:inherit default :background unspecified)))))

(provide 'nh-theme)

;;; nh-theme.el ends here
