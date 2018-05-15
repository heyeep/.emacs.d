(add-to-list 'load-path "~/.emacs.d/fork/evil")

(use-package undo-tree
  ;; Vim Style Undo
  :ensure t
  :diminish undo-tree-mode
  :config
  ;; Double undo limit
  (setq undo-limit 160000)
  (setq undo-strong-limit 240000)
  (setq undo-outer-limit 24000000)
  (global-undo-tree-mode))

(use-package evil
  ;; :load-path "~/.emacs.d/fork/evil"
  :ensure t
  :init
  (setq evil-want-C-u-scroll t) ;; Regain scroll up with C-u.
  (setq evil-want-fine-undo t)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-move-beyond-eol nil)

  ;; Search by symbol.
  (setq-default evil-symbol-word-search t)

  ;; `isearch' is faster than `evil-search' so use it even though `evil-search'
  ;; has `evil-paste-from-register' support.
  (setq evil-search-module 'isearch)

  (setq evil-ex-search-persistent-highlight nil)
  :config
  (evil-ex-define-cmd "W" #'evil-write-all) ;; :wa[ll] also works.
  (evil-ex-define-cmd "al[ign]" #'align-regexp)
  (evil-ex-define-cmd "formata[ll]" #'+format-open-buffers)
  (evil-ex-define-cmd "sh[ell]" #'+open-shell)
  (evil-ex-define-cmd "git" #'magit-status)
  (evil-ex-define-cmd "gstage" #'magit-stage)
  (evil-ex-define-cmd "gu[nstage]" #'magit-unstage)
  (evil-ex-define-cmd "gb[lame]" #'magit-blame)
  (evil-ex-define-cmd "a" #'projectile-find-other-file)
  (evil-ex-define-cmd "er[rors]" #'flycheck-list-errors)
  (evil-ex-define-cmd "[colump]aste" #'+paste-column)

  (setq evil-jumps-max-length 1000)

  ;; Set underscore to be a word.
  ;; https://github.com/emacs-evil/evil
  (modify-syntax-entry ?_ "w")

  ;; http://spacemacs.org/doc/FAQ
  ;; https://github.com/syl20bnr/spacemacs/issues/2032
  ;; https://emacs.stackexchange.com/questions/14940/emacs-doesnt-paste-in-evils-visual-mode-with-every-os-clipboard/15054#15054
  (fset 'evil-visual-update-x-selection 'ignore)
  (setq evil-flash-delay 8) ;; control the highlight time of searches

  ;; C-S-o jumps foward in jumplist, C-o goes the other way.
  (setq evil-want-C-i-jump nil)
  (define-key evil-motion-state-map (kbd "C-S-o") 'evil-jump-forward)

  (setq evil-normal-state-tag   (propertize " NORMAL")
        evil-emacs-state-tag    (propertize " EMACS")
        evil-insert-state-tag   (propertize " INSERT")
        evil-motion-state-tag   (propertize " MOTION")
        evil-visual-state-tag   (propertize " VISUAL")
        evil-replace-state-tag  (propertize " REPLACE")
        evil-operator-state-tag (propertize " OPERATOR"))
  (setq evil-default-state 'emacs)
  (evil-set-initial-state 'etags-select-mode 'motion)
  (evil-set-initial-state 'debugger-mode 'motion)
  (evil-set-initial-state 'messages-buffer-mode 'motion)
  (evil-mode 1)

  ;; Reselect text after identing.
  ;; https://superuser.com/questions/469327/combining-two-operators-in-evil-mode-emacs
  (define-key evil-visual-state-map "g>" 'evil-shift-right)
  (define-key evil-visual-state-map "g<" 'evil-shift-left)
  (define-key evil-visual-state-map ">" (kbd "g>gv"))
  (define-key evil-visual-state-map "<" (kbd "g<gv"))

  (define-key evil-normal-state-map (kbd "M-.") nil)

  (add-hook 'buffer-menu-mode-hook
            (lambda ()
              (define-key Buffer-menu-mode-map (kbd "g") nil)))

  ;; Fix black cursor.
  (setq evil-default-cursor t))

(use-package evil-visualstar
  :ensure t
  :init
  ;; Advise star/pound search to be case insensitive.
  ;; This advice could also go into the evil package.
  ;; This probably causes the search highlight to flicker
  ;; while going to the next search candidate.
  (defun +evil-visual-star-ignorecase (orig-fun &rest args)
    (interactive)
    (let ((evil-ex-search-case 'insensitive))
      (apply orig-fun args)))

  (advice-add 'evil-ex-search-word-backward :around #'+evil-visual-star-ignorecase)
  (advice-add 'evil-ex-search-word-forward :around #'+evil-visual-star-ignorecase)
  :bind (:map evil-visual-state-map
              ("*" . evil-visualstar/begin-search-forward)
              ("#" . evil-visualstar/begin-search-backward))
  :config
  (global-evil-visualstar-mode)
  (setq evil-visualstar/persistent t))

;; (use-package evil-magit
;;   ;; Magit integration
;;   :ensure nil
;;   :after magit
;;   :init
;;   (setq evil-magit-want-horizontal-movement t))

(provide 'hpd-evil)
