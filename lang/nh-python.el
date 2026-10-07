;;; nh-python.el --- Python development configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Python editing, shell, debugging and anaconda-mode setup.

;;; Code:

(require 'nh-env)

;; python: Emacs's built-in Python mode, plus shell and interpreter setup.
(use-package python
  :ensure nil
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :init
  (defun nh/setup-python-interpreter ()
    "Set up Python interpreter based on available versions and OS."
    (cond
     (nh-env/is-mac
      (cond
       ((executable-find "python3") (setq python-shell-interpreter "python3"))
       ((executable-find "python2") (setq python-shell-interpreter "python2"))
       (t (setq python-shell-interpreter "python"))))
     (t
      (cond
       ((executable-find "python3") (setq python-shell-interpreter "python3"))
       (t (setq python-shell-interpreter "python"))))))

  (defun nh/setup-inferior-python ()
    "Launch Python shell in background for immediate availability."
    (nh/setup-python-interpreter)
    (unless (python-shell-get-buffer)
      (save-selected-window
        (let ((python-shell-prompt-detect-enabled nil))
          (run-python (python-shell-calculate-command) nil nil)))))

  :config
  (setq python-indent-offset 4
        python-indent-guess-indent-offset t
        python-shell-completion-native-enable nil  ;; Native completion breaks in Emacs 30.1.
        python-shell-prompt-detect-enabled t
        python-shell-prompt-detect-failure-warning nil)

  :hook ((python-mode . (lambda ()
                          (setq-local tab-width 4
                                      indent-tabs-mode nil
                                      fill-column 88)  ;; Black's line length.

                          (when (fboundp 'eldoc-mode)
                            (eldoc-mode 1))
                          (when (fboundp 'electric-pair-local-mode)
                            (electric-pair-local-mode 1))

                          ;; Pick the interpreter, but don't start a shell yet.
                          (nh/setup-python-interpreter)

                          ;; Highlight TODO-style tags and decorators.
                          (font-lock-add-keywords
                           nil
                           '(("\\<\\(TODO\\|FIXME\\|BUG\\|HACK\\|NOTE\\|XXX\\):"
                              1 'font-lock-warning-face t)
                             ("@\\(\\sw\\|\\s_\\)+" . 'font-lock-preprocessor-face))))))

  :bind (:map python-mode-map
              ("C-c C-e" . python-shell-send-statement)
              ("C-c C-r" . python-shell-send-region)
              ("C-c C-b" . python-shell-send-buffer)
              ("C-c C-f" . python-shell-send-defun)
              ("C-c C-p" . run-python)
              ("C-c C-z" . python-shell-switch-to-shell)
              ("C-c C-c" . python-shell-send-buffer)

              ("C-c e e" . python-shell-send-statement)
              ("C-c e b" . python-shell-send-buffer)
              ("C-c e f" . python-shell-send-defun)
              ("C-c e s" . python-shell-send-string)
              ("C-c e R" . python-shell-send-region)

              ("C-c D d" . python-describe-at-point)

              ("C-c D b" . pdb)
              ("C-c D t" . python-shell-send-file)))

;; anaconda-mode: completion, navigation and docs for Python through Jedi.
;; GitHub: https://github.com/pythonic-emacs/anaconda-mode
(use-package anaconda-mode
  :ensure t
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode))
  :config
  ;; Keep a separate server install for each Emacs major version.
  (setq anaconda-mode-installation-directory
        (expand-file-name (format "anaconda-mode/%s" emacs-major-version)
                          user-emacs-directory))

  (setq anaconda-mode-eldoc-as-single-line t
        anaconda-mode-server-command "python")

  :bind (:map anaconda-mode-map
              ("M-." . anaconda-mode-find-definitions)
              ("M-," . anaconda-mode-go-back)
              ("C-c F r" . anaconda-mode-find-references)
              ("C-c F a" . anaconda-mode-find-assignments)

              ("C-c D s" . anaconda-mode-show-doc)

              ("C-c c c" . anaconda-mode-complete)))

(defun nh/python-insert-breakpoint ()
  "Insert a Python breakpoint at current line."
  (interactive)
  (beginning-of-line)
  (open-line 1)
  (insert "import pdb; pdb.set_trace()"))

(defun nh/python-insert-ipdb-breakpoint ()
  "Insert an IPython debugger breakpoint at current line."
  (interactive)
  (beginning-of-line)
  (open-line 1)
  (insert "import ipdb; ipdb.set_trace()"))

(defun nh/python-remove-breakpoints ()
  "Remove all pdb breakpoints from current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^.*import i?pdb; i?pdb.set_trace().*$" nil t)
      (beginning-of-line)
      (kill-whole-line))))

(defun nh/python-run-file ()
  "Run current Python file in shell."
  (interactive)
  (when buffer-file-name
    (python-shell-send-file buffer-file-name)
    (message "Executed %s" (file-name-nondirectory buffer-file-name))))

(with-eval-after-load 'python
  (define-key python-mode-map (kbd "C-c D p") #'nh/python-insert-breakpoint)
  (define-key python-mode-map (kbd "C-c D i") #'nh/python-insert-ipdb-breakpoint)
  (define-key python-mode-map (kbd "C-c D r") #'nh/python-remove-breakpoints)
  (define-key python-mode-map (kbd "C-c r r") #'nh/python-run-file))

;; Old entry point some auto-mode-alist entries may still name.
(defun +python-mode ()
  "Bootstrap Python mode - maintained for compatibility."
  (setq auto-mode-alist (rassq-delete-all #'+python-mode auto-mode-alist))
  (python-mode))

(provide 'nh-python)
;;; nh-python.el ends here
