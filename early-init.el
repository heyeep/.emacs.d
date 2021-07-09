;;; -*- lexical-binding: t; -*-

(require 'package)

(defvar package-quickstart)

(setq package-user-dir
      (format "%selpa/%s/" user-emacs-directory emacs-major-version))

(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))

(setq package-enable-at-startup
      nil)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))

(setq package-archive-priorities
      '(("org" . 10)
        ("melpa" . 5)
        ("gnu" . 1)))

(if (>= emacs-major-version 27)
    (setq package-quickstart t)
  (package-initialize))

(provide 'early-init)
