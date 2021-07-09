;;; -*- lexical-binding: t; -*-

(when (< emacs-major-version 27)
  (load "~/.emacs.d/early-init.el")
  (require 'early-init))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file 
