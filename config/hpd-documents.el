;;;; -*- lexical-binding: t; -*-

;; PDF Support
(use-package let-alist
  :ensure t)

(use-package tablist
  :ensure t)

(use-package pdf-tools
  :ensure t)

(pdf-tools-install)

(provide 'hpd-documents)
