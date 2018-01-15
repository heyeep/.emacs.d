;;;; -*- lexical-binding: t; -*-

(use-package lua-mode
  :ensure t
  :mode ("\\.lua\\'" . lua-mode)
  :interpreter ("lua" . lua-mode)
  :config
  (setq lua-indent-level 2)

  (defun hpd/lua-run-test-suite ()
    "Run test_suite.lua."
    (interactive)
    (let ((default-directory (locate-dominating-file
                              (file-name-directory buffer-file-name)
                              "test_suite.lua")))
      (compilation-start
       "lua test_suite.lua -v"
       'compilation-mode
       (lambda (_mode-name)
         "*lua test results*")
       t)))

  (defun hpd/lua-run-test-file ()
    "Run test file using buffer as file."
    (interactive)
    (if-let ((buffer-file (buffer-file-name)))
        (let ((default-directory (locate-dominating-file
                                  (file-name-directory buffer-file-name)
                                  "main.lua")))
          (compilation-start (format "lua %s -v" buffer-file)
                             'compilation-mode
                             (lambda (_mode-name)
                               "*lua test results*")
                             t))
      (message "`buffer-file-name' is nil.")))

  (defun hpd/lua-run-test-at-point ()
    "Run test at point."
    (interactive)
    (if-let ((buffer-file (buffer-file-name)))
        (let ((function-name
               (let ((current-line (thing-at-point 'line t)))
                 (progn
                   (unless (string-match-p "function" current-line)
                     (search-backward "function"))
                   (let ((new-current-line (s-trim (thing-at-point 'line t))))
                     (s-trim
                      (s-chop-suffix
                       "()"
                       (s-chop-prefix "function" new-current-line))))))))
          (if function-name
              (let ((default-directory (locate-dominating-file
                                        (file-name-directory buffer-file-name)
                                        "main.lua")))
                (compilation-start
                 (format "lua %s %s -v" buffer-file (s-replace ":" "." function-name))
                 'compilation-mode
                 (lambda (_mode-name)
                   "*lua test results*")
                 t))
            (message "Couldn't find `function-name'.")))
      (message "`buffer-file-name' is nil.")))

  (defun pd/love-run ()
    "Run pdrun script in root of project."
    (interactive)
    (let ((default-directory
            (locate-dominating-file default-directory "pdrun")))
      (with-current-buffer
          (get-buffer-create "*compilation*")
        (compilation-start "./pdrun" 'compilation-mode
                           (lambda (_mode-name)
                             "*pdrun*")
                           t)))))

(use-package company-lua
  :ensure t
  :commands (company-lua)
  :init
  (add-hook 'lua-mode-hook
            (lambda ()
              (setq company-lua-interpreter 'love)
              (hpd/company-push-backend-local 'company-lua))))

(use-package love-minor-mode
  :ensure t
  :commands (love/possibly-enable-mode)
  :init
  (defcustom love/documentation-url
    "https://love2d.org/wiki/"
    "URL pointing to the Love wiki."
    :type 'string
    :group 'lua)

  (defun love/search-documentation ()
    "Search Love documentation for the word at the point."
    (interactive)
    (let ((url (concat love/documentation-url (lua-funcname-at-point))))
      (funcall lua-documentation-function url)))

  (add-hook 'lua-mode-hook
            (lambda ()
              (love/possibly-enable-mode))))

(provide 'hpd-lua)
