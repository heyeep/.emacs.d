;;; lang/nh-swift.el --- Swift configuration -*- lexical-binding: t; -*-

;; This file configures Emacs for working with Swift files.

;; Configure sourcekit-lsp path for macOS
(defun nh/sourcekit-lsp-path ()
  "Get the path to sourcekit-lsp on macOS."
  (let ((xcode-path (shell-command-to-string "xcode-select -p")))
    (when xcode-path
      (setq xcode-path (string-trim xcode-path))
      (expand-file-name "sourcekit-lsp" (expand-file-name "usr/bin" xcode-path)))))

;; Xcode simulator utilities
(defun nh/swift-list-simulators ()
  "List available iOS simulators."
  (interactive)
  (let ((simulators (shell-command-to-string "xcrun simctl list devices")))
    (with-current-buffer (get-buffer-create "*Simulators*")
      (erase-buffer)
      (insert simulators)
      (pop-to-buffer (current-buffer)))))

(defun nh/swift-run-simulator (device-name)
  "Run iOS simulator with specified device name."
  (interactive "sDevice name (e.g., 'iPhone 14'): ")
  (async-shell-command (format "open -a Simulator --args -CurrentDevice '%s'" device-name)))

(defun nh/swift-build-and-run (scheme)
  "Build and run the specified scheme in the simulator."
  (interactive "sScheme name: ")
  (let ((default-directory (projectile-project-root)))
    (async-shell-command
     (format "xcodebuild -scheme %s -destination 'platform=iOS Simulator,name=iPhone 14' build" scheme))))

;; Swift Mode: Major mode for editing Swift files
;; Provides syntax highlighting, indentation, and basic editing support for
;; Swift programming with Xcode integration and LSP server configuration.
;; GitHub: https://github.com/swift-emacs/swift-mode
(use-package swift-mode
  :ensure t
  :mode (("\\.swift\\'" . swift-mode)
         ("\\.xib\\'" . nxml-mode)
         ("\\.storyboard\\'" . nxml-mode)
         ("\\.xcodeproj\\'" . nxml-mode)
         ("\\.xcworkspace\\'" . nxml-mode)
         ("\\.plist\\'" . nxml-mode))
  :config
  ;; Match Xcode's indentation style
  (setq swift-mode:parenthesized-expression-offset 4)
  (setq swift-mode:multiline-statement-offset 4)
  (setq swift-mode:basic-offset 4)

  ;; Enable syntax highlighting
  (setq swift-mode:highlight-keywords t)

  ;; Enable auto-indentation
  (setq swift-mode:indent-tabs-mode nil)

  ;; Enable electric-pair-mode for Swift
  (add-hook 'swift-mode-hook 'electric-pair-mode)

  ;; Enable flycheck for Swift
  (add-hook 'swift-mode-hook 'flycheck-mode)

  ;; Configure sourcekit-lsp path
  (setq lsp-swift-server-path (nh/sourcekit-lsp-path)))

;; Format All: Universal code formatter
;; Provides automatic code formatting for multiple languages including Swift
;; with format-on-save functionality and support for various formatters.
;; GitHub: https://github.com/lassik/emacs-format-all-the-code
(use-package format-all
  :ensure t
  :config
  (add-hook 'swift-mode-hook 'format-all-mode)
  (setq format-all-formatters
        '(("Swift" swift-format))))

;; Project creation utilities
(defun nh/swift-create-new-project ()
  "Create a new Swift project using Swift Package Manager."
  (interactive)
  (let ((project-name (read-string "Project name: ")))
    (shell-command (format "swift package init --type executable %s" project-name))
    (find-file (format "%s/Sources/%s/main.swift" project-name project-name))))

(defun nh/swift-create-new-package ()
  "Create a new Swift package using Swift Package Manager."
  (interactive)
  (let ((package-name (read-string "Package name: ")))
    (shell-command (format "swift package init --type library %s" package-name))
    (find-file (format "%s/Sources/%s/%s.swift" package-name package-name package-name))))

(defun nh/swift-create-ios-app ()
  "Create a new iOS app project using Xcode command line tools."
  (interactive)
  (let ((app-name (read-string "App name: ")))
    (shell-command
     (format "mkdir -p %s && cd %s && xcodegen generate" app-name app-name))
    (find-file (format "%s/%s.xcodeproj/project.pbxproj" app-name app-name))))

;; Add utility keybindings
(with-eval-after-load 'swift-mode
  (define-key swift-mode-map (kbd "C-c C-n") 'nh/swift-create-new-project)
  (define-key swift-mode-map (kbd "C-c C-p") 'nh/swift-create-new-package)
  (define-key swift-mode-map (kbd "C-c C-i") 'nh/swift-create-ios-app)
  (define-key swift-mode-map (kbd "C-c C-s") 'nh/swift-list-simulators)
  (define-key swift-mode-map (kbd "C-c C-d") 'nh/swift-run-simulator)
  (define-key swift-mode-map (kbd "C-c C-x") 'nh/swift-build-and-run))

(provide 'nh-swift)

;;; lang/nh-swift.el ends here
