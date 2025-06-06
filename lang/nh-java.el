;;; nh-java.el --- Modern Java development configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Java Mode: Built-in Java major mode
;; Provides syntax highlighting, indentation, and core Java editing features.
;; https://www.gnu.org/software/emacs/manual/html_node/ccmode/
(use-package cc-mode
  :ensure nil
  :mode ("\\.java\\'" . java-mode)
  :config
  (with-eval-after-load 'evil
    (evil-define-key 'normal java-mode-map
      (kbd "K") #'javadoc-lookup
      (kbd "gf") #'xref-find-definitions)))

;; Javadoc Lookup: Quick access to Java API documentation
;; https://github.com/nicferrier/emacs-javadoc-lookup
(use-package javadoc-lookup
  :ensure t
  :commands (javadoc-lookup))

;; LSP Java: Java support for lsp-mode using Eclipse JDT Language Server
;; https://github.com/emacs-lsp/lsp-java
(use-package lsp-java
  :ensure t
  :after lsp-mode
  :config
  ;; Enhanced Java LSP configuration
  (add-hook 'java-mode-hook #'lsp-java-lens-mode)

  ;; Performance optimizations - crucial for larger Java projects
  ;; These settings significantly improve responsiveness and reduce memory usage
  (setq lsp-java-vmargs '("-Xmx2G"             ;; More memory for JDT server
                          "-XX:+UseG1GC"       ;; Modern garbage collector
                          "-XX:+UseStringDeduplication"))

  ;; Code generation settings - improve productivity with automatic code generation
  (setq lsp-java-completion-generate-parameters t    ;; Generate method parameters
        lsp-java-completion-generate-constructor t)  ;; Generate constructors

  ;; Maven and Gradle setup - enable source downloads for better navigation
  (setq lsp-java-maven-download-sources t      ;; Download Maven sources
        lsp-java-gradle-download-sources t))   ;; Download Gradle sources

;; IntelliJ Style: Indentation and formatting to match IntelliJ IDEA
(c-add-style
 "intellij"
 '("Java"
   (c-basic-offset . 4)
   (c-offsets-alist
    (inline-open . 0)
    (topmost-intro-cont    . +)
    (statement-block-intro . +)
    (knr-argdecl-intro     . 5)
    (substatement-open     . +)
    (substatement-label    . +)
    (label                 . +)
    (statement-case-open   . +)
    (statement-cont        . ++)
    (arglist-intro  . +)
    (arglist-close . c-lineup-arglist)
    (access-label   . 0)
    (inher-cont     . ++)
    (func-decl-cont . ++))))

;; Maven and Gradle integration
(defun nh/java-run-maven-command (command)
  "Run a Maven command in the project root directory.
Argument COMMAND is the Maven command to run (e.g., 'clean install')."
  (interactive "sCommand (e.g. 'clean install'): ")
  (let ((default-directory (or (projectile-project-root) default-directory)))
    (compile (format "mvn %s" command))))

(defun nh/java-run-gradle-command (command)
  "Run a Gradle command in the project root directory.
Argument COMMAND is the Gradle command to run (e.g., 'build')."
  (interactive "sCommand (e.g. 'build'): ")
  (let ((default-directory (or (projectile-project-root) default-directory)))
    (compile (format "./gradlew %s" command))))

;; Auto-detect build system and run appropriate command
(defun nh/java-run-build-command (command)
  "Run build command based on available build system (Maven or Gradle).
Argument COMMAND is the build command to run."
  (interactive "sCommand (e.g. 'build' or 'clean install'): ")
  (let ((default-directory (or (projectile-project-root) default-directory)))
    (cond
     ((file-exists-p "pom.xml") (nh/java-run-maven-command command))
     ((or (file-exists-p "build.gradle") (file-exists-p "build.gradle.kts"))
      (nh/java-run-gradle-command command))
     (t (message "No pom.xml or build.gradle found in project root")))))

(defun nh/java-organize-imports ()
  "Organize imports using LSP.
Removes unused imports and sorts the remaining ones."
  (interactive)
  (lsp-execute-code-action-by-kind "source.organizeImports"))

;; Generate constructor, getters, setters using LSP
(defun nh/java-generate-constructor ()
  "Generate class constructor using LSP.
Creates constructors for the current class using available fields."
  (interactive)
  (lsp-execute-code-action-by-kind "source.generate.constructor"))

;; Generate getters and setters with LSP
(defun nh/java-generate-getters-setters ()
  "Generate getters and setters using LSP.
Creates accessor methods for the current class's fields."
  (interactive)
  (lsp-execute-code-action-by-kind "source.generate.accessors"))

;; Import class at point
(defun nh/java-import-class-at-point ()
  "Import class at point using LSP.
Detects the unresolved class name and adds the appropriate import."
  (interactive)
  (call-interactively 'lsp-java-add-import))

;; Create a new Java class
(defun nh/java-create-class (classname package)
  "Create a new Java class with the given name and package.
Argument CLASSNAME is the name of the class to create.
Argument PACKAGE is the package name for the class."
  (interactive
   (list
    (read-string "Class name: ")
    (read-string "Package: " (lsp-java-get-package-name))))
  (let* ((src-dirs (lsp-java-get-source-paths))
         (src-dir (if (= (length src-dirs) 1)
                     (car src-dirs)
                   (completing-read "Source directory: " src-dirs)))
         (package-path (replace-regexp-in-string "\\." "/" package))
         (dir-path (expand-file-name package-path src-dir))
         (file-path (expand-file-name (concat classname ".java") dir-path)))

    ;; Create directory if it doesn't exist
    (unless (file-exists-p dir-path)
      (make-directory dir-path t))

    ;; Create and visit the file
    (find-file file-path)

    ;; Insert class template
    (insert (format "package %s;\n\n" package))
    (insert (format "public class %s {\n\n" classname))
    (insert "    public " classname "() {\n")
    (insert "        // TODO: Initialize\n")
    (insert "    }\n\n")
    (insert "}")

    ;; Position cursor inside constructor
    (goto-char (point-min))
    (search-forward "// TODO: Initialize")))

;; Main Java mode setup function that configures everything for each Java buffer
(defun nh/java-mode-setup ()
  "Setup function for Java mode enhancements.
Configures indentation, LSP features, and keybindings for Java development."
  ;; Apply IntelliJ indentation style
  (c-set-style "intellij")
  (setq-local tab-width 4)
  (setq-local indent-tabs-mode nil)

  ;; Enable electric pair mode for automatic parenthesis/bracket closing
  (when (fboundp 'electric-pair-local-mode)
    (electric-pair-local-mode 1))

  ;; Navigation bindings
  (local-set-key (kbd "C-c j t") #'nh/java-toggle-test-impl)

  ;; Build tool bindings
  (local-set-key (kbd "C-c j b") #'nh/java-run-build-command)
  (local-set-key (kbd "C-c j m") #'nh/java-run-maven-command)
  (local-set-key (kbd "C-c j g") #'nh/java-run-gradle-command)

  ;; Code generation and refactoring bindings
  (local-set-key (kbd "C-c j o") #'nh/java-organize-imports)
  (local-set-key (kbd "C-c j i") #'nh/java-import-class-at-point)
  (local-set-key (kbd "C-c j c") #'nh/java-create-class)
  (local-set-key (kbd "C-c j gc") #'nh/java-generate-constructor)
  (local-set-key (kbd "C-c j gg") #'nh/java-generate-getters-setters))

;; Add hooks for Java mode
(add-hook 'java-mode-hook #'nh/java-mode-setup)

(provide 'nh-java)

;;; nh-java.el ends here
