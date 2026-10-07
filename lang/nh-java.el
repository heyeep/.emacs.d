;;; nh-java.el --- Modern Java development configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; java-mode: Emacs's built-in Java mode.
;; https://www.gnu.org/software/emacs/manual/html_node/ccmode/
(use-package cc-mode
  :ensure nil
  :mode ("\\.java\\'" . java-mode))

;; javadoc-lookup: open Java API docs for a class name.
;; https://github.com/nicferrier/emacs-javadoc-lookup
(use-package javadoc-lookup
  :ensure t
  :commands (javadoc-lookup))

;; lsp-java: Java language server support through Eclipse JDT.
;; https://github.com/emacs-lsp/lsp-java
(use-package lsp-java
  :ensure t
  :after lsp-mode
  :config
  (add-hook 'java-mode-hook #'lsp-java-lens-mode)

  ;; The JDT server needs more memory than the default on large projects.
  (setq lsp-java-vmargs '("-Xmx2G"
                          "-XX:+UseG1GC"
                          "-XX:+UseStringDeduplication"))

  (setq lsp-java-completion-generate-parameters t
        lsp-java-completion-generate-constructor t)

  ;; Download library sources so jump-to-definition opens real code.
  (setq lsp-java-maven-download-sources t
        lsp-java-gradle-download-sources t))

;; Indent Java the way IntelliJ IDEA does.
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

(defun nh/java-generate-constructor ()
  "Generate class constructor using LSP.
Creates constructors for the current class using available fields."
  (interactive)
  (lsp-execute-code-action-by-kind "source.generate.constructor"))

(defun nh/java-generate-getters-setters ()
  "Generate getters and setters using LSP.
Creates accessor methods for the current class's fields."
  (interactive)
  (lsp-execute-code-action-by-kind "source.generate.accessors"))

(defun nh/java-import-class-at-point ()
  "Import class at point using LSP.
Detects the unresolved class name and adds the appropriate import."
  (interactive)
  (call-interactively 'lsp-java-add-import))

(defun nh/java-toggle-test-impl ()
  "Toggle between Java test and implementation files."
  (interactive)
  (let* ((filename (buffer-file-name))
         (is-test (string-match-p "Test\\.java$" filename))
         (new-filename
          (if is-test
              ;; From test to implementation
              (replace-regexp-in-string
               "/src/test/java/" "/src/main/java/"
               (replace-regexp-in-string "Test\\.java$" ".java" filename))
            ;; From implementation to test
            (replace-regexp-in-string
             "/src/main/java/" "/src/test/java/"
             (replace-regexp-in-string "\\.java$" "Test.java" filename)))))
    (if (file-exists-p new-filename)
        (find-file new-filename)
      (message "Target file does not exist: %s" new-filename))))

(defun nh/java-create-class (classname package)
  "Create a new Java class with the given name and package.
Argument CLASSNAME is the name of the class to create.
Argument PACKAGE is the package name for the class."
  (interactive
   (list
    (read-string "Class name: ")
    (read-string "Package: ")))
  (let* ((src-dir (expand-file-name "src/main/java"
                                    (or (projectile-project-root) default-directory)))
         (package-path (replace-regexp-in-string "\\." "/" package))
         (dir-path (expand-file-name package-path src-dir))
         (file-path (expand-file-name (concat classname ".java") dir-path)))

    (unless (file-exists-p dir-path)
      (make-directory dir-path t))

    (find-file file-path)

    (insert (format "package %s;\n\n" package))
    (insert (format "public class %s {\n\n" classname))
    (insert "    public " classname "() {\n")
    (insert "        // TODO: Initialize\n")
    (insert "    }\n\n")
    (insert "}")

    (goto-char (point-min))
    (search-forward "// TODO: Initialize")))

(defun nh/java-mode-setup ()
  "Setup function for Java mode enhancements.
Configures indentation, LSP features, and keybindings for Java development."
  (c-set-style "intellij")
  (setq-local tab-width 4)
  (setq-local indent-tabs-mode nil)

  (when (fboundp 'electric-pair-local-mode)
    (electric-pair-local-mode 1))

  (local-set-key (kbd "C-c j t") #'nh/java-toggle-test-impl)

  (local-set-key (kbd "C-c j b") #'nh/java-run-build-command)
  (local-set-key (kbd "C-c j m") #'nh/java-run-maven-command)
  (local-set-key (kbd "C-c j g") #'nh/java-run-gradle-command)

  (local-set-key (kbd "C-c j o") #'nh/java-organize-imports)
  (local-set-key (kbd "C-c j i") #'nh/java-import-class-at-point)
  (local-set-key (kbd "C-c j c") #'nh/java-create-class)
  (local-set-key (kbd "C-c j C") #'nh/java-generate-constructor)
  (local-set-key (kbd "C-c j G") #'nh/java-generate-getters-setters))

(add-hook 'java-mode-hook #'nh/java-mode-setup)

(provide 'nh-java)

;;; nh-java.el ends here
