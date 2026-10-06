;;; nh-code.el --- Code development tools configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for code development tools including debugging, linting, and formatting.

;;; Code:

;; Debug Adapter Protocol for Emacs (dape)
(use-package dape
  :ensure t
  :after lsp-mode
  :config
  ;; Enable dape
  (dape-mode 1)

  ;; Configure debug templates
  (setq dape-configs
        '((python
           (:type "python"
		  :request "launch"
		  :args ""
		  :cwd nil
		  :module nil
		  :program nil
		  :name "Python :: Run file (buffer)"))
          (python-django
           (:type "python"
		  :request "launch"
		  :args ""
		  :cwd nil
		  :module "django"
		  :args '("runserver")
		  :name "Python :: Django server"))
          (python-flask
           (:type "python"
		  :request "launch"
		  :args ""
		  :cwd nil
		  :module "flask"
		  :args '("run" "--debug")
		  :name "Python :: Flask server"))
          (node
           (:type "node"
		  :request "launch"
		  :program nil
		  :cwd nil
		  :runtimeExecutable "node"
		  :runtimeArgs nil
		  :env '(("DEBUG" . "*"))
		  :name "Node :: Run file (buffer)"))
          (node-nodemon
           (:type "node"
		  :request "launch"
		  :program nil
		  :cwd nil
		  :runtimeExecutable "nodemon"
		  :runtimeArgs '("--inspect")
		  :name "Node :: Run with Nodemon"))
          (javascript
           (:type "chrome"
		  :request "launch"
		  :url "http://localhost:3000"
		  :webRoot "${workspaceFolder}"
		  :sourceMapPathOverrides
		  '(("webpack:///src/*" . "${workspaceFolder}/src/*"))
		  :name "JavaScript :: Chrome Debug"))
          (react
           (:type "chrome"
		  :request "launch"
		  :url "http://localhost:3000"
		  :webRoot "${workspaceFolder}"
		  :sourceMapPathOverrides
		  '(("webpack:///src/*" . "${workspaceFolder}/src/*"))
		  :name "React :: Chrome Debug"))
          (ruby
           (:type "ruby"
		  :request "launch"
		  :program nil
		  :useBundler t
		  :name "Ruby :: Run file (buffer)"))
          (rails
           (:type "ruby"
		  :request "launch"
		  :program nil
		  :useBundler t
		  :command "rails server"
		  :args '("--binding" "0.0.0.0")
		  :name "Rails :: Server"))))

  ;; Debugger settings
  (setq dape-node-debugger 'node)
  (setq dape-chrome-debugger 'chrome)
  (setq dape-ruby-debugger 'ruby-debug-ide)

  ;; Keybindings
  :bind
  (:map dape-mode-map
        ("<f5>" . dape-debug)
        ("M-<f5>" . dape-hydra)
        ("<f9>" . dape-breakpoint-toggle)
        ("M-<f9>" . dape-breakpoint-condition)
        ("C-<f9>" . dape-breakpoint-hit-condition)
        ("<f10>" . dape-next)
        ("<f11>" . dape-step-in)
        ("<f12>" . dape-step-out)
        ("M-<f12>" . dape-step-out)
        ("C-M-<f12>" . dape-continue)))

(provide 'nh-code)
;;; nh-code.el ends here
