# .emacs.d

Purpose: explain how this Emacs configuration is organized, and list the keys and commands it adds.

Reader: me, and anyone browsing or borrowing from this config.

Last updated: 7 October 2026.

## Overview

This is my Emacs configuration for day-to-day development, mainly Ruby on Rails, with JavaScript, TypeScript, Python and Lisp support too.

It uses a modern completion stack (Vertico, Consult, Corfu), Language Server Protocol (LSP) support for code intelligence, and terminal-based AI coding agents.

To try a change, run `M-x eval-buffer` in the file you edited, or restart Emacs. There is no build step.

## Contents

- [How the config loads](#how-the-config-loads)
- [Where things live](#where-things-live)
- [Keys and commands](#keys-and-commands)
- [How the main parts are set up](#how-the-main-parts-are-set-up)
- [Make changes safely](#make-changes-safely)
- [Known issues and workarounds](#known-issues-and-workarounds)

## How the config loads

Emacs starts in two files, then loads each module and records whether it loaded.

1. `early-init.el` runs first. It sets up native compilation, tunes garbage collection (GC), and turns off toolbars and other UI before any package loads.
2. `init.el` sets up packages, then loads each module with `nh/require-with-log`, which records whether it loaded.
3. Most modules load in `after-init-hook`, so Emacs opens before the slower setup finishes.
4. When loading finishes, Emacs shows a summary in the `*Messages*` buffer. The `nh/load-stats` variable holds the counts of loaded and failed modules, and each failure's error.

## Where things live

```
~/.emacs.d/
├── init.el                 # Main entry point
├── early-init.el           # Runs before packages load (GC, native compilation)
├── config/                 # Core modules
│   ├── nh-default.el       # Default settings, keybindings, packages
│   ├── nh-helpers.el       # Helper functions used across the config
│   ├── nh-autocompletion.el # Vertico, Consult, Corfu, LSP
│   ├── nh-theme.el         # Themes and visuals
│   ├── nh-git.el           # Magit and git
│   ├── nh-org.el           # Org mode
│   └── ...
├── lang/                   # One module per language
│   ├── nh-ruby.el          # Ruby and Rails: LSP, RSpec, Rails console
│   ├── nh-web.el           # HTML, CSS, JavaScript, TypeScript (Tide)
│   ├── nh-python.el        # Python
│   ├── nh-elisp.el         # Emacs Lisp
│   └── ...
├── ai/                     # AI tools
│   ├── nh-copilot-ai.el    # GitHub Copilot
│   ├── nh-aider-ai.el      # Aider
│   ├── nh-agent-ai.el      # Agent Shell (Claude Code and Codex)
│   └── nh-vaibe-ai.el      # My own AI tooling
├── docs/                   # This README
└── submodules/             # Git submodules, such as transient
```

Every module is named `nh-<topic>.el` and ends with `(provide 'nh-<topic>)`.

## Keys and commands

`C-` means Control and `M-` means Meta. Meta is the Command key on macOS in this config.

### Ruby and Rails

These work in Ruby buffers.

| Key | What it does |
|-----|--------------|
| `C-c r p f` | Run the current spec file |
| `C-c r p p` | Run the spec at point |
| `C-c r p r` | Rerun the last spec |
| `C-c r p a` | Run all specs |
| `C-c r r` | Start the right console for the project |
| `C-c r d` | Start a development Rails console without Spring |
| `C-c r C` | Start a Rails console, asking for the environment |
| `C-c r N` | Start a Rails console without Spring |
| `C-c r t r` | Show Rails routes |
| `C-c r t d` | Open the Rails database console |
| `C-c r t g` | Search Rails routes |
| `C-c r u c` | Check the current file with RuboCop |
| `C-c r u a` | Auto-correct the current file with RuboCop |
| `C-c r s` | Find the spec for this file (projectile-rails) |
| `C-c C-d` | Show robe documentation for the symbol at point |

### JavaScript and TypeScript

Tide (the TypeScript editing package) starts on its own in these cases:

- in `typescript-mode`
- in `js2-mode`, when the project has a `tsconfig.json` or `jsconfig.json`
- in `web-mode`, for `.tsx` files

Tide formats on save only when the project has a `tsfmt.json`.

### Python, Emacs Lisp and other languages

| Key | Where | What it does |
|-----|-------|--------------|
| `C-c F r` | Python | Find references |
| `C-c F a` | Python | Find assignments |
| `C-c D d` | Python, Common Lisp, Elixir | Describe the symbol at point |
| `C-c D p` | Python | Insert a breakpoint |
| `C-c F f` | Emacs Lisp | Find references to a function |
| `C-c F v` | Emacs Lisp | Find references to a variable |

### LSP

| Key or command | What it does |
|----------------|--------------|
| `C-c l d` | Show documentation |
| `C-c l f` | Show Flycheck errors |
| `C-c l i` | List symbols in the file |
| `C-c l r` | Find references |
| `C-c l .` | Find definitions |
| `M-x nh/check-ruby-lsp-status` | Check the Ruby LSP server |
| `M-x nh/lsp-describe-workspace` | Show the LSP servers and project root |
| `M-x nh/lsp-restart-workspace` | Restart the LSP server |

### Find files and text

| Key | What it does |
|-----|--------------|
| `C-c p f` | Find a file in the project |
| `C-c p p` | Switch project |
| `C-c p s r` | Search the project with ripgrep |
| `C-x b` | Switch buffer |
| `C-x C-r` | Open a recent file |
| `C-s` | Search in the buffer |
| `C-c k` | Search the project with ripgrep (Consult) |
| `C-c f` | Find files by name |
| `C-c d` | Find files with fd |

### Terminal

| Key | What it does |
|-----|--------------|
| `C-c v` | Open vterm |
| `C-c V` | Open a new multi-vterm terminal |
| `C-c n` / `C-c p` / `C-c c` | In vterm: next, previous, or new terminal |
| `C-SPC` | In vterm: freeze the terminal and start selecting text |
| `M-w` or Cmd+C | Copy the selection and unfreeze |
| `C-g` | Unfreeze without copying |
| Cmd+V | Paste |

Dragging with the mouse also freezes the terminal and selects text.

### AI agents and debugging

| Key | What it does |
|-----|--------------|
| `C-c A c` | Start Claude Code (agent-shell) |
| `C-c A x` | Start Codex (agent-shell) |
| `C-c A A` | Reopen the last agent session |
| `F5` | Start a debug session (dape) |
| `F9` | Toggle a breakpoint |
| `C-x C-a` then a letter | Other dape commands, such as `n` next, `s` step in, `c` continue |

### Formatting

| Command | What it does |
|---------|--------------|
| `M-x nh/indent-buffer` | Indent the whole buffer |
| `M-x nh/indent-region-or-buffer` | Indent the region, or the buffer if none |
| `M-x nh/whitespace-region-or-buffer-cleanup` | Clean up whitespace |

## How the main parts are set up

### Packages load only when needed

- `use-package` installs each package automatically through `:ensure t`.
- Heavy packages wait for `:commands`, `:hook` or `:defer` before loading.
- Each language has its own setup function, such as `nh/ruby-mode-setup`.
- `nh/globally-ignored-directories` lists folders that Projectile, Consult and completion all skip.

### Ruby uses ruby-lsp only

- `ruby-lsp-ls` is the only Ruby language server. Solargraph, RuboCop's server and the others are turned off through `lsp-disabled-clients`.
- RSpec code lenses run through `bundle exec`. See [the RSpec code lens workaround](#rspec-code-lenses-run-through-bundle-exec).
- `.rake` files use a shorter LSP timeout. See [LSP errors in rake files](#lsp-errors-in-rake-files-are-usually-harmless).
- `inf-ruby` detects Rails projects and starts the right console.

### Completion uses Vertico, Consult and Corfu

This config doesn't use Ivy, Counsel or Company.

| Package | Role |
|---------|------|
| Vertico | Vertical list in the minibuffer |
| Consult | Search, navigation and file-finding commands |
| Corfu | Completion popup in the buffer |
| Cape | Extra completion sources: files, words in open buffers, keywords |
| Orderless | Match words in any order, separated by spaces |
| Marginalia | Notes beside each item in the minibuffer |

### Indentation uses spaces only

| Language | Indent |
|----------|--------|
| Most languages | 4 spaces |
| Web, JavaScript, CSS, TypeScript | 2 spaces |
| Ruby | 2 spaces |

Tabs are never used (`indent-tabs-mode` is off). `nh/indent-offset` returns the right indent for the current major mode.

### Some packages skip byte-compilation

`rake` and `tide` break when byte-compiled, so `init.el` skips them. The function `nh/maybe-skip-package-compile` does this as advice on `package--compile`.

### Shell variables are copied into Emacs

`exec-path-from-shell` copies these from your shell:

- API keys: `OPENAI_API_KEY`, `CLAUDE_API_KEY`, `ANTHROPIC_API_KEY` and others
- Development: `GOPATH`, `ANDROID_HOME`, `NPM_TOKEN`
- Ruby: `DISABLE_SPRING`, `OBJC_DISABLE_INITIALIZE_FORK_SAFETY`
- Build tools: `LDFLAGS`, `CPPFLAGS`, `PKG_CONFIG_PATH`

### Transient comes from a git submodule

The config loads transient from `submodules/transient` to avoid clashes with the built-in and package-archive versions. `init.el` adds it to the load path before anything else, then marks the archive version's autoloads as already loaded:

```elisp
;; In init.el, loaded before normal-top-level-add-subdirs-to-load-path
(add-to-list 'load-path "submodules/transient/lisp")
(provide 'transient-autoloads)  ; Stops the archive version from loading
```

## Make changes safely

### Test a change

1. Reload one module: run `M-x eval-buffer` in its file.
2. Reload everything: restart Emacs, or run `M-x restart-emacs`.
3. Check load times: each module's load time appears in `*Messages*`.
4. Find load failures: check the `nh/load-stats` variable.

### Add a package

1. Add a `use-package` block to the right module.
2. Set `:ensure t` so it installs automatically.
3. Use `:hook`, `:commands` or `:defer` so it loads only when needed.
4. If it creates cache folders, add them to `nh/globally-ignored-directories`.
5. Restart Emacs and check the startup summary.

### Add a language

Create or edit `lang/nh-<language>.el`. `nh/load-directory` in `nh-helpers.el` loads every file in `lang/` automatically.

```elisp
;;; nh-<language>.el --- <Language> configuration -*- lexical-binding: t; -*-

;;; Code:

(use-package <major-mode>
  :ensure t
  :mode ("\\.ext\\'" . <mode>)
  :hook (<mode> . lsp-deferred)
  :config
  ;; Configuration here
  )

(provide 'nh-<language>)
;;; nh-<language>.el ends here
```

### Helper functions

These live in `nh-helpers.el`:

| Function | What it does |
|----------|--------------|
| `nh/load-directory` | Load every `.el` file in a folder, logging errors |
| `nh/indent-buffer` | Indent the buffer |
| `nh/indent-region-or-buffer` | Indent the region or the buffer |
| `nh/toggle-window-split` | Switch between side-by-side and stacked windows |
| `nh/rotate-windows` | Rotate buffers between windows |
| `nh/find-file-dwim` | Open files with context (Dired, Magit and others) |

## Known issues and workarounds

### RSpec code lenses run through bundle exec

ruby-lsp's "Run" lenses send `./rspec` commands, which fail without a binstub. An advice on `lsp-ruby-lsp--run-test` changes a leading `./rspec`, `bin/rspec` or `rspec` to `bundle exec rspec`. `nh/run-rspec-at-point` still works as a manual fallback.

### TypeScript uses one checker

TypeScript buffers use only the `typescript-tide` Flycheck checker. Tide's server already type-checks with the project's `tsconfig.json`, so there's no separate `tsc` checker.

### Some packages must not be byte-compiled

`rake` and `tide` must not be byte-compiled. The config handles this, but keep it in mind when you debug those packages.

### LSP errors in rake files are usually harmless

ruby-lsp can raise `LocationNotFoundError` in `.rake` files. These errors are usually harmless. To make them rarer, `.rake` buffers use a 2-second LSP timeout (`lsp-response-timeout`).
