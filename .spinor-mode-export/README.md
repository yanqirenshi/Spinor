# spinor-mode

Emacs major mode for [Spinor](https://github.com/yanqirenshi/Spinor) - a statically-typed Lisp with Haskell-style semantics.

## Features

- Syntax highlighting for `.spin` files
- REPL integration via comint
- LSP support (via `lsp-mode` or `eglot`)
- SLY/SLIME support via Swank server

## Installation

### use-package (recommended)

```elisp
(use-package spinor-mode
  :ensure t
  :mode "\\.spin\\'"
  :custom
  (spinor-program "spinor"))
```

### straight.el

```elisp
(straight-use-package
 '(spinor-mode :type git
               :host github
               :repo "yanqirenshi/spinor-mode"))
```

### Manual Installation

1. Clone this repository:
   ```bash
   git clone https://github.com/yanqirenshi/spinor-mode.git
   ```

2. Add to your `init.el`:
   ```elisp
   (add-to-list 'load-path "/path/to/spinor-mode")
   (require 'spinor-mode)
   ```

## Usage

### Basic Editing

Open any `.spin` file and `spinor-mode` activates automatically with syntax highlighting.

### REPL

Start the Spinor REPL:

```
M-x run-spinor
```

Key bindings in `spinor-mode`:

| Key       | Command                | Description                        |
|-----------|------------------------|------------------------------------|
| `C-x C-e` | `spinor-eval-last-sexp`| Evaluate S-expression before point |
| `C-c C-k` | `spinor-load-file`     | Load current file into REPL        |
| `C-c C-z` | `spinor-switch-to-repl`| Switch to REPL buffer              |

### LSP Support

Spinor provides a built-in LSP server. Configure `lsp-mode`:

```elisp
(use-package lsp-mode
  :ensure t
  :hook (spinor-mode . lsp)
  :config
  (add-to-list 'lsp-language-id-configuration '(spinor-mode . "spinor"))
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("spinor" "lsp"))
    :major-modes '(spinor-mode)
    :server-id 'spinor-lsp)))
```

Or with `eglot`:

```elisp
(use-package eglot
  :ensure t
  :hook (spinor-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               '(spinor-mode . ("spinor" "lsp"))))
```

### SLY Integration

Spinor includes a Swank server compatible with SLY/SLIME.

1. Start the Swank server:
   ```bash
   spinor server --port 4005
   ```

2. Connect from Emacs:
   ```
   M-x sly-connect RET localhost RET 4005 RET
   ```

Full SLY configuration:

```elisp
(use-package sly
  :ensure t
  :config
  (defun sly-spinor ()
    "Connect to Spinor Swank server."
    (interactive)
    (sly-connect "localhost" 4005)))
```

## Customization

| Variable               | Default      | Description                      |
|------------------------|--------------|----------------------------------|
| `spinor-program`       | `"spinor"`   | Command to run Spinor            |
| `spinor-program-args`  | `nil`        | Arguments passed to interpreter  |
| `spinor-repl-buffer-name` | `"*spinor*"` | Name of the REPL buffer       |

Example:

```elisp
(setq spinor-program "/usr/local/bin/spinor")
(setq spinor-program-args '("--no-banner"))
```

## Requirements

- Emacs 26.1 or later
- [Spinor](https://github.com/yanqirenshi/Spinor) installed and in PATH

## License

MIT License. See [LICENSE](LICENSE) for details.

## Contributing

Contributions are welcome! Please open an issue or submit a pull request.

## See Also

- [Spinor Language](https://github.com/yanqirenshi/Spinor) - The Spinor compiler and runtime
- [Spinor Manual](https://yanqirenshi.github.io/Spinor/) - Online documentation
