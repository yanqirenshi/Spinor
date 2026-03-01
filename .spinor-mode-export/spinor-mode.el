;;; spinor-mode.el --- Major mode for Spinor programming language -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Spinor Project

;; Author: Spinor Project <spinor@example.com>
;; Maintainer: Spinor Project <spinor@example.com>
;; URL: https://github.com/yanqirenshi/Spinor
;; Version: 0.1.0
;; Keywords: languages, lisp, spinor
;; Package-Requires: ((emacs "26.1"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the MIT License.

;;; Commentary:

;; Spinor is a statically-typed Lisp with Haskell-style semantics.
;; This package provides an Emacs major mode for editing Spinor source files.
;;
;; Features:
;;   - Syntax highlighting for .spin files
;;   - REPL integration via comint
;;   - LSP support (via lsp-mode or eglot)
;;   - SLY/SLIME support via Swank server
;;
;; Quick start:
;;   1. Install this package
;;   2. Open a .spin file - spinor-mode activates automatically
;;   3. M-x run-spinor to start the REPL
;;
;; Key bindings:
;;   C-x C-e   Evaluate the S-expression before point
;;   C-c C-k   Load the current file into the REPL
;;   C-c C-z   Switch to the REPL buffer
;;
;; For LSP support, add to your init.el:
;;   (add-to-list \\='lsp-language-id-configuration \\='(spinor-mode . \"spinor\"))
;;   (lsp-register-client
;;    (make-lsp-client :new-connection (lsp-stdio-connection \\='(\"spinor\" \"lsp\"))
;;                     :major-modes \\='(spinor-mode)
;;                     :server-id \\='spinor-lsp))
;;
;; For SLY support:
;;   1. Start the Swank server: spinor server --port 4005
;;   2. Connect from Emacs: M-x sly-connect RET localhost RET 4005 RET

;;; Code:

(require 'comint)
(require 'lisp-mode)

;;; Customization

(defgroup spinor nil
  "Customization group for Spinor development environment."
  :group 'languages
  :prefix "spinor-")

(defcustom spinor-program "spinor"
  "Command to invoke the Spinor interpreter.
This can be an absolute path or a command available in PATH."
  :type 'string
  :group 'spinor)

(defcustom spinor-program-args nil
  "List of arguments to pass to the Spinor interpreter.
These arguments are passed when starting the REPL."
  :type '(repeat string)
  :group 'spinor)

(defcustom spinor-repl-buffer-name "*spinor*"
  "Name of the buffer for the Spinor REPL."
  :type 'string
  :group 'spinor)

;;; Syntax Highlighting

(defvar spinor-font-lock-keywords
  (list
   ;; Definition keywords
   (cons (regexp-opt '("def" "defun" "defmacro" "mac" "fn" "let" "data"
                       "defpackage" "in-package" "linear" "with-region")
                     'symbols)
         'font-lock-keyword-face)
   ;; Control flow keywords
   (cons (regexp-opt '("if" "cond" "when" "unless" "match" "begin"
                       "load" "quote" "setq" "drop")
                     'symbols)
         'font-lock-builtin-face)
   ;; Boolean constants
   (cons (regexp-opt '("#t" "#f" "nil" "Nothing") 'symbols)
         'font-lock-constant-face)
   ;; Built-in functions
   (cons (regexp-opt '("cons" "car" "cdr" "list" "map" "filter" "fold"
                       "null?" "length" "append" "reverse" "print"
                       "not" "eq" "equal" "assert" "test"
                       "+" "-" "*" "/" "%" "=" "<" ">" "<=" ">=")
                     'symbols)
         'font-lock-function-name-face)
   ;; Type constructors (capitalized identifiers)
   (cons "\\<[A-Z][a-zA-Z0-9]*\\>" 'font-lock-type-face))
  "Font lock keywords for `spinor-mode'.")

;;; Keymap

(defvar spinor-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map lisp-mode-shared-map)
    (define-key map (kbd "C-x C-e") #'spinor-eval-last-sexp)
    (define-key map (kbd "C-c C-k") #'spinor-load-file)
    (define-key map (kbd "C-c C-z") #'spinor-switch-to-repl)
    map)
  "Keymap for `spinor-mode'.")

;;; Major Mode

;;;###autoload
(define-derived-mode spinor-mode lisp-mode "Spinor"
  "Major mode for editing Spinor source code.

Spinor is a statically-typed Lisp with Haskell-style type inference
and strict evaluation semantics.

\\{spinor-mode-map}"
  ;; Comment settings
  (setq-local comment-start "; ")
  (setq-local comment-start-skip ";+ *")
  ;; Font lock
  (setq-local font-lock-defaults '(spinor-font-lock-keywords)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.spin\\'" . spinor-mode))

;;; Inferior Spinor Mode (REPL)

(defvar inferior-spinor-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map comint-mode-map)
    map)
  "Keymap for `inferior-spinor-mode'.")

(define-derived-mode inferior-spinor-mode comint-mode "Inferior Spinor"
  "Major mode for interacting with the Spinor REPL.

This mode is derived from `comint-mode' and provides interaction
with a running Spinor interpreter process.

\\{inferior-spinor-mode-map}"
  (setq comint-prompt-regexp "^spinor> ")
  (setq comint-prompt-read-only t)
  ;; Apply Spinor font lock to REPL output
  (setq-local font-lock-defaults '(spinor-font-lock-keywords)))

;;; REPL Functions

;;;###autoload
(defun run-spinor ()
  "Start the Spinor REPL.
If a REPL process is already running, switch to its buffer."
  (interactive)
  (let ((buf (get-buffer spinor-repl-buffer-name)))
    (if (and buf (comint-check-proc buf))
        (pop-to-buffer buf)
      (let ((buf (apply #'make-comint-in-buffer
                        "spinor"
                        spinor-repl-buffer-name
                        spinor-program
                        nil
                        spinor-program-args)))
        (with-current-buffer buf
          (inferior-spinor-mode))
        (pop-to-buffer buf)))))

(defun spinor--get-process ()
  "Return the Spinor REPL process.
Signal an error if the REPL is not running."
  (let ((proc (get-buffer-process spinor-repl-buffer-name)))
    (unless proc
      (error "Spinor REPL is not running.  Start it with M-x run-spinor"))
    proc))

(defun spinor-eval-last-sexp ()
  "Evaluate the S-expression before point in the Spinor REPL."
  (interactive)
  (let* ((end (point))
         (beg (save-excursion (backward-sexp) (point)))
         (str (buffer-substring-no-properties beg end))
         (proc (spinor--get-process)))
    (comint-send-string proc (concat str "\n"))
    (display-buffer spinor-repl-buffer-name)))

(defun spinor-load-file ()
  "Load the current buffer's file into the Spinor REPL.
This sends a `(load \"filename\")' command to the REPL."
  (interactive)
  (save-buffer)
  (let* ((file (buffer-file-name))
         (proc (spinor--get-process))
         (cmd (format "(load \"%s\")\n" file)))
    (comint-send-string proc cmd)
    (display-buffer spinor-repl-buffer-name)))

(defun spinor-switch-to-repl ()
  "Switch to the Spinor REPL buffer.
If the REPL is not running, start it first."
  (interactive)
  (let ((buf (get-buffer spinor-repl-buffer-name)))
    (if (and buf (comint-check-proc buf))
        (pop-to-buffer buf)
      (run-spinor))))

(provide 'spinor-mode)

;;; spinor-mode.el ends here
