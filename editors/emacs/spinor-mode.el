;;; spinor-mode.el --- Major mode for Spinor (静的型付け Lisp) -*- lexical-binding: t; -*-

;; Author: Spinor Project
;; Version: 0.1.0
;; Keywords: languages, lisp
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:

;; Spinor 言語用の Emacs メジャーモード。
;; .spin ファイルのシンタックスハイライトと、REPL 連携機能を提供する。
;;
;; 機能:
;;   - spinor-mode: .spin ファイル用メジャーモード (lisp-mode 継承)
;;   - inferior-spinor-mode: REPL 用モード (comint-mode 継承)
;;   - run-spinor: REPL プロセスの起動
;;   - C-x C-e: カーソル直前の S式を REPL に送信
;;   - C-c C-k: バッファ全体をファイルとして REPL にロード
;;
;; 設定例:
;;   (add-to-list 'load-path "/path/to/Spinor/editors/emacs")
;;   (require 'spinor-mode)

;;; Code:

(require 'comint)
(require 'lisp-mode)

;; ============================================================
;; カスタマイズ変数
;; ============================================================

(defgroup spinor nil
  "Spinor 言語の開発環境設定。"
  :group 'languages
  :prefix "spinor-")

(defcustom spinor-program "cabal"
  "Spinor REPL を起動するプログラム。"
  :type 'string
  :group 'spinor)

(defcustom spinor-program-args '("-v0" "run" "spinor")
  "Spinor REPL プログラムに渡す引数リスト。
`-v0' は cabal のビルドログを抑制する。"
  :type '(repeat string)
  :group 'spinor)

(defcustom spinor-repl-buffer-name "*spinor*"
  "Spinor REPL バッファの名前。"
  :type 'string
  :group 'spinor)

;; ============================================================
;; シンタックスハイライト
;; ============================================================

(defvar spinor-font-lock-keywords
  (list
   ;; 定義系キーワード
   (cons (regexp-opt '("define" "def" "mac" "fn" "let") 'symbols)
         'font-lock-keyword-face)
   ;; 制御系キーワード
   (cons (regexp-opt '("if" "cond" "when" "load" "quote") 'symbols)
         'font-lock-builtin-face)
   ;; 定数
   (cons (regexp-opt '("#t" "#f" "nil") 'symbols)
         'font-lock-constant-face)
   ;; プリミティブ関数
   (cons (regexp-opt '("cons" "car" "cdr" "list" "map" "filter" "foldl" "foldr"
                       "null?" "empty?" "length" "append" "reverse" "print"
                       "not" "even?" "odd?" "fact" "fib")
                     'symbols)
         'font-lock-function-name-face)
   ;; 型アノテーション (:: で始まる行)
   (cons "^:: .*$" 'font-lock-type-face))
  "Spinor モード用のフォントロック定義。")

;; ============================================================
;; spinor-mode (メジャーモード)
;; ============================================================

(defvar spinor-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map lisp-mode-shared-map)
    (define-key map (kbd "C-x C-e") #'spinor-eval-last-sexp)
    (define-key map (kbd "C-c C-k") #'spinor-load-file)
    (define-key map (kbd "C-c C-z") #'spinor-switch-to-repl)
    map)
  "spinor-mode 用キーマップ。")

;;;###autoload
(define-derived-mode spinor-mode lisp-mode "Spinor"
  "Spinor 言語用のメジャーモード。

\\{spinor-mode-map}"
  ;; コメント設定
  (setq-local comment-start "; ")
  (setq-local comment-start-skip ";+ *")
  ;; フォントロック
  (setq-local font-lock-defaults '(spinor-font-lock-keywords)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.spin\\'" . spinor-mode))

;; ============================================================
;; inferior-spinor-mode (REPL モード)
;; ============================================================

(defvar inferior-spinor-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map comint-mode-map)
    map)
  "inferior-spinor-mode 用キーマップ。")

(define-derived-mode inferior-spinor-mode comint-mode "Inferior Spinor"
  "Spinor REPL 用のモード。

\\{inferior-spinor-mode-map}"
  (setq comint-prompt-regexp "^spinor> ")
  (setq comint-prompt-read-only t)
  ;; REPL 出力のフォントロック
  (setq-local font-lock-defaults '(spinor-font-lock-keywords)))

;; ============================================================
;; REPL 起動・連携
;; ============================================================

;;;###autoload
(defun run-spinor ()
  "Spinor REPL を起動する。
既に起動済みの場合はそのバッファに切り替える。"
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
  "Spinor REPL プロセスを取得する。未起動ならエラー。"
  (let ((proc (get-buffer-process spinor-repl-buffer-name)))
    (unless proc
      (error "Spinor REPL が起動していません。M-x run-spinor で起動してください"))
    proc))

(defun spinor-eval-last-sexp ()
  "カーソル直前の S式を Spinor REPL に送信して評価する。"
  (interactive)
  (let* ((end (point))
         (beg (save-excursion (backward-sexp) (point)))
         (str (buffer-substring-no-properties beg end))
         (proc (spinor--get-process)))
    (comint-send-string proc (concat str "\n"))
    (display-buffer spinor-repl-buffer-name)))

(defun spinor-load-file ()
  "現在のバッファのファイルを Spinor REPL にロードする。
`(load \"ファイルパス\")' を REPL に送信する。"
  (interactive)
  (save-buffer)
  (let* ((file (buffer-file-name))
         (proc (spinor--get-process))
         (cmd (format "(load \"%s\")\n" file)))
    (comint-send-string proc cmd)
    (display-buffer spinor-repl-buffer-name)))

(defun spinor-switch-to-repl ()
  "Spinor REPL バッファに切り替える。未起動なら起動する。"
  (interactive)
  (let ((buf (get-buffer spinor-repl-buffer-name)))
    (if (and buf (comint-check-proc buf))
        (pop-to-buffer buf)
      (run-spinor))))

(provide 'spinor-mode)

;;; spinor-mode.el ends here
