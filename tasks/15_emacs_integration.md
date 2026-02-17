# タスク: ステップ15 - Emacs 開発環境 (spinor-mode.el) の構築

## 目標

Emacs ユーザーである開発者のために、Spinor 専用のメジャーモードを作成する。
SLY 相当とまではいかないが、`run-spinor` による REPL 起動と、バッファからのコード送信 (`eval-last-sexp`) を可能にする。

## 実装詳細指示

### 1. ディレクトリ作成

* プロジェクトルートに `editors/emacs/` ディレクトリを作成してください。

### 2. `editors/emacs/spinor-mode.el` の作成

以下の機能を持つ Emacs Lisp ファイルを作成してください。

* **`spinor-mode` (メジャーモード):**
    * `lisp-mode` を継承（これで基本的なインデントや括弧処理は自動で対応されます）。
    * 拡張子 `.spin` に関連付け。
    * **キーワードのハイライト:**
        * 定義系: `define`, `def`, `mac`, `fn`, `let`
        * 制御系: `if`, `cond`, `load`, `quote`
        * 定数: `#t`, `#f`, `nil`
    * コメント文字を `;` に設定。

* **`inferior-spinor-mode` (REPLモード):**
    * `comint-mode` を継承。
    * プロンプトの正規表現: `^spinor> `
    * `cabal run spinor` (またはバイナリ直接指定) でプロセスを起動する `run-spinor` コマンドを実装。
        * ※ `cabal` のビルドログが邪魔にならないよう、`cabal -v0 run spinor` とする等の工夫があると良い。

* **キーバインドと連携:**
    * `C-x C-e`: カーソル直前の S式 を REPL プロセスに送信して評価する機能。
    * `C-c C-k`: バッファ全体をロード (`load` コマンドを送信) する機能。

## コードのイメージ (参考)

```elisp
(defun spinor-eval-last-sexp ()
  (interactive)
  (let ((str (buffer-substring-no-properties ...)))
    (comint-send-string "*spinor*" (concat str "\n"))))

```

## 出力要件

* `editors/emacs/spinor-mode.el` の完全なコード。
* ユーザー (Emacs設定ファイル) への設定手順の解説。

# 実装方針

## 概要

Emacs 用の Spinor 専用メジャーモード (`spinor-mode`) と REPL 連携 (`inferior-spinor-mode`) を実装する。`.spin` ファイル編集時のシンタックスハイライトと、バッファからの対話的なコード送信を可能にする。

## 設計判断

### lisp-mode の継承

`spinor-mode` は `lisp-mode` を継承する。基本的なインデントや括弧処理は自動で対応される。Spinor は Lisp 系言語なので、既存の Lisp モードの資産を活用する。

### comint-mode ベースの REPL

`inferior-spinor-mode` は `comint-mode` を継承。プロンプト正規表現 `^spinor> ` でプロセス出力を処理する。`cabal -v0 run spinor` でビルドログを抑制。

### シンタックスハイライトの分類

- 定義系 (`define`, `def`, `mac`, `fn`, `let`) → `font-lock-keyword-face`
- 制御系 (`if`, `cond`, `when`, `load`, `quote`) → `font-lock-builtin-face`
- 定数 (`#t`, `#f`, `nil`) → `font-lock-constant-face`
- プリミティブ関数 (`cons`, `car`, `map` 等) → `font-lock-function-name-face`
- 型アノテーション (`:: ...`) → `font-lock-type-face`

### キーバインド設計

- `C-x C-e`: カーソル直前の S式を REPL に送信 (`spinor-eval-last-sexp`)
- `C-c C-k`: バッファファイル全体をロード (`spinor-load-file`)
- `C-c C-z`: REPL バッファに切り替え (`spinor-switch-to-repl`)

### カスタマイズ変数

`spinor-program`, `spinor-program-args`, `spinor-repl-buffer-name` をカスタマイズ可能にし、環境ごとの柔軟な設定を可能にする。

## 変更の流れ

1. `editors/emacs/spinor-mode.el` (新規) — メジャーモード、REPL モード、キーバインド、ハイライト定義

# 実装内容

## 新規ファイル

| ファイル | 概要 |
|---|---|
| `editors/emacs/spinor-mode.el` | Spinor 用 Emacs メジャーモード + REPL 連携 |

## 機能一覧

### spinor-mode (メジャーモード)

- `lisp-mode` を継承。基本的なインデント・括弧処理は自動対応。
- `.spin` 拡張子に自動関連付け (`auto-mode-alist`)。
- コメント文字: `;`
- **シンタックスハイライト:**
  - 定義系 (`define`, `def`, `mac`, `fn`, `let`) → `font-lock-keyword-face`
  - 制御系 (`if`, `cond`, `when`, `load`, `quote`) → `font-lock-builtin-face`
  - 定数 (`#t`, `#f`, `nil`) → `font-lock-constant-face`
  - プリミティブ関数 (`cons`, `car`, `map`, `filter` 等) → `font-lock-function-name-face`
  - 型アノテーション (`:: ...`) → `font-lock-type-face`

### inferior-spinor-mode (REPL モード)

- `comint-mode` を継承。
- プロンプト正規表現: `^spinor> `
- プロンプトは読み取り専用 (`comint-prompt-read-only`)。

### コマンド

| コマンド | キーバインド | 説明 |
|---|---|---|
| `run-spinor` | `M-x run-spinor` | REPL 起動 (`cabal -v0 run spinor`)。既存プロセスがあれば切り替え |
| `spinor-eval-last-sexp` | `C-x C-e` | カーソル直前の S式を REPL に送信 |
| `spinor-load-file` | `C-c C-k` | 現在のバッファファイルを `(load ...)` で REPL にロード |
| `spinor-switch-to-repl` | `C-c C-z` | REPL バッファに切り替え (未起動なら起動) |

### カスタマイズ変数

| 変数 | デフォルト | 説明 |
|---|---|---|
| `spinor-program` | `"cabal"` | REPL 起動プログラム |
| `spinor-program-args` | `'("-v0" "run" "spinor")` | プログラム引数 (`-v0` でビルドログ抑制) |
| `spinor-repl-buffer-name` | `"*spinor*"` | REPL バッファ名 |

## 設定手順

Emacs の設定ファイル (`~/.emacs.d/init.el` 等) に以下を追加:

```elisp
;; Spinor モードのロードパスを追加
(add-to-list 'load-path "/path/to/Spinor/editors/emacs")
(require 'spinor-mode)

;; (オプション) プロジェクトルートから実行する場合
;; (setq spinor-program-args '("-v0" "run" "spinor"))
```

### 使い方

1. `.spin` ファイルを開くと自動的に `spinor-mode` が有効になる
2. `M-x run-spinor` で REPL を起動
3. `.spin` バッファで `C-x C-e` でカーソル前の式を評価
4. `C-c C-k` でファイル全体をロード
5. `C-c C-z` で REPL バッファに切り替え
