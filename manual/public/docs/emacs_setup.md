# Editor Setup Guide

Spinor の開発環境を構築するためのガイドです。Emacs を中心に、SLY/LSP 連携、プロジェクト管理、AI アシスタントの活用方法を解説します。

---

## 目次

1. [Emacs & spinor-mode](#1-emacs--spinor-mode)
2. [SLY Integration](#2-sly-integration)
3. [LSP Mode](#3-lsp-mode)
4. [Project Management](#4-project-management)
5. [AI-Assisted Development](#5-ai-assisted-development)

---

## 1. Emacs & spinor-mode

### インストール

`spinor-mode.el` は Spinor リポジトリの `editors/emacs/` に含まれています。

```elisp
;; load-path に追加
(add-to-list 'load-path "/path/to/Spinor/editors/emacs")
(require 'spinor-mode)
```

### use-package を使用した設定

```elisp
(use-package spinor-mode
  :load-path "/path/to/Spinor/editors/emacs"
  :mode ("\\.spin\\'" . spinor-mode)
  :custom
  (spinor-program "cabal")
  (spinor-program-args '("-v0" "run" "spinor"))
  :bind (:map spinor-mode-map
         ("C-x C-e" . spinor-eval-last-sexp)
         ("C-c C-k" . spinor-load-file)
         ("C-c C-z" . spinor-switch-to-repl)))
```

### 基本的なキーバインド

| キー | コマンド | 説明 |
|------|---------|------|
| `C-x C-e` | `spinor-eval-last-sexp` | カーソル直前の S 式を REPL で評価 |
| `C-c C-k` | `spinor-load-file` | 現在のファイルを REPL にロード |
| `C-c C-z` | `spinor-switch-to-repl` | REPL バッファに切り替え |
| `M-x run-spinor` | - | REPL を起動 |

### REPL の起動と使用

```
M-x run-spinor
```

REPL が `*spinor*` バッファで起動します。ソースファイルで `C-x C-e` を押すと、カーソル位置の式が REPL に送信されて評価されます。

---

## 2. SLY Integration

SLY は Common Lisp 用の対話的開発環境ですが、Spinor も Swank プロトコルをサポートしています。

### Spinor サーバーの起動

```bash
# デフォルトポート (4005) で起動
cabal run spinor -- server

# ポートを指定する場合
cabal run spinor -- server --port 4006
```

### SLY からの接続

```elisp
;; SLY のインストール (use-package)
(use-package sly
  :ensure t
  :commands sly-connect)
```

Emacs から接続:

```
M-x sly-connect RET localhost RET 4005
```

### SLY のキーバインド

接続後、以下のキーバインドが使用可能です:

| キー | 説明 |
|------|------|
| `C-c C-c` | カーソル位置の定義をコンパイル |
| `C-x C-e` | 式を評価して結果を表示 |
| `C-c C-d d` | シンボルのドキュメントを表示 |
| `M-.` | 定義にジャンプ |
| `M-,` | ジャンプ元に戻る |

### 推奨設定

```elisp
(use-package sly
  :ensure t
  :config
  ;; Spinor サーバーへの接続用関数
  (defun sly-connect-spinor ()
    "Connect to a running Spinor server."
    (interactive)
    (sly-connect "localhost" 4005))

  :bind (("C-c s c" . sly-connect-spinor)))
```

---

## 3. LSP Mode

Spinor は Language Server Protocol (LSP) をサポートしており、リアルタイムの診断、補完、ホバー表示が可能です。

### LSP サーバーの確認

```bash
# LSP サーバーとして起動 (stdio モード)
cabal run spinor -- lsp
```

### lsp-mode の設定

```elisp
(use-package lsp-mode
  :ensure t
  :hook (spinor-mode . lsp-deferred)
  :commands (lsp lsp-deferred)
  :init
  ;; Spinor LSP クライアントを登録
  (with-eval-after-load 'lsp-mode
    (add-to-list 'lsp-language-id-configuration
                 '(spinor-mode . "spinor"))

    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection
                       (lambda ()
                         '("cabal" "-v0" "run" "spinor" "--" "lsp")))
      :major-modes '(spinor-mode)
      :server-id 'spinor-lsp
      :priority -1)))

  :custom
  (lsp-log-io nil)  ; デバッグ時は t に設定
  (lsp-idle-delay 0.5))
```

### eglot の設定 (軽量な代替)

```elisp
(use-package eglot
  :ensure t
  :hook (spinor-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               '(spinor-mode . ("cabal" "-v0" "run" "spinor" "--" "lsp"))))
```

### LSP 機能

| 機能 | 説明 |
|------|------|
| **Diagnostics** | パースエラー・型エラーをリアルタイム表示 |
| **Hover** | シンボル上でドキュメントと型情報を表示 |
| **Completion** | 組み込み関数の補完候補を表示 |

---

## 4. Project Management

### Projectile との統合

```elisp
(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)

  ;; .spin ファイルを含むディレクトリをプロジェクトとして認識
  (add-to-list 'projectile-project-root-files "spinor.cabal")

  :bind-keymap
  ("C-c p" . projectile-command-map))
```

### Org mode によるリテラルプログラミング

Org mode のソースブロックで Spinor コードを記述し、ドキュメントとコードを一体化できます。

```org
#+begin_src spinor :tangle factorial.spin
;; 階乗を計算する関数
(defun factorial (n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(print (factorial 10))
#+end_src
```

Org-babel の設定:

```elisp
;; Spinor 用の org-babel 設定
(defun org-babel-execute:spinor (body params)
  "Execute a block of Spinor code with org-babel."
  (let ((tmp-file (make-temp-file "spinor" nil ".spin")))
    (with-temp-file tmp-file
      (insert body))
    (shell-command-to-string
     (format "cabal -v0 run spinor -- %s" tmp-file))))

(add-to-list 'org-src-lang-modes '("spinor" . spinor))
```

### タスク管理

プロジェクトのタスクを Org mode で管理する例:

```org
* Spinor Project Tasks
** TODO 新しいプリミティブを実装
   SCHEDULED: <2026-03-01>
   - [ ] 仕様を確認
   - [ ] Haskell 側の実装
   - [ ] テストを追加

** DONE JSON パーサーの改善
   CLOSED: [2026-02-25]
```

---

## 5. AI-Assisted Development

### Claude Code

[Claude Code](https://claude.ai/claude-code) は Anthropic の AI コーディングアシスタントです。

**インストール:**
```bash
npm install -g @anthropic-ai/claude-code
```

**使用方法:**
```bash
cd /path/to/Spinor
claude
```

**プロジェクト設定 (CLAUDE.md):**

`CLAUDE.md` ファイルをプロジェクトルートに配置すると、Claude がプロジェクトのコンテキストを理解します。

```markdown
# Spinor Project Context for Claude

## プロジェクト概要
- **言語:** Haskell で実装された Lisp 処理系
- **コンセプト:** 静的型付け Lisp

## ビルドコマンド
- `cabal build` - ビルド
- `cabal test` - テスト実行
- `cabal run spinor` - REPL 起動

## ディレクトリ構成
- `src/Spinor/` - Haskell カーネル
- `twister/` - 標準ライブラリ (Spinor)
- `manual/` - ドキュメント
```

**タスクの実行例:**
```bash
# テストを実行してエラーを修正
claude "cabal test を実行して、失敗しているテストを修正してください"

# 新機能の実装
claude "tasks/43f_editor_setup_task.md を読んで実装してください"
```

### Gemini CLI

Google の Gemini CLI も同様のワークフローで使用できます。

```bash
# インストール
npm install -g @anthropic-ai/gemini-cli

# 使用
cd /path/to/Spinor
gemini
```

### AI とのコンテキスト共有のコツ

1. **明確なプロジェクト構造**: `CLAUDE.md` や `README.md` に技術スタックとディレクトリ構成を記載
2. **タスク指示書**: `tasks/` ディレクトリに具体的な実装手順を記載した Markdown ファイルを配置
3. **型情報の活用**: Spinor の型システムについて説明し、型エラーの解釈を依頼
4. **テストファースト**: テストケースを先に書いて、実装を依頼

### Emacs + AI の統合

```elisp
;; gptel: Emacs 内で LLM を使用
(use-package gptel
  :ensure t
  :config
  (setq gptel-model "claude-3-opus")
  (setq gptel-default-mode 'org-mode)

  :bind
  ("C-c g" . gptel-send))

;; コード選択 → AI に送信するワークフロー
(defun spinor-explain-region ()
  "Send selected region to AI for explanation."
  (interactive)
  (let ((code (buffer-substring-no-properties
               (region-beginning) (region-end))))
    (gptel-send (format "以下の Spinor コードを説明してください:\n\n```lisp\n%s\n```" code))))
```

---

## 完全な設定例

以下は、上記すべてを統合した `init.el` の設定例です:

```elisp
;;; Spinor Development Environment

;; spinor-mode
(use-package spinor-mode
  :load-path "/path/to/Spinor/editors/emacs"
  :mode ("\\.spin\\'" . spinor-mode)
  :custom
  (spinor-program "cabal")
  (spinor-program-args '("-v0" "run" "spinor")))

;; LSP support
(use-package lsp-mode
  :ensure t
  :hook (spinor-mode . lsp-deferred)
  :init
  (with-eval-after-load 'lsp-mode
    (add-to-list 'lsp-language-id-configuration
                 '(spinor-mode . "spinor"))
    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection
                       '("cabal" "-v0" "run" "spinor" "--" "lsp"))
      :major-modes '(spinor-mode)
      :server-id 'spinor-lsp))))

;; SLY for interactive development
(use-package sly
  :ensure t
  :config
  (defun sly-connect-spinor ()
    (interactive)
    (sly-connect "localhost" 4005)))

;; Company for completion
(use-package company
  :ensure t
  :hook (spinor-mode . company-mode))

;; Flycheck for diagnostics
(use-package flycheck
  :ensure t
  :hook (spinor-mode . flycheck-mode))
```

---

## トラブルシューティング

### REPL が起動しない

```elisp
;; cabal のパスを確認
(setq spinor-program "/path/to/cabal")

;; または直接 spinor バイナリを指定
(setq spinor-program "/path/to/spinor")
(setq spinor-program-args nil)
```

### LSP が接続できない

```bash
# LSP サーバーが正常に起動するか確認
cabal run spinor -- lsp

# ログを有効にしてデバッグ
```

```elisp
(setq lsp-log-io t)
```

### SLY 接続エラー

```bash
# サーバーが起動しているか確認
netstat -an | grep 4005

# サーバーを再起動
cabal run spinor -- server
```

---

## 関連ドキュメント

- [Introduction](introduction) - インストールと入門ガイド
- [Build Guide](build) - ビルド環境の構築
- [Architecture](architecture) - 内部アーキテクチャ
- [API Reference](api-index) - 全プリミティブ一覧
