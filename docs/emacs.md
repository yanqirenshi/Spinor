# Emacs Integration

Spinor を Emacs で快適に開発するためのガイドです。

## spinor-mode.el

`spinor-mode` は `.spin` ファイル用の Emacs メジャーモードです。

### 機能

- シンタックスハイライト (キーワード、プリミティブ、型情報)
- REPL 連携 (`inferior-spinor-mode`)
- S式評価のショートカット

### インストール

```elisp
;; init.el または .emacs に追加
(add-to-list 'load-path "/path/to/Spinor/editors/emacs")
(require 'spinor-mode)
```

### キーバインド

| キー | 関数 | 説明 |
|------|------|------|
| `M-x run-spinor` | `run-spinor` | Spinor REPL を起動 |
| `C-x C-e` | `spinor-eval-last-sexp` | カーソル直前の S式を REPL に送信 |
| `C-c C-k` | `spinor-load-file` | バッファ全体を REPL にロード |
| `C-c C-z` | `spinor-switch-to-repl` | REPL バッファに切り替え |

### カスタマイズ

```elisp
;; REPL 起動コマンドを変更 (デフォルト: cabal -v0 run spinor)
(setq spinor-program "cabal")
(setq spinor-program-args '("-v0" "run" "spinor"))

;; REPL バッファ名を変更
(setq spinor-repl-buffer-name "*my-spinor*")
```

## SLY 連携

より高度な開発体験のために、Swank プロトコルを使用した SLY 連携が可能です。

### セットアップ

1. **Swank サーバーを起動**

```bash
cabal run spinor -- server --port 4005
```

2. **SLY で接続**

```
M-x sly-connect RET localhost RET 4005 RET
```

### SLY の機能

SLY 接続時は以下の機能が利用できます:

- **バッファ評価** — `C-M-x` で defun を評価
- **式評価** — `C-x C-e` でカーソル直前の式を評価
- **補完** — TAB による補完
- **インスペクト** — 値のインスペクション

### SLY の設定例

```elisp
;; SLY のインストール (MELPA から)
(use-package sly
  :ensure t)

;; Spinor 用の Lisp 実装として登録 (オプション)
(setq sly-lisp-implementations
      '((spinor ("cabal" "run" "spinor" "--" "server"))))
```

## ワークフロー例

### 1. REPL ベースの開発

```
1. M-x run-spinor で REPL を起動
2. .spin ファイルを編集
3. C-x C-e で式を評価しながら開発
4. C-c C-k でファイル全体をリロード
```

### 2. SLY ベースの開発

```
1. ターミナルで spinor server を起動
2. M-x sly-connect で接続
3. 通常の SLY ワークフローで開発
```

## トラブルシューティング

### REPL が起動しない

- `cabal build` が成功することを確認
- `spinor-program` と `spinor-program-args` の設定を確認

### 日本語が文字化けする

```elisp
;; UTF-8 を使用
(setq default-process-coding-system '(utf-8 . utf-8))
```

### SLY 接続がタイムアウトする

- サーバーが正しいポートで起動しているか確認
- ファイアウォールの設定を確認
