# Environment

Spinor の実行環境と環境変数について解説します。

## 実行環境

Spinor は複数の実行環境をサポートしています。

### インタプリタモード

REPL または `spinor` コマンドでファイルを実行する標準的なモードです。

```bash
# REPL の起動
cabal run spinor

# ファイルの実行
cabal run spinor -- script.spin
```

### ネイティブバイナリ

将来のバージョンでは、Spinor コードをネイティブバイナリにコンパイルする機能が計画されています。

### WebAssembly (WASM)

将来のバージョンでは、WASM への出力がサポートされる予定です。ブラウザや WASI 環境での実行が可能になります。

## コマンドライン引数

### command-line-args

スクリプト実行時のコマンドライン引数を取得します。

```lisp
;; script.spin
(def args (command-line-args))
(print args)
```

```bash
$ cabal run spinor -- script.spin arg1 arg2 arg3
("arg1" "arg2" "arg3")
```

### 引数の処理

```lisp
(def args (command-line-args))

;; 引数の数をチェック
(if (null? args)
    (print "Usage: script.spin <input-file>")
    (begin
      (def input-file (car args))
      (print (string-append "Processing: " input-file))))
```

## 環境変数

### getenv

OS の環境変数を取得します。

```lisp
(getenv "HOME")       ; => "/home/user"
(getenv "PATH")       ; => "/usr/bin:/bin:..."
(getenv "USER")       ; => "username"

;; 存在しない環境変数
(getenv "UNDEFINED")  ; => nil
```

### 環境変数の活用例

```lisp
;; ホームディレクトリの設定ファイル
(def home (getenv "HOME"))
(def config-path (string-append home "/.spinorrc"))

(if (file-exists? config-path)
    (load config-path)
    (print "No config file found"))

;; 環境に応じた動作切り替え
(def env (getenv "SPINOR_ENV"))
(def debug-mode (equal env "development"))

(if debug-mode
    (print "Running in debug mode")
    (print "Running in production mode"))
```

## 環境ごとの違い

### インタプリタ環境

- 完全な機能セットが利用可能
- ファイル IO、ネットワーク、OpenCL などすべてのプリミティブが使用可能
- `command-line-args` と `getenv` が完全に機能

### WASM 環境 (計画中)

- サンドボックス内で実行
- ファイル IO は WASI を通じて制限付きで利用可能
- `getenv` は WASI 経由で環境変数にアクセス
- ネイティブライブラリ (OpenCL, OpenGL) は利用不可

### 機能の検出

環境に応じて利用可能な機能を検出する方法が計画されています。

```lisp
;; 計画中の機能
(if (feature? 'file-io)
    (read-file "data.txt")
    (print "File IO not available"))
```

## 標準入出力

### print

標準出力に値を出力します。

```lisp
(print "Hello, World!")
(print 42)
(print '(1 2 3))
```

### 入力 (計画中)

標準入力からの読み取りは将来のバージョンで計画されています。

```lisp
;; 計画中の機能
(def input (read-line))
(print (string-append "You entered: " input))
```

## Symbols

| Type | Name | Description |
|:-----|:-----|:------------|
| Function | [print](../ref/print) | 標準出力に出力 |
| Function | [read-file](../ref/read-file) | ファイルを読み込み |
| Function | [write-file](../ref/write-file) | ファイルに書き込み |
