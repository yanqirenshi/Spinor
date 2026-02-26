# Files

Spinor におけるファイル操作について解説します。

## 概要

Spinor はファイルの読み書きのためのプリミティブ関数を提供しています。これらの関数は副作用を伴う IO 操作です。

## ファイルの読み込み

### read-file

ファイルの内容を文字列として読み込みます。

```lisp
(read-file "path/to/file.txt")
; => "ファイルの内容..."

;; 変数に格納
(def content (read-file "data.txt"))
(print content)
```

### 相対パスと絶対パス

```lisp
;; 相対パス (カレントディレクトリから)
(read-file "config.txt")
(read-file "./data/input.txt")

;; 絶対パス
(read-file "/home/user/documents/file.txt")
```

## ファイルの書き込み

### write-file

ファイルに文字列を書き込みます。既存のファイルは上書きされます。

```lisp
(write-file "output.txt" "Hello, World!")

;; 複数行の書き込み
(write-file "lines.txt" "Line 1\nLine 2\nLine 3")

;; 計算結果の保存
(def result (+ 1 2 3 4 5))
(write-file "result.txt" (string-append "Result: " (print result)))
```

### append-file

ファイルの末尾に文字列を追加します。ファイルが存在しない場合は新規作成されます。

```lisp
(append-file "log.txt" "Log entry 1\n")
(append-file "log.txt" "Log entry 2\n")
(append-file "log.txt" "Log entry 3\n")

;; ログ関数の例
(def log (fn (message)
  (append-file "app.log"
    (string-append message "\n"))))

(log "Application started")
(log "Processing data...")
```

## ファイルの存在確認

### file-exists?

ファイルが存在するかどうかを確認します。

```lisp
(file-exists? "config.txt")    ; => t または nil

;; 条件分岐での使用
(if (file-exists? "data.txt")
    (read-file "data.txt")
    "File not found")

;; 安全なファイル読み込み
(def safe-read (fn (path default)
  (if (file-exists? path)
      (read-file path)
      default)))

(safe-read "config.txt" "{}")
```

## エラー処理

ファイル操作は失敗する可能性があります。存在確認を事前に行うか、適切なデフォルト値を用意してください。

```lisp
;; 存在確認してから読み込み
(def load-config (fn (path)
  (if (file-exists? path)
      (read-file path)
      (begin
        (print "Config not found, using defaults")
        "{}"))))

;; 書き込み結果の確認
(write-file "output.txt" "data")
(if (file-exists? "output.txt")
    (print "Write successful")
    (print "Write failed"))
```

## パス操作

現在の Spinor では、パス操作の専用関数は提供されていません。文字列操作でパスを構築してください。

```lisp
;; パスの結合
(def join-path (fn (dir file)
  (string-append dir "/" file)))

(join-path "data" "input.txt")  ; => "data/input.txt"
```

## 使用例

### 設定ファイルの読み込み

```lisp
(def config-path "config.txt")

(def load-settings (fn ()
  (if (file-exists? config-path)
      (read-file config-path)
      (begin
        (write-file config-path "default=true")
        "default=true"))))
```

### ログファイルへの記録

```lisp
(def log-file "application.log")

(def log-message (fn (level msg)
  (append-file log-file
    (string-append "[" level "] " msg "\n"))))

(log-message "INFO" "Application started")
(log-message "DEBUG" "Processing item 1")
(log-message "ERROR" "Something went wrong")
```

## Symbols

| Type | Name | Description |
|:-----|:-----|:------------|
| Function | [read-file](ref/read-file) | ファイルを読み込み |
| Function | [write-file](ref/write-file) | ファイルに書き込み |
| Function | [append-file](ref/append-file) | ファイルに追記 |
| Function | [file-exists?](ref/file-exists-p) | ファイルの存在確認 |
