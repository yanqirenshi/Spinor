# Strings

Spinor における文字列型と操作について解説します。

## 文字列型 (String)

文字列はダブルクオートで囲まれたテキストです。内部的には Haskell の `Text` 型として表現されます。

```lisp
"Hello, World!"
"日本語も使えます"
""                  ; 空文字列
```

## 文字列リテラル

### 基本的な文字列

```lisp
"simple string"
"with spaces and punctuation!"
```

### 複数行文字列

エスケープシーケンス `\n` を使用して複数行を表現します。

```lisp
"Line 1\nLine 2\nLine 3"
```

### エスケープシーケンス

```lisp
"Tab:\tIndented"
"Quote: \"Hello\""
"Backslash: \\"
```

## 文字列操作

### 連結

```lisp
(string-append "Hello" " " "World")  ; => "Hello World"
```

### 長さの取得

```lisp
(string-length "Hello")  ; => 5
(string-length "日本語")  ; => 3 (文字数)
```

### 部分文字列

```lisp
(substring "Hello World" 0 5)  ; => "Hello"
(substring "Hello World" 6 11) ; => "World"
```

### 比較

```lisp
(string-eq "abc" "abc")  ; => t
(string-eq "abc" "ABC")  ; => nil (大文字小文字を区別)
```

## リストとの変換

```lisp
;; 文字列 -> 文字リスト
(string-to-list "abc")  ; => ("a" "b" "c")

;; 文字リスト -> 文字列
(list-to-string '("H" "e" "l" "l" "o"))  ; => "Hello"
```

## 型推論

```lisp
spinor> "Hello"
:: String
"Hello"

spinor> (string-append "a" "b")
:: String
"ab"
```

## Symbols

| Type | Name | Description |
|:-----|:-----|:------------|
| Function | [string-append](ref/string-append) | 文字列を連結 |
| Function | [string-length](ref/string-length) | 文字列の長さを取得 |
| Function | [string-eq](ref/string-eq) | 文字列の等価性を比較 |
| Function | [substring](ref/substring) | 部分文字列を取得 |
| Function | [string-to-list](ref/string-to-list) | 文字列を文字リストに変換 |
| Function | [list-to-string](ref/list-to-string) | 文字リストを文字列に変換 |
| Function | [print](ref/print) | 文字列を出力 |
