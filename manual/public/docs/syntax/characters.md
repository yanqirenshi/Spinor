# Characters

Spinor における文字の扱いについて解説します。

## 文字型

Spinor では文字は文字列の構成要素として扱われます。独立した文字型 (`Char`) は現在サポートされていませんが、長さ1の文字列として表現できます。

```lisp
"a"           ; 単一文字の文字列
"あ"          ; Unicode 文字
```

## エスケープシーケンス

文字列内で特殊文字を表現するためのエスケープシーケンスをサポートしています。

| シーケンス | 意味 |
|:-----------|:-----|
| `\n` | 改行 (LF) |
| `\t` | タブ |
| `\r` | 復帰 (CR) |
| `\\` | バックスラッシュ |
| `\"` | ダブルクオート |

```lisp
"Line 1\nLine 2"    ; 改行を含む文字列
"Tab:\tHere"        ; タブを含む文字列
"Say \"Hello\""     ; クオートを含む文字列
```

## Unicode サポート

Spinor は UTF-8 エンコーディングをサポートしており、日本語やその他の Unicode 文字を直接使用できます。

```lisp
"こんにちは"
"Hello, 世界!"
"Emoji: 🎉"
```

## 文字列との関係

文字列を文字のリストとして扱うことができます。

```lisp
(string-to-list "abc")    ; => ("a" "b" "c")
(list-to-string '("H" "i"))  ; => "Hi"
```

## Symbols

| Type | Name | Description |
|:-----|:-----|:------------|
| Function | [string-to-list](../api/string-to-list) | 文字列を文字リストに変換 |
| Function | [list-to-string](../api/list-to-string) | 文字リストを文字列に変換 |
| Function | [string-length](../api/string-length) | 文字列の長さを取得 |
| Function | [substring](../api/substring) | 部分文字列を取得 |
