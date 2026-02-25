# Atoms

Spinor の最も基本的なデータ型です。

## 数値 (Numbers)

整数リテラルをサポートしています。

```lisp
42        ; 正の整数
-17       ; 負の整数
0         ; ゼロ
```

## シンボル (Symbols)

変数名や関数名として使用される識別子です。

```lisp
x
my-variable
calculate-sum
string->list    ; 矢印を含むシンボル
null?           ; 疑問符で終わるシンボル (述語)
```

## 文字列 (Strings)

ダブルクオートで囲まれたテキストです。

```lisp
"Hello, World!"
"日本語も使えます"
"Line 1\nLine 2"    ; エスケープシーケンス
```

## 特殊シンボル

| シンボル | 意味 |
|:---------|:-----|
| `nil` | 空リスト / 偽値 |
| `t` | 真値 |

## Symbols

- [eq](../ref/eq) - シンボルの同一性を比較
- [equal](../ref/equal) - 値の等価性を比較
- [null?](../ref/null-p) - `nil` かどうかを判定
- [print](../ref/print) - 値を出力
- [string-length](../ref/string-length) - 文字列の長さを取得
- [string-append](../ref/string-append) - 文字列を連結
- [string-eq](../ref/string-eq) - 文字列の等価性を比較
