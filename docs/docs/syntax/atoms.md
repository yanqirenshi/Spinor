# Atoms

Spinor の最も基本的なデータ型です。

## 数値 (Numbers)

整数と浮動小数点数をサポートしています。

```lisp
42        ; 整数 (Int)
3.14      ; 浮動小数点数 (Float)
```

詳細は [Numbers](numbers) を参照してください。

## シンボル (Symbols)

変数名や関数名として使用される識別子です。

```lisp
x
my-variable
calculate-sum
```

詳細は [Symbols](symbols) を参照してください。

## 文字列 (Strings)

ダブルクオートで囲まれたテキストです。

```lisp
"Hello, World!"
"日本語も使えます"
```

詳細は [Strings](strings) を参照してください。

## 特殊シンボル

| シンボル | 意味 |
|:---------|:-----|
| `nil` | 空リスト / 偽値 |
| `t` | 真値 |

## 関連ページ

- [Symbols](symbols) - シンボルの性質と比較の詳細
- [Numbers](numbers) - 数値型と算術演算の詳細
- [Characters](characters) - 文字とエスケープシーケンス
- [Strings](strings) - 文字列操作の詳細
- [Conses](conses) - リスト構造とコンスセル

## Symbols

| Type | Name | Description |
|:-----|:-----|:------------|
| Function | [eq](ref/eq) | シンボルの同一性を比較 |
| Function | [equal](ref/equal) | 値の等価性を比較 |
| Function | [null?](ref/null-p) | `nil` かどうかを判定 |
| Function | [print](ref/print) | 値を出力 |
