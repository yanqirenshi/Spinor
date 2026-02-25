# Type System

Spinor は Hindley-Milner 型推論に基づく静的型付け言語です。

## 型推論

明示的な型注釈なしで、コンパイル時に型が推論されます。

```lisp
spinor> (+ 1 2)
:: Int
3

spinor> (cons 1 '(2 3))
:: (List Int)
(1 2 3)

spinor> (fn (x) (* x 2))
:: (-> Int Int)
#<fn>
```

## 基本型

| 型 | 説明 | 例 |
|:---|:-----|:---|
| `Int` | 整数 | `42`, `-17` |
| `Float` | 浮動小数点数 | `3.14`, `-2.5` |
| `String` | 文字列 | `"hello"` |
| `Bool` | 真偽値 | `t`, `nil` |
| `(List a)` | リスト | `'(1 2 3)` |
| `(-> a b)` | 関数型 | `(fn (x) x)` |

## 型表示: `::`

REPL では評価結果の前に `::` で型が表示されます。

```lisp
spinor> (list 1 2 3)
:: (List Int)
(1 2 3)

spinor> (def id (fn (x) x))
:: (-> a a)
id
```

## 多相性

型パラメータ (`a`, `b` など) による多相関数をサポートしています。

```lisp
;; 恒等関数: 任意の型を受け取り、そのまま返す
(def id (fn (x) x))
;; :: (-> a a)

;; リスト操作: 任意の要素型のリストを処理
(def length (fn (lst)
  (match lst
    (nil 0)
    ((cons _ xs) (+ 1 (length xs))))))
;; :: (-> (List a) Int)
```

## Symbols

| Type | Name | Description |
|:-----|:-----|:------------|
| Special Form | [fn](../ref/fn) | 関数型 `(-> a b)` を持つ無名関数を作成 |
| Function | [list](../ref/list) | `(List a)` 型のリストを作成 |
| Function | [cons](../ref/cons) | リスト型の値を構築 |
| Special Form | [def](../ref/def) | 多相型を持つ値を定義 |
