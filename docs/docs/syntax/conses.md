# Conses

Spinor におけるコンスセルとリスト構造について解説します。

## コンスセル (Cons Cell)

コンスセルは Lisp の基本的なデータ構造で、2つの要素（`car` と `cdr`）を持つペアです。

```lisp
(cons 1 2)        ; => (1 . 2) ドット対
(cons 1 nil)      ; => (1)     1要素のリスト
(cons 1 '(2 3))   ; => (1 2 3) リストの先頭に追加
```

## リスト構造

リストはコンスセルの連鎖として表現されます。各コンスセルの `cdr` が次のコンスセルを指し、最後の `cdr` が `nil` になります。

```lisp
;; (1 2 3) の内部構造:
;; (cons 1 (cons 2 (cons 3 nil)))

'(1 2 3)          ; リストリテラル
(list 1 2 3)      ; list 関数による構築
```

## リストの分解

### car と cdr

```lisp
(car '(1 2 3))    ; => 1   先頭要素
(cdr '(1 2 3))    ; => (2 3) 残りのリスト

(car '(a))        ; => a
(cdr '(a))        ; => nil

(car nil)         ; => nil
(cdr nil)         ; => nil
```

### パターンマッチによる分解

```lisp
(match '(1 2 3)
  ((cons x xs) x))    ; => 1

(match '(1 2 3)
  ((cons x xs) xs))   ; => (2 3)
```

## リストの構築

### list 関数

```lisp
(list)            ; => nil (空リスト)
(list 1)          ; => (1)
(list 1 2 3)      ; => (1 2 3)
(list 'a 'b 'c)   ; => (a b c)
```

### クオートによる構築

```lisp
'()               ; => nil
'(1 2 3)          ; => (1 2 3)
'(a b c)          ; => (a b c)
```

## ドット対 (Dotted Pair)

`cdr` がリストでない場合、ドット対として表示されます。

```lisp
(cons 1 2)        ; => (1 . 2)
(cons 'a 'b)      ; => (a . b)
```

## 空リストの判定

```lisp
(null? nil)       ; => t
(null? '())       ; => t
(null? '(1))      ; => nil
(empty? '())      ; => t
```

## 再帰的なリスト処理

```lisp
;; リストの長さを計算
(def length (fn (lst)
  (match lst
    (nil 0)
    ((cons _ xs) (+ 1 (length xs))))))

(length '(a b c d))  ; => 4

;; リストの合計
(def sum (fn (lst)
  (match lst
    (nil 0)
    ((cons x xs) (+ x (sum xs))))))

(sum '(1 2 3 4 5))   ; => 15
```

## Symbols

| Type | Name | Description |
|:-----|:-----|:------------|
| Function | [cons](../ref/cons) | コンスセルを作成 |
| Function | [car](../ref/car) | コンスセルの先頭要素を取得 |
| Function | [cdr](../ref/cdr) | コンスセルの残りを取得 |
| Function | [list](../ref/list) | リストを作成 |
| Function | [null?](../ref/null-p) | `nil` かどうかを判定 |
| Function | [empty?](../ref/empty-p) | 空リストかどうかを判定 |
| Special Form | [quote](../ref/quote) | 評価を抑制してリストを作成 |
| Special Form | [match](../ref/match) | パターンマッチでリストを分解 |
