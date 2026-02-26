# Data Types

代数的データ型とパターンマッチについて解説します。

## 代数的データ型: `data`

カスタムデータ型を定義します。

```lisp
;; Maybe 型
(data Maybe
  (Just val)
  Nothing)

;; 二分木
(data Tree
  (Leaf val)
  (Node left right))

;; 使用例
(Just 42)
Nothing
(Node (Leaf 1) (Leaf 2))
```

### 型パラメータ

```lisp
;; リンクリスト
(data MyList
  (Cons head tail)
  Nil)

(Cons 1 (Cons 2 (Cons 3 Nil)))
```

## パターンマッチ: `match`

データ型の構造に基づいて分岐します。

```lisp
(def safe-div (fn (a b)
  (if (= b 0)
      Nothing
      (Just (/ a b)))))

(def show-result (fn (result)
  (match result
    ((Just x) (print x))
    (Nothing  (print "Error")))))

(show-result (safe-div 10 2))   ; 出力: 5
(show-result (safe-div 10 0))   ; 出力: Error
```

### ワイルドカードパターン

`_` は任意の値にマッチし、値を束縛しません。

```lisp
(match value
  ((Just _) "has value")
  (Nothing  "empty"))
```

### リテラルパターン

数値や文字列などのリテラルにマッチします。

```lisp
(match x
  (0 "zero")
  (1 "one")
  (_ "other"))
```

## リストのパターンマッチ

`cons` パターンでリストを分解できます。

```lisp
(def sum-list (fn (lst)
  (match lst
    (nil 0)
    ((cons x xs) (+ x (sum-list xs))))))

(sum-list '(1 2 3 4 5))  ; => 15
```

### 複数要素のマッチ

```lisp
(def first-two (fn (lst)
  (match lst
    ((cons a (cons b _)) (list a b))
    (_                   nil))))

(first-two '(1 2 3 4))  ; => (1 2)
(first-two '(1))        ; => nil
```

## ネストしたパターン

パターンは任意の深さでネストできます。

```lisp
(def tree-sum (fn (tree)
  (match tree
    ((Leaf n)     n)
    ((Node l r)   (+ (tree-sum l) (tree-sum r))))))

(def my-tree (Node (Leaf 1) (Node (Leaf 2) (Leaf 3))))
(tree-sum my-tree)  ; => 6
```

## Symbols

| Type | Name | Description |
|:-----|:-----|:------------|
| Special Form | [data](ref/data) | 代数的データ型を定義 |
| Special Form | [match](ref/match) | パターンマッチによる分岐 |
| Function | [list](ref/list) | リストを作成 |
| Function | [cons](ref/cons) | 要素とリストを結合 (パターンでも使用) |
| Function | [car](ref/car) | リストの先頭要素を取得 |
| Function | [cdr](ref/cdr) | リストの残りを取得 |
| Function | [null?](ref/null-p) | `nil` かどうかを判定 |
