# Iteration

Spinor における反復処理について解説します。

## 概要

Spinor では、反復処理は主に再帰関数によって実現されます。関数型プログラミングのスタイルに従い、ループ構文よりも再帰が推奨されます。

## 再帰による反復

### 基本的な再帰

```lisp
;; 階乗の計算
(def factorial (fn (n)
  (if (= n 0)
      1
      (* n (factorial (- n 1))))))

(factorial 5)  ; => 120
```

### リストの処理

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

(sum '(1 2 3 4 5))  ; => 15
```

## 末尾再帰最適化 (TCO)

末尾再帰とは、再帰呼び出しが関数の最後の操作である場合を指します。Spinor は末尾再帰を最適化し、スタックオーバーフローを防ぎます。

### 末尾再帰の例

```lisp
;; 末尾再帰版の階乗
(def factorial-tail (fn (n acc)
  (if (= n 0)
      acc
      (factorial-tail (- n 1) (* n acc)))))

(def factorial (fn (n)
  (factorial-tail n 1)))

(factorial 1000)  ; 大きな数でもスタックオーバーフローしない
```

### アキュムレータパターン

末尾再帰を実現するために、計算途中の結果を「アキュムレータ」として引数で渡します。

```lisp
;; 末尾再帰版のリスト合計
(def sum-tail (fn (lst acc)
  (match lst
    (nil acc)
    ((cons x xs) (sum-tail xs (+ acc x))))))

(def sum (fn (lst)
  (sum-tail lst 0)))

;; 末尾再帰版のリスト反転
(def reverse-tail (fn (lst acc)
  (match lst
    (nil acc)
    ((cons x xs) (reverse-tail xs (cons x acc))))))

(def reverse (fn (lst)
  (reverse-tail lst nil)))

(reverse '(1 2 3))  ; => (3 2 1)
```

## 高階関数による反復

### map

リストの各要素に関数を適用します。

```lisp
(def map (fn (f lst)
  (match lst
    (nil nil)
    ((cons x xs) (cons (f x) (map f xs))))))

(map (fn (x) (* x 2)) '(1 2 3))  ; => (2 4 6)
```

### filter

条件を満たす要素だけを抽出します。

```lisp
(def filter (fn (pred lst)
  (match lst
    (nil nil)
    ((cons x xs)
      (if (pred x)
          (cons x (filter pred xs))
          (filter pred xs))))))

(filter (fn (x) (> x 2)) '(1 2 3 4 5))  ; => (3 4 5)
```

### fold (reduce)

リストを畳み込んで単一の値にします。

```lisp
(def fold (fn (f init lst)
  (match lst
    (nil init)
    ((cons x xs) (fold f (f init x) xs)))))

(fold + 0 '(1 2 3 4 5))  ; => 15
(fold * 1 '(1 2 3 4 5))  ; => 120
```

## 将来の拡張

今後のバージョンでは、以下のマクロの追加が計画されています：

- `dotimes` - 指定回数の反復
- `dolist` - リスト要素の反復

```lisp
;; 計画中の構文
(dotimes (i 5)
  (print i))

(dolist (x '(a b c))
  (print x))
```

## Symbols

| Type | Name | Description |
|:-----|:-----|:------------|
| Special Form | [fn](ref/fn) | 再帰関数を定義 |
| Special Form | [match](ref/match) | パターンマッチで場合分け |
| Special Form | [if](ref/if) | 条件分岐 |
| Special Form | [let](ref/let) | ローカル変数の束縛 |
