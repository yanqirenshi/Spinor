# Control Flow

制御構造について解説します。

## 条件分岐: `if`

```lisp
(if condition
    then-expr
    else-expr)

;; 例
(if (> x 0)
    "positive"
    "non-positive")
```

**注意:** Spinor では `nil` のみが偽です。`0` や空文字列も真として扱われます。

```lisp
(if 0 "truthy" "falsy")        ; => "truthy"
(if "" "truthy" "falsy")       ; => "truthy"
(if nil "truthy" "falsy")      ; => "falsy"
```

## ローカル変数: `let`

```lisp
(let ((var1 val1)
      (var2 val2))
  body)

;; 例
(let ((x 10)
      (y 20))
  (+ x y))  ; => 30
```

束縛は逐次的に行われます (Common Lisp の `let*` と同様)。

```lisp
(let ((x 5)
      (y (* x 2)))   ; x を参照可能
  y)  ; => 10
```

### ネストした let

```lisp
(let ((x 1))
  (let ((y 2))
    (+ x y)))  ; => 3
```

## 逐次実行: `begin` / `progn`

複数の式を順に実行し、最後の値を返します。

```lisp
(begin
  (print "Step 1")
  (print "Step 2")
  42)  ; => 42
```

`progn` は `begin` のエイリアスです (Common Lisp スタイル)。

```lisp
(progn
  (setq x 1)
  (setq x (+ x 1))
  x)  ; => 2
```

## 代入: `setq`

既存の変数の値を変更します。

```lisp
(def x 1)
(setq x 2)
x  ; => 2
```

### 注意点

- 未定義の変数に `setq` するとエラーになります
- `let` で束縛された変数も `setq` で変更可能です

```lisp
(let ((counter 0))
  (begin
    (setq counter (+ counter 1))
    (setq counter (+ counter 1))
    counter))  ; => 2
```

