# let
**Kind:** Special Form  
**Signature:** `(Bindings, Body) -> Val`
### Syntax:
```lisp
(let ((var1 val1) (var2 val2) ...) body)
```
### Arguments and Values:
- `bindings` -- `(変数名 値)` のペアのリスト
- `body` -- 束縛のスコープ内で評価される式
- 戻り値: body の評価結果
### Description:
ローカル変数を導入し、その環境で `body` を評価します。束縛は逐次的に行われます (let*)。
### Examples:
```lisp
(let ((x 10)
      (y 20))
  (+ x y))  ; => 30

;; 逐次束縛 (前の変数を参照可能)
(let ((x 5)
      (y (* x 2)))
  y)  ; => 10
```
### See Also:
[def](def), [fn](fn)
### Notes:
Spinor の `let` は Common Lisp の `let*` と同じ意味論です。