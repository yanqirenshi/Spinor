# if
**Kind:** Special Form  
**Signature:** `(Bool, Then, Else) -> Val`
### Syntax:
```lisp
(if condition then-expr else-expr)
```
### Arguments and Values:
- `condition` -- 条件式
- `then-expr` -- 条件が真の場合に評価される式
- `else-expr` -- 条件が偽の場合に評価される式
- 戻り値: 評価された分岐の結果
### Description:
条件分岐を行います。`condition` が真 (nil 以外) なら `then-expr` を、偽 (nil) なら `else-expr` を評価します。
### Examples:
```lisp
(if (> 5 3) "yes" "no")  ; => "yes"

(def abs (fn (n)
  (if (< n 0) (- 0 n) n)))
(abs -5)  ; => 5
```
### See Also:
[match](match), [begin](begin)
### Notes:
Spinor では `nil` のみが偽として扱われます。`0` や空リストは真です。