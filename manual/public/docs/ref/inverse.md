# inverse
**Kind:** Function  
**Signature:** `(Matrix) -> Matrix`
### Syntax:
```lisp
(inverse m)
```
### Arguments and Values:
- `m` -- 正方行列 (n×n)
- 戻り値: 逆行列 (n×n)
### Description:
正方行列の逆行列を計算します。LAPACK を利用した高速演算です。
### Examples:
```lisp
;; 単位行列の逆行列は単位行列
(inverse (matrix 2 2 '(1 0 0 1)))  ; => #m((1.0 0.0) (0.0 1.0))

(inverse (matrix 2 2 '(1 2 3 4)))
; => #m((-2.0 1.0) (1.5 -0.5))
```
### Exceptional Situations:
- 引数が行列でない場合、エラーを返します。
- 行列が正方でない場合、エラーを返します。
- 行列が特異 (singular) な場合、エラーを返します。
### See Also:
[m*](m*), [transpose](transpose), [matrix](matrix)
### Notes:
内部的に hmatrix (LAPACK) を使用しています。