# m*
**Kind:** Function  
**Signature:** `(Matrix, Matrix) -> Matrix`
### Syntax:
```lisp
(m* a b)
```
### Arguments and Values:
- `a` -- 左行列 (m×k)
- `b` -- 右行列 (k×n)
- 戻り値: 行列積 (m×n)
### Description:
行列積を計算します。BLAS を利用した高速演算です。
### Examples:
```lisp
(def a (matrix 2 3 '(1 2 3 4 5 6)))
(def b (matrix 3 2 '(7 8 9 10 11 12)))
(m* a b)  ; => #m((58.0 64.0) (139.0 154.0))
```
### Exceptional Situations:
- 引数が行列でない場合、エラーを返します。
- 左行列の列数と右行列の行数が一致しない場合、エラーを返します。
### See Also:
[m+](m+), [transpose](transpose), [inverse](inverse)
### Notes:
内部的に hmatrix (BLAS/dgemm) を使用しています。