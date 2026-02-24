# m+
**Kind:** Function  
**Signature:** `(Matrix, Matrix) -> Matrix`
### Syntax:
```lisp
(m+ a b)
```
### Arguments and Values:
- `a` -- 第一行列
- `b` -- 第二行列
- 戻り値: 要素ごとの和を持つ新しい行列
### Description:
2つの行列の要素ごとの加算を行います。BLAS を利用した高速演算です。
### Examples:
```lisp
(def a (matrix 2 2 '(1 2 3 4)))
(def b (matrix 2 2 '(5 6 7 8)))
(m+ a b)  ; => #m((6.0 8.0) (10.0 12.0))
```
### Exceptional Situations:
- 引数が行列でない場合、エラーを返します。
- 2つの行列の次元が一致しない場合、エラーを返します。
### See Also:
[m*](m*), [matrix](matrix)
### Notes:
内部的に hmatrix (BLAS) を使用しています。