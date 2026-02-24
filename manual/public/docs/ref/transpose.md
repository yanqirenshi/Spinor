# transpose
**Kind:** Function  
**Signature:** `(Matrix) -> Matrix`
### Syntax:
```lisp
(transpose m)
```
### Arguments and Values:
- `m` -- 行列 (m×n)
- 戻り値: 転置行列 (n×m)
### Description:
行列の転置を返します。
### Examples:
```lisp
(def m (matrix 2 3 '(1 2 3 4 5 6)))
(transpose m)  ; => #m((1.0 4.0) (2.0 5.0) (3.0 6.0))
```
### Exceptional Situations:
引数が行列でない場合、エラーを返します。
### See Also:
[m*](m*), [inverse](inverse), [matrix](matrix)