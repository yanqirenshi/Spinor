# mdim
**Kind:** Function  
**Signature:** `(Matrix) -> (Int Int)`
### Syntax:
```lisp
(mdim m)
```
### Arguments and Values:
- `m` -- 行列
- 戻り値: `(rows cols)` の形式のリスト
### Description:
行列の次元 (行数, 列数) をリストとして返します。
### Examples:
```lisp
(def m (matrix 3 4 '(1 2 3 4 5 6 7 8 9 10 11 12)))
(mdim m)  ; => (3 4)
```
### Exceptional Situations:
引数が行列でない場合、エラーを返します。
### See Also:
[matrix](ref/matrix), [mref](ref/mref)