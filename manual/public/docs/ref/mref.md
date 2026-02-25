# mref
**Kind:** Function  
**Signature:** `(Matrix, Int, Int) -> Float`
### Syntax:
```lisp
(mref m row col)
```
### Arguments and Values:
- `m` -- 行列
- `row` -- 行インデックス (0-indexed)
- `col` -- 列インデックス (0-indexed)
- 戻り値: 指定位置の要素 (浮動小数点数)
### Description:
行列の指定位置の要素を取得します。インデックスは 0-indexed です。
### Examples:
```lisp
(def m (matrix 2 3 '(1 2 3 4 5 6)))
(mref m 0 0)  ; => 1.0  ; 左上
(mref m 0 2)  ; => 3.0  ; 1行目の最後
(mref m 1 1)  ; => 5.0  ; 2行目の中央
```
### Exceptional Situations:
インデックスが範囲外の場合、エラーを返します。
### See Also:
[matrix](ref/matrix), [mdim](ref/mdim)