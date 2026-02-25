# matrix
**Kind:** Function  
**Signature:** `(Int, Int, [Number]) -> Matrix`
### Syntax:
```lisp
(matrix rows cols elements)
```
### Arguments and Values:
- `rows` -- 行数 (正の整数)
- `cols` -- 列数 (正の整数)
- `elements` -- 要素のリスト (`Int` または `Float`)
- 戻り値: `rows × cols` の行列
### Description:
行列を生成します。行数・列数と要素のフラットリストから行優先 (row-major) で構築します。
### Examples:
```lisp
;; 2x3 行列を生成
(matrix 2 3 '(1 2 3 4 5 6))
; => #m((1.0 2.0 3.0) (4.0 5.0 6.0))

;; 単位行列
(matrix 2 2 '(1 0 0 1))
; => #m((1.0 0.0) (0.0 1.0))
```
### Exceptional Situations:
- `rows * cols` と要素数が一致しない場合、エラーを返します。
- 要素に数値以外が含まれる場合、エラーを返します。
### See Also:
[mdim](ref/mdim), [mref](ref/mref)
### Notes:
内部的には `Data.Vector.Storable` を使用し、将来的な BLAS/LAPACK 連携を想定しています。