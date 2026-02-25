# gl-draw-points
**Kind:** Function  
**Signature:** `(Matrix) -> Nil`
### Syntax:
```lisp
(gl-draw-points matrix)
```
### Arguments and Values:
- `matrix` -- 頂点座標の行列 (Nx2 または Nx3)
    - Nx2: 各行が `(x y)` の 2D 座標
    - Nx3: 各行が `(x y z)` の 3D 座標
- 戻り値: `nil`
### Description:
Nx2 または Nx3 の行列を受け取り、各行を頂点座標として画面上に点 (GL_POINTS) を描画します。
### Examples:
```lisp
;; 3点を描画
(def pts (matrix 3 2 '(0.0 0.0  0.5 0.5  -0.5 -0.5)))
(gl-draw-points pts)
```
### Side Effects:
OpenGL の固定機能パイプラインで点を描画します。
### Exceptional Situations:
- 引数が Matrix でない場合、エラーを返します。
- 列数が 2 または 3 でない場合、エラーを返します。
### See Also:
[gl-clear](ref/gl-clear), [gl-swap-buffers](ref/gl-swap-buffers), [matrix](ref/matrix)
### Notes:
座標系は OpenGL 標準の正規化デバイス座標系 (-1.0 ～ 1.0) です。