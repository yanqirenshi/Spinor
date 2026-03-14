# gl-swap-buffers
**Kind:** Function  
**Signature:** `(Window) -> Nil`
### Syntax:
```lisp
(gl-swap-buffers win)
```
### Arguments and Values:
- `win` -- GLFW ウィンドウ
- 戻り値: `nil`
### Description:
フロントバッファとバックバッファを入れ替え、描画内容を画面に反映させます。同時に GLFW のイベントをポーリングします。
### Examples:
```lisp
(gl-clear)
(gl-draw-points pts)
(gl-swap-buffers win)
```
### Side Effects:
バッファスワップとイベントポーリングを行います。
### Exceptional Situations:
引数が Window でない場合、エラーを返します。
### See Also:
[gl-init](gl-init), [gl-clear](gl-clear), [gl-draw-points](gl-draw-points)