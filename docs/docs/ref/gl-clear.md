# gl-clear
**Kind:** Function  
**Signature:** `() -> Nil`
### Syntax:
```lisp
(gl-clear)
```
### Arguments and Values:
- 引数なし
- 戻り値: `nil`
### Description:
カラーバッファをクリアします (デフォルトは黒)。
### Examples:
```lisp
(gl-clear)  ; 画面を黒でクリア
```
### Side Effects:
OpenGL のカラーバッファをクリアします。
### See Also:
[gl-draw-points](gl-draw-points), [gl-swap-buffers](gl-swap-buffers)