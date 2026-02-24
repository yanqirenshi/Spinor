# gl-init
**Kind:** Function  
**Signature:** `(Int, Int, String) -> Window`
### Syntax:
```lisp
(gl-init width height title)
```
### Arguments and Values:
- `width` -- ウィンドウの幅 (ピクセル)
- `height` -- ウィンドウの高さ (ピクセル)
- `title` -- ウィンドウのタイトル (文字列)
- 戻り値: GLFW ウィンドウハンドル (`Window`)
### Description:
GLFW を初期化し、指定されたサイズとタイトルでウィンドウを作成します。
### Examples:
```lisp
(def win (gl-init 640 480 "My Window"))
win  ; => <Window>
```
### Side Effects:
GLFW を初期化し、OpenGL コンテキスト付きのウィンドウを作成します。
### Affected By:
ディスプレイ環境に依存します。
### Exceptional Situations:
- GLFW の初期化に失敗した場合、エラーを返します。
- ウィンドウの作成に失敗した場合、エラーを返します。
### See Also:
[gl-window-should-close](gl-window-should-close), [gl-swap-buffers](gl-swap-buffers), [gl-clear](gl-clear), [gl-draw-points](gl-draw-points)