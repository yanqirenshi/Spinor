# gl-window-should-close
**Kind:** Function  
**Signature:** `(Window) -> Bool`
### Syntax:
```lisp
(gl-window-should-close win)
```
### Arguments and Values:
- `win` -- GLFW ウィンドウ
- 戻り値: 閉じるべきなら `#t`、そうでなければ `#f`
### Description:
ウィンドウの閉じるボタンが押されたか、または終了フラグがセットされているかを確認します。
### Examples:
```lisp
(if (not (gl-window-should-close win))
    (begin (gl-clear) (gl-swap-buffers win))
    (print "done"))
```
### Affected By:
ユーザーのウィンドウ操作 (閉じるボタン) に依存します。
### Exceptional Situations:
引数が Window でない場合、エラーを返します。
### See Also:
[gl-init](ref/gl-init), [gl-swap-buffers](ref/gl-swap-buffers)