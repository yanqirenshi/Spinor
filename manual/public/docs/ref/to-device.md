# to-device
**Kind:** Function  
**Signature:** `(CLContext, Matrix) -> CLBuffer`
### Syntax:
```lisp
(to-device ctx matrix)
```
### Arguments and Values:
- `ctx` -- OpenCL コンテキスト
- `matrix` -- CPU 側の行列データ (`VMatrix`)
- 戻り値: GPU バッファ (`CLBuffer`)
### Description:
行列データを GPU 側のバッファに転送します。
### Examples:
```lisp
(def ctx (cl-init))
(def m (matrix 2 2 '(1 2 3 4)))
(def buf (to-device ctx m))  ; => <CLBuffer:size=4>
```
### Side Effects:
GPU メモリを確保し、データを転送します。
### Exceptional Situations:
- 第1引数が CLContext でない場合、エラーを返します。
- 第2引数が Matrix でない場合、エラーを返します。
### See Also:
[to-host](ref/to-host), [cl-init](ref/cl-init), [matrix](ref/matrix)