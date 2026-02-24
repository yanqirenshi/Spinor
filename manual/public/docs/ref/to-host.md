# to-host
**Kind:** Function  
**Signature:** `(CLContext, CLBuffer, Int, Int) -> Matrix`
### Syntax:
```lisp
(to-host ctx buffer rows cols)
```
### Arguments and Values:
- `ctx` -- OpenCL コンテキスト
- `buffer` -- GPU バッファ (`CLBuffer`)
- `rows` -- 行数
- `cols` -- 列数
- 戻り値: CPU 側の行列 (`Matrix`)
### Description:
GPU バッファのデータを CPU に読み戻し、指定された次元の行列を生成します。
### Examples:
```lisp
(def ctx (cl-init))
(def m (matrix 2 2 '(1 2 3 4)))
(def buf (to-device ctx m))
(to-host ctx buf 2 2)  ; => #m((1.0 2.0) (3.0 4.0))
```
### Side Effects:
GPU からデータを読み戻します。
### Exceptional Situations:
- バッファサイズと指定された次元 (`rows * cols`) が不一致の場合、エラーを返します。
- 第1引数が CLContext でない場合、エラーを返します。
- 第2引数が CLBuffer でない場合、エラーを返します。
### See Also:
[to-device](to-device), [cl-init](cl-init), [matrix](matrix)