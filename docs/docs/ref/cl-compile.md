# cl-compile
**Kind:** Function  
**Signature:** `(CLContext, String, String) -> CLKernel`
### Syntax:
```lisp
(cl-compile ctx source kernel-name)
```
### Arguments and Values:
- `ctx` -- OpenCL コンテキスト
- `source` -- OpenCL C ソースコード (文字列)
- `kernel-name` -- エントリポイントとなるカーネル関数名 (文字列)
- 戻り値: コンパイル済みカーネル (`CLKernel`)
### Description:
OpenCL C のソースコードをコンパイルし、指定されたカーネル関数へのハンドルを取得します。
### Examples:
```lisp
(def ctx (cl-init))
(def k (cl-compile ctx
  "__kernel void add(__global double* a, __global double* b, __global double* c, int n) { int i = get_global_id(0); if (i < n) c[i] = a[i] + b[i]; }"
  "add"))
k  ; => <CLKernel:add>
```
### Side Effects:
OpenCL プログラムのコンパイルを行います。
### Exceptional Situations:
- ソースコードのコンパイルに失敗した場合、ビルドログ付きのエラーを返します。
- 指定されたカーネル名が見つからない場合、エラーを返します。
- 第1引数が CLContext でない場合、エラーを返します。
### See Also:
[cl-init](cl-init), [to-device](to-device), [to-host](to-host), [cl-enqueue](cl-enqueue)
### Notes:
コンパイルエラー時はビルドログが含まれます。