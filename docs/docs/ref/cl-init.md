# cl-init
**Kind:** Function  
**Signature:** `() -> CLContext`
### Syntax:
```lisp
(cl-init)
```
### Arguments and Values:
- 引数なし
- 戻り値: OpenCL コンテキスト (`CLContext`)
### Description:
OpenCL プラットフォームとデバイスをスキャンし、コンテキストとコマンドキューを初期化します。GPU を優先し、なければ CPU にフォールバックします。
### Examples:
```lisp
(def ctx (cl-init))
ctx  ; => <CLContext>
```
### Side Effects:
OpenCL プラットフォームの初期化を行います。
### Affected By:
システムにインストールされた OpenCL ドライバとデバイスに依存します。
### Exceptional Situations:
- OpenCL プラットフォームが見つからない場合、エラーを返します。
- 利用可能なデバイス (GPU/CPU) が見つからない場合、エラーを返します。
### See Also:
[to-device](ref/to-device), [to-host](ref/to-host), [cl-compile](ref/cl-compile)
### Notes:
Haskell の OpenCL パッケージ (`Control.Parallel.OpenCL`) を使用しています。