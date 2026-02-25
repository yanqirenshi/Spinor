# cl-enqueue
**Kind:** Function  
**Signature:** `(CLContext, CLKernel, [Int], [Int], Args...) -> Bool`
### Syntax:
```lisp
(cl-enqueue ctx kernel global-work-size local-work-size arg1 arg2 ...)
```
### Arguments and Values:
- `ctx` -- OpenCL コンテキスト (`CLContext`)
- `kernel` -- コンパイル済みカーネル (`CLKernel`)
- `global-work-size` -- 全スレッド数のリスト (例: `'(1024)`)。1D/2D/3D に対応。
- `local-work-size` -- ワークグループサイズのリスト (例: `'(64)`)。空リスト `'()` の場合はドライバ任せ。
- `argN` -- カーネルに渡す引数 (可変長):
    - `CLBuffer` → GPU メモリポインタとして渡される
    - `Int` → `cl_int` (4バイト) のスカラー値として渡される
    - `Float` → `cl_double` (8バイト) のスカラー値として渡される
- 戻り値: 成功時に `#t`
### Description:
コンパイル済みカーネルに引数を設定し、GPU 上で実行します。完了まで同期的に待機します。
### Examples:
```lisp
;; ベクトル加算の例
(def ctx (cl-init))
(def m1 (matrix 1 1024 (list-of-range 1024)))
(def m2 (matrix 1 1024 (list-of-range 1024)))
(def b1 (to-device ctx m1))
(def b2 (to-device ctx m2))
(def b3 (to-device ctx m1))  ; 出力用バッファ

(def src "__kernel void add(__global double* a, __global double* b, __global double* c) {
    int i = get_global_id(0);
    c[i] = a[i] + b[i];
}")
(def knl (cl-compile ctx src "add"))

(cl-enqueue ctx knl '(1024) '() b1 b2 b3)  ; => #t
(def res (to-host ctx b3 1 1024))
```
### Side Effects:
GPU 上でカーネルを実行し、完了を待機します。
### Exceptional Situations:
- 第1引数が CLContext でない場合、エラーを返します。
- 第2引数が CLKernel でない場合、エラーを返します。
- global-work-size が空の場合、エラーを返します。
- カーネル引数の型が不正 (CLBuffer/Int/Float 以外) の場合、エラーを返します。
### See Also:
[cl-init](ref/cl-init), [cl-compile](ref/cl-compile), [to-device](ref/to-device), [to-host](ref/to-host)
### Notes:
現在はブロッキング実行のみ対応しています。