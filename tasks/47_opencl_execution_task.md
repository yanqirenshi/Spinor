# Task 47: OpenCL Execution Pipeline (GPGPU 実行レイヤー) の実装

GPU カーネルの実行プリミティブを実装し、GPGPU 計算パイプラインを完成させてください。

## ステップ

### 1. FFI の拡張 (OpenCL/Raw.hs)
- `src/Spinor/OpenCL/Raw.hs` に以下の FFI 定義とラッパー関数を追加してください。
    - `clSetKernelArg`: `raw_clSetKernelArg` と、型安全な `clSetKernelArg` (バッファ用、整数用、浮動小数点用などのオーバーロードを考慮したヘルパー)。
    - `clEnqueueNDRangeKernel`: `raw_clEnqueueNDRangeKernel` と、リストから `CSize` 配列を生成して呼び出す `clEnqueueNDRangeKernel` ラッパー。

### 2. プリミティブの実装 (GPGPU.hs)
- `src/Spinor/GPGPU.hs` に `primCLEnqueue` を実装してください。
- 可変長引数 (`arg1 arg2 ...`) をループで処理し、インデックス 0 から順に `clSetKernelArg` を呼び出すロジックを実装してください。
- `VCLBuffer` の場合は `CLMem` ポインタ自体へのポインタを渡す必要がある点に注意してください。
- `gpgpuBindings` に `"cl-enqueue"` を追加してください。

### 3. ドキュメントの追加 (Docs.hs)
- `src/Spinor/Lsp/Docs.hs` に `cl-enqueue` のドキュメントを追加してください。
- `Arguments and Values` セクションで、`global-work-size` のリスト形式について明記してください。

### 4. 検証 (Vector Addition)
REPL を使用して、以下の手順でベクトル加算が GPU で行えるか手動検証してください。

```lisp
; 1. 初期化
(def ctx (cl-init))

; 2. データの準備 (1024要素の行列)
(def m1 (matrix 1 1024 (list-of-range 1024)))
(def m2 (matrix 1 1024 (list-of-range 1024)))
(def b1 (to-device ctx m1))
(def b2 (to-device ctx m2))
(def b3 (to-device ctx m1)) ; 出力用バッファ

; 3. カーネルのコンパイル
(def src "__kernel void add(__global double* a, __global double* b, __global double* c) {
    int i = get_global_id(0);
    c[i] = a[i] + b[i];
}")
(def knl (cl-compile ctx src "add"))

; 4. 実行
(cl-enqueue ctx knl '(1024) '() b1 b2 b3)

; 5. 結果の確認
(def res (to-host ctx b3 1 1024))
; res の各要素が (* i 2) になっていれば成功
```

## 実装報告ルール
実装完了後、**このファイル自体を編集して**、以下のセクションを末尾に追記してください。

### 実装方針
(FFI でのポインタ操作の工夫や、可変引数の処理方法について)

### 実装内容
(変更したファイルの一覧、追加した主要な関数の説明など)
