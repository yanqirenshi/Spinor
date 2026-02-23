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

**可変長カーネル引数の処理:**
- `primCLEnqueue` は固定 4 引数 (ctx, kernel, global-work-size, local-work-size) + 可変長カーネル引数を受け取る
- パターンマッチで `(VCLContext ... : VCLKernel ... : gwsVal : lwsVal : kernelArgs)` と分割し、`kernelArgs` を再帰関数 `setKernelArgs` でインデックス 0 から順に処理

**FFI でのポインタ操作:**
- `VCLBuffer (mem :: Ptr ())` の場合: `Foreign.Marshal.Utils.with` で一時メモリに `mem` を格納し、その `Ptr (Ptr ())` を `castPtr` して `clSetKernelArg` に渡す。サイズは `sizeOf (undefined :: Ptr ())` = 8 バイト (64-bit)
- `VInt n` の場合: `CInt` に変換して `with` で渡す。サイズは `sizeOf (undefined :: CInt)` = 4 バイト
- `VFloat f` の場合: `CDouble` に変換して `with` で渡す。サイズは `sizeOf (undefined :: CDouble)` = 8 バイト

**ワークサイズのリスト変換:**
- Lisp のリスト (`VList [VInt ...]` または `VNil`) を `[CSize]` に変換するヘルパー `valListToSizes` を実装
- `global-work-size` のリスト長が `work_dim` (1D/2D/3D) となる
- `local-work-size` が空リスト (`VNil` / `'()`) の場合は `nullPtr` を渡してドライバ任せにする (Raw.hs のラッパーで対応済み)

**同期実行:**
- `clEnqueueNDRangeKernel` 後に `clFinish` を呼び出し、カーネル実行の完了を待機するブロッキング方式

### 実装内容

**変更ファイル一覧:**

1. **src/Spinor/OpenCL/Raw.hs**
   - `clSetKernelArg` の FFI 宣言 (`raw_clSetKernelArg`) とラッパー関数を追加
   - `clEnqueueNDRangeKernel` の FFI 宣言 (`raw_clEnqueueNDRangeKernel`) とラッパー関数を追加
   - ラッパーは `[CSize]` リストを `withArray` で C 配列に変換し、`local-work-size` が空の場合は `nullPtr` を渡す
   - エクスポートリストに `clSetKernelArg`, `clEnqueueNDRangeKernel` を追加

2. **src/Spinor/GPGPU.hs**
   - `primCLEnqueue` (`cl-enqueue`): カーネル実行プリミティブの実装
   - `setKernelArgs`: カーネル引数リストを再帰的に処理する IO 関数
   - `setOneKernelArg`: 値の型に応じて `clSetKernelArg` を呼び出すディスパッチ関数
   - `valListToSizes`: Lisp リスト → `[CSize]` 変換ヘルパー
   - `gpgpuBindings` に `"cl-enqueue"` を追加
   - インポート追加: `Foreign.C.Types`, `Foreign.Marshal.Utils`, `Foreign.Ptr`, `showVal`

3. **src/Spinor/Lsp/Docs.hs**
   - `cl-enqueue` の CLHS 形式ドキュメントを追加
   - 引数の型マッピング (CLBuffer/Int/Float) とワークサイズのリスト形式を明記
   - `cl-compile` の `docSeeAlso` に `cl-enqueue` を追加

4. **src/Spinor/Infer.hs**
   - `baseTypeEnv` に行列・BLAS・OpenCL 関連プリミティブの型シグネチャを追加
   - REPL の型推論ステップで「未定義のシンボル」エラーとなっていた問題を解消
   - 文字列操作 (`string-append` 等)、等価性 (`eq`, `equal`)、行列 (`matrix`, `mdim`, `mref`)、BLAS (`m+`, `m*`, `transpose`, `inverse`)、OpenCL (`cl-init`, `to-device`, `to-host`, `cl-compile`, `cl-enqueue`) の型を登録

**動作確認 (WSL2 環境):**
- `cabal build`: 警告なしでビルド成功
- `cabal test`: 全 152 テストパス (0 failures)
- OpenCL ランタイムテスト: `pocl-opencl-icd` (CPU OpenCL) を使用し、REPL 上でベクトル加算の End-to-End 検証に成功

**REPL 検証ログ:**
```
spinor> (def ctx (cl-init))
:: CLContext
<CLContext>
spinor> (def m1 (matrix 1 4 '(1 2 3 4)))
:: Matrix
#m((1.0 2.0 3.0 4.0))
spinor> (def m2 (matrix 1 4 '(10 20 30 40)))
:: Matrix
#m((10.0 20.0 30.0 40.0))
spinor> (def b1 (to-device ctx m1))
:: CLBuffer
<CLBuffer:size=4>
spinor> (def b2 (to-device ctx m2))
:: CLBuffer
<CLBuffer:size=4>
spinor> (def b3 (to-device ctx m1))
:: CLBuffer
<CLBuffer:size=4>
spinor> (def knl (cl-compile ctx "__kernel void add(__global double* a, __global double* b, __global double* c) { int i = get_global_id(0); c[i] = a[i] + b[i]; }" "add"))
:: CLKernel
<CLKernel:add>
spinor> (cl-enqueue ctx knl '(4) '() b1 b2 b3)
:: t1
#t
spinor> (to-host ctx b3 1 4)
:: Matrix
#m((11.0 22.0 33.0 44.0))
```
各要素が `(1+10)=11, (2+20)=22, (3+30)=33, (4+40)=44` と正しく加算されている。
