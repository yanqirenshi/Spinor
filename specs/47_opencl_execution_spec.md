# Spec 47: OpenCL Execution Pipeline (GPGPU 実行レイヤー)

## 概要
コンパイル済みの OpenCL カーネルに対して引数をセットし、GPU デバイスのコマンドキューに投入して実行する機能を実装する。これにより、Lisp から直接 GPU カーネルをキックすることが可能になる。

## アーキテクチャ
`src/Spinor/OpenCL/Raw.hs` に、カーネル操作と実行に関する OpenCL C API のバインディングを追加する。

### 追加する FFI 関数
- `clSetKernelArg`: カーネルの特定の引数インデックスに値をバインドする。
- `clEnqueueNDRangeKernel`: カーネルをコマンドキューに投入し、指定された次元 (1D, 2D, 3D) で実行する。

## 新規プリミティブ仕様

### `(cl-enqueue ctx kernel global-work-size local-work-size arg1 arg2 ...)`
- **説明:** カーネルに引数を設定し、実行キューに投入する。完了まで同期的に待機する。
- **引数:**
    - `ctx`: `VCLContext`
    - `kernel`: `VCLKernel`
    - `global-work-size`: 整数のリスト。全スレッド数 (例: `'(1024)`兄。
    - `local-work-size`: 整数のリスト。ワークグループサイズ (例: `'(64)`)。空リスト `'()` の場合はドライバ任せ。
    - `argN`: カーネルに渡す引数。
        - `VCLBuffer`: GPU メモリポインタとして渡される。
        - `VInt`: `int` 型のスカラー値として渡される。
        - `VFloat`: `double` 型のスカラー値として渡される。
- **戻り値:** 成功時に `#t` (VBool True)。

### 動作フロー
1. 各 `argN` をその型に応じたサイズで `clSetKernelArg` に渡す。
2. `global-work-size` のリスト長を `work_dim` とし、値を `CSize` 配列として準備する。
3. `clEnqueueNDRangeKernel` を発行する。
4. `clFinish` を呼び出し、計算の完了を待機する。

## 考慮事項
- **引数の型マッピング:** 
    - `VCLBuffer` -> `cl_mem` (ポインタのサイズ)
    - `VInt` -> `cl_int` (4バイト)。
    - `VFloat` -> `cl_double` (8バイト)。
- **同期実行:** 簡単のため、まずはブロッキング呼び出しとする。
