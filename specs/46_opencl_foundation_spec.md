# Spec 46: OpenCL Foundation (GPGPU 基盤とカーネル構文)

## 概要
Spinor に GPGPU 計算の基盤となる OpenCL 連携機能を導入する。
Haskell の `OpenCL` パッケージを利用して、GPU デバイスの初期化、メモリ転送 (Host <-> Device)、およびカーネルプログラムの動的コンパイルを可能にする。

## データモデル
`src/Spinor/Val.hs` の `Val` 型に、OpenCL オブジェクトを保持するための型を追加する。

```haskell
import qualified Control.Parallel.OpenCL as CL

data Val
  = ...
  | VCLContext CL.CLContext CL.CLCommandQueue -- コンテキストとコマンドキュー
  | VCLBuffer  CL.CLMem Int                   -- GPUメモリ(Mem)とサイズ(要素数)
  | VCLKernel  CL.CLKernel                    -- コンパイル済みカーネル
  | ...
```

## プリミティブ仕様

### 1. `(cl-init)`
- **説明:** システム内の OpenCL プラットフォームとデバイスをスキャンし、最初に見つかった GPU (なければ CPU) を使用してコンテキストとコマンドキューを初期化する。
- **戻り値:** `VCLContext`

### 2. `(to-device ctx matrix)`
- **引数:**
    - `ctx`: `VCLContext`
    - `matrix`: `VMatrix` (CPU 側の行列データ)
- **説明:** `VMatrix` のデータを GPU 側のバッファに転送する。
- **戻り値:** `VCLBuffer`

### 3. `(to-host ctx buffer rows cols)`
- **引数:**
    - `ctx`: `VCLContext`
    - `buffer`: `VCLBuffer` (GPU 側のバッファ)
    - `rows`, `cols`: 戻り値となる行列の次元
- **説明:** GPU 側のデータを CPU 側に読み戻し、指定された次元の `VMatrix` を生成する。
- **戻り値:** `VMatrix`

### 4. `(cl-compile ctx source kernel-name)`
- **引数:**
    - `ctx`: `VCLContext`
    - `source`: OpenCL C 言語のソースコード (String)
    - `kernel-name`: エントリポイントとなる関数名 (String)
- **説明:** ソースコードをその場でビルドし、特定のカーネル関数へのハンドルを取得する。
- **戻り値:** `VCLKernel`

## 考慮事項
- **リソース管理:** Haskell の GC が `VCLBuffer` 等を回収する際、OpenCL の `clReleaseMemObject` 等が適切に呼ばれるよう、`ForeignPtr` や `OpenCL` パッケージの自動管理機能に留意する。
- **エラーハンドリング:** OpenCL の実行時エラー（コンパイルエラー等）は、詳細なメッセージと共に Spinor の例外として報告する。
