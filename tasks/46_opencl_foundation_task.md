# Task 46: OpenCL Foundation (GPGPU 基盤とカーネル構文) の実装

GPU 計算を可能にする OpenCL 基盤を実装してください。

## ステップ

### 1. 依存関係の追加
- `spinor.cabal` の `library` セクションに `OpenCL` を追加してください。
- ※重要: 実行環境に OpenCL SDK (ICD Loader) がインストールされている必要があります。Windows の場合は `OpenCL.dll` が PATH に通っていることを確認してください。

### 2. データ型の追加 (Val.hs)
- `src/Spinor/Val.hs` を編集し、`Val` 型に `VCLContext`, `VCLBuffer`, `VCLKernel` を追加してください。
- `showVal` を修正し、これらを `"<CLContext>"`, `"<CLBuffer:size=N>"`, `"<CLKernel:name>"` のように表示するようにしてください。

### 3. OpenCL プリミティブの実装
- `src/Spinor/Primitive.hs` (または新規作成する `src/Spinor/GPGPU.hs`) に以下の機能を実装してください。
    - `primCLInit`: デバイスの選択、Context/Queue の作成。
    - `primToDevice`: `VS.Vector Double` から `CLMem` へのデータ転送。
    - `primToHost`: `CLMem` から `VS.Vector Double` への読み戻しと `VMatrix` 構築。
    - `primCLCompile`: `clBuildProgram` を実行し、`clCreateKernel` でカーネルを取得。コンパイルエラー時はビルドログを抽出してエラーメッセージに含めること。
- `primitiveBindings` に各プリミティブを登録してください。

### 4. ドキュメントの更新 (Docs.hs)
- `src/Spinor/Lsp/Docs.hs` に OpenCL 関連の 4 つの関数のドキュメントを追加してください。
- CLHS フォーマットに従い、特に `cl-compile` の引数やエラー条件を詳細に記述してください。

### 5. 手動検証
OpenCL はハードウェア依存のため、以下の REPL 操作で手動検証を行ってください。
1. `(def ctx (cl-init))` -> `<CLContext>` が返るか。
2. `(def m (matrix 2 2 '(1 2 3 4)))`
3. `(def buf (to-device ctx m))` -> `<CLBuffer:size=4>` が返るか。
4. `(def m2 (to-host ctx buf 2 2))` -> もとの行列と一致するか。
5. `(cl-compile ctx "__kernel void add..." "add")` -> `<CLKernel>` が返るか。

## 実装報告ルール
実装完了後、**このファイル自体を編集して**、以下のセクションを末尾に追記してください。

### 実装方針

**Hackage の `OpenCL` パッケージの不採用:**
- Hackage 上の `OpenCL` パッケージ (1.0.3.4) は GHC 9.6.6 と互換性がなく、`Control.Parallel.OpenCL.Memory` モジュールでコンパイルエラー (`Operator applied to too few arguments: !`) が発生する
- そのため、`extra-libraries: OpenCL` で OpenCL C ライブラリを直接リンクし、`Spinor.OpenCL.Raw` モジュールで必要な FFI バインディングを自前で実装する方針とした

**FFI バインディング設計:**
- `Spinor.OpenCL.Raw` モジュールに、OpenCL C API の最小限のバインディングを定義
- 低レベルの `foreign import ccall` と、エラーハンドリング込みの Haskell ラッパー関数を分離
- すべてのラッパー関数が `IO (Either Text ...)` を返す設計とし、OpenCL のエラーコードを Spinor のランタイムエラーに変換

**デバイス選択ロジック:**
- `cl-init` は最初のプラットフォームを使用
- GPU (`CL_DEVICE_TYPE_GPU`) を優先的に探索し、見つからなければ CPU (`CL_DEVICE_TYPE_CPU`) にフォールバック
- デバイスが見つからない場合は明確なエラーメッセージを返す

**コンパイルエラーのトラップ:**
- `cl-compile` で `clBuildProgram` が失敗した場合、`clGetProgramBuildInfo` (CL_PROGRAM_BUILD_LOG = 0x1183) でビルドログを取得
- 取得したビルドログをエラーメッセージに含めて返す

**IO と純粋関数の橋渡し:**
- プリミティブ関数は `[Val] -> Either Text Val` のシグネチャを持つため、`unsafePerformIO` で IO アクションをラップ
- `try` で例外をキャッチし、Left に変換

### 実装内容

**変更ファイル一覧:**

1. **spinor.cabal**
   - `extra-libraries: OpenCL` を追加 (直接 C ライブラリをリンク)
   - `Spinor.GPGPU` と `Spinor.OpenCL.Raw` を `exposed-modules` に追加

2. **src/Spinor/OpenCL/Raw.hs** (新規作成)
   - OpenCL C API への最小限の FFI バインディング
   - 型定義: `CLPlatformID`, `CLDeviceID`, `CLContext`, `CLCommandQueue`, `CLMem`, `CLProgram`, `CLKernel` (すべて `Ptr ()`)
   - ラッパー関数: `clGetPlatformIDs`, `clGetDeviceIDs`, `clCreateContext`, `clCreateCommandQueue`, `clCreateBuffer`, `clEnqueueWriteBuffer`, `clEnqueueReadBuffer`, `clFinish`, `clCreateProgramWithSource`, `clBuildProgram`, `clGetProgramBuildLog`, `clCreateKernel`, `clReleaseProgram`, `clGetContextDevices`

3. **src/Spinor/GPGPU.hs** (新規作成)
   - `gpgpuBindings`: OpenCL プリミティブの環境辞書
   - `primCLInit` (`cl-init`): プラットフォーム/デバイス探索、コンテキスト/キュー初期化
   - `primToDevice` (`to-device`): VMatrix → GPU バッファ転送
   - `primToHost` (`to-host`): GPU バッファ → VMatrix 読み戻し (サイズ検証付き)
   - `primCLCompile` (`cl-compile`): ソースコンパイル + カーネル取得 (ビルドログ付きエラー)

4. **src/Spinor/Val.hs**
   - `VCLContext (Ptr ()) (Ptr ())` 追加 (Context, CommandQueue)
   - `VCLBuffer (Ptr ()) Int` 追加 (Mem, 要素数)
   - `VCLKernel (Ptr ()) Text` 追加 (Kernel, カーネル名)
   - `showVal`, `Eq` インスタンスを拡張

5. **src/Spinor/Primitive.hs**
   - `Spinor.GPGPU.gpgpuBindings` をインポートし、`Map.union` で統合

6. **src/Spinor/Server.hs**
   - `formatValForDisassembly`, `valContentText`, `valTitle`, `valTypeName` に `VCLContext`, `VCLBuffer`, `VCLKernel` のパターンマッチを追加

7. **src/Spinor/Lsp/Docs.hs**
   - `cl-init`, `to-device`, `to-host`, `cl-compile` の CLHS 形式ドキュメントを追加

**動作確認 (WSL2 環境):**
- `cabal build`: 警告なしでビルド成功
- `cabal test`: 全152テストパス (0 failures)
- ※OpenCL ランタイムテスト (REPL による手動検証) は GPU/CPU OpenCL ドライバがインストールされた環境で別途実施が必要
