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
(OpenCL デバイス選択のロジックや、コンパイルエラーのトラップ方法について)

### 実装内容
(変更したファイルの一覧、追加した主要な関数の説明など)
