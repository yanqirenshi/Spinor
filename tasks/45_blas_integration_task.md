# Task 45: BLAS/LAPACK Integration (Haskell Interpreter) の実装

`hmatrix` ライブラリを導入し、高速な行列演算プリミティブを実装してください。

## ステップ

### 1. 依存関係の追加
- `spinor.cabal` の `library` セクションの `build-depends` に `hmatrix` を追加してください。
- ※環境によっては `libblas-dev`, `liblapack-dev` 等のインストールが必要です。

### 2. 変換ヘルパーの実装 (Primitive.hs)
- `src/Spinor/Primitive.hs` に `hmatrix` との相互変換を行う非公開関数を実装してください。
  ```haskell
  -- 概略
  import qualified Numeric.LinearAlgebra as LA

  toLA :: Int -> Int -> VS.Vector Double -> LA.Matrix Double
  toLA r c vec = (r LA.>< c) (VS.toList vec) -- または reshape

  fromLA :: LA.Matrix Double -> (Int, Int, VS.Vector Double)
  fromLA m = (LA.rows m, LA.cols m, LA.flatten m)
  ```

### 3. プリミティブの実装
- 以下の関数を `src/Spinor/Primitive.hs` に実装し、`primitiveBindings` に登録してください。
    - `m+`: `primMAdd` (要素ごとの加算)
    - `m*`: `primMMul` (行列積)
    - `transpose`: `primMTranspose` (転置)
    - `inverse`: `primMInverse` (逆行列)
- 各関数で、引数が `VMatrix` であることのチェックと、演算前の次元整合性チェックを厳密に行ってください。

### 4. ドキュメントの追加 (Docs.hs)
- `src/Spinor/Lsp/Docs.hs` に上記 4 つのプリミティブのドキュメントを追加してください。
- CLHS フォーマットに従い、`Exceptional Situations` セクションで次元不一致等のエラー条件を明記してください。

### 5. ユニットテスト
- `test/Spinor/EvalSpec.hs` にテストを追加してください。
    - 2x2 行列どうしの加算、積。
    - 2x3 と 3x2 の行列積。
    - 転置の結果が正しいか。
    - 単位行列の逆行列が単位行列になるか。

### 6. 動作確認
- `cabal test` で演算の正確性を確認してください。
- `cabal run spinor` で REPL を起動し、行列演算が正常に行えることを確認してください。

## 実装報告ルール
実装完了後、**このファイル自体を編集して**、以下のセクションを末尾に追記してください。

### 実装方針
(hmatrix の採用に当たって考慮した点、逆行列のエラーハンドリングの実装方法など)

### 実装内容
(変更したファイルの一覧、追加した主要なプリミティブ関数の説明など)
