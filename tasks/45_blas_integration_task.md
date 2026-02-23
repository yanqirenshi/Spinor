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

**hmatrix の採用理由:**
- `hmatrix` は `Numeric.LinearAlgebra` モジュールを通じて BLAS/LAPACK をバックエンドとする高速な行列演算を提供する
- Step 44 で導入した `VMatrix` は `Data.Vector.Storable` ベースであり、`hmatrix` の内部表現と互換性が高いため、変換オーバーヘッドが最小限

**データ変換戦略:**
- `toLA`: `(r LA.>< c)` コンストラクタで `VS.Vector Double` → `LA.Matrix Double` に変換
- `fromLA`: `LA.flatten` / `LA.rows` / `LA.cols` で `LA.Matrix Double` → `VMatrix` に変換
- `fromLA` は直接 `Val` を返す設計とし、呼び出し側のボイラープレートを削減

**逆行列のエラーハンドリング:**
- `hmatrix` の `inv` は特異行列に対して例外を投げるため、`Control.Exception.try` + `System.IO.Unsafe.unsafePerformIO` で捕捉し、`Left Text` (Spinor のランタイムエラー) に変換
- `unsafePerformIO` の使用は `inv` が参照透過的であることに依拠しており、安全に使用可能

**次元チェック:**
- `m+`: 行数・列数の完全一致を検証
- `m*`: 左行列の列数と右行列の行数の一致を検証
- `inverse`: 正方行列であることを検証 (行数 == 列数)
- `transpose`: 次元制約なし (任意の行列に適用可能)

### 実装内容

**変更ファイル一覧:**

1. **spinor.cabal**
   - `hmatrix >= 0.20 && < 1` を `build-depends` に追加

2. **src/Spinor/Primitive.hs**
   - `Numeric.LinearAlgebra` のインポート追加
   - `toLA`: `VMatrix` の構成要素 → `LA.Matrix Double` への変換ヘルパー
   - `fromLA`: `LA.Matrix Double` → `VMatrix` への変換ヘルパー
   - `primMAdd` (`m+`): 要素ごとの加算。次元一致チェック付き
   - `primMMul` (`m*`): 行列積。左の列数 == 右の行数チェック付き
   - `primMTranspose` (`transpose`): 転置。`LA.tr` を使用
   - `primMInverse` (`inverse`): 逆行列。正方チェック + 特異行列の例外ハンドリング付き

3. **src/Spinor/Lsp/Docs.hs**
   - `m+`, `m*`, `transpose`, `inverse` の CLHS 形式ドキュメントを追加
   - 各エントリの `Exceptional Situations` に次元不一致・特異行列等のエラー条件を明記

4. **test/Spinor/EvalSpec.hs**
   - 「BLAS/LAPACK 行列演算」セクションを追加 (11テスト):
     - `m+` で 2x2 行列の加算
     - `m+` の全要素検証
     - `m+` の次元不一致エラー
     - `m*` で 2x2 行列の積
     - `m*` で 2x3 と 3x2 の行列積
     - `m*` の次元不一致エラー
     - `m*` の結果次元検証
     - `transpose` で 2x3 行列の転置 (次元と要素)
     - `inverse` で単位行列の逆行列
     - `inverse` で非正方行列エラー
     - `inverse` で特異行列エラー

**動作確認 (WSL2 環境):**
- `cabal build`: 警告なしでビルド成功
- `cabal test`: 全152テストパス (0 failures)
