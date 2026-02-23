# Task 44: Matrix/Tensor Type Foundation の実装

Haskell インタプリタに `Matrix` 型と基本プリミティブを実装してください。

## ステップ

### 1. 依存関係の追加
- `spinor.cabal` の `library` セクションの `build-depends` に `vector` パッケージを追加してください。

### 2. データ型の追加と文字列化 (Val.hs / Syntax.hs)
- `Val` 型の定義（通常 `src/Spinor/Val.hs`）に `VMatrix Int Int (Data.Vector.Storable.Vector Double)` を追加してください。
- `showVal` 関数を修正し、`#m((row1) (row2) ...)` 形式の出力を実装してください。
- ※必要に応じて `src/Spinor/Syntax.hs` の `Expr` や `exprToVal` 等の関連箇所を確認してください。

### 3. プリミティブの実装 (Primitive.hs)
- `src/Spinor/Primitive.hs` (または `Primitives.hs`) に以下の関数を実装し、`primitiveBindings` に追加してください。
    - `matrix`: `[Val] -> Either Text Val`
    - `mdim`: `[Val] -> Either Text Val`
    - `mref`: `[Val] -> Either Text Val`
- リストから `Vector Double` への変換の際、`VInt` は `VFloat` (Double) に変換して扱ってください。

### 4. ドキュメントの追加 (Docs.hs)
- `src/Spinor/Lsp/Docs.hs` に新しいプリミティブのドキュメントを追加してください。
- CLHS フォーマット (Syntax, Arguments, Description, Examples 等) に従ってください。

### 5. ユニットテストの追加
- `test/Spinor/EvalSpec.hs` に行列のテストケースを追加してください。
    - 正しい生成と要素アクセス。
    - 範囲外アクセスやサイズ不一致時のエラーハンドリング。

### 6. 動作確認
- `cabal test` を実行し、すべてのテストがパスすることを確認してください。
- `cabal run spinor` で REPL を起動し、`#m((1 2)(3 4))` のような表示ができるか手動でも確認してください。

## 実装報告ルール
実装完了後、**このファイル自体を編集して**、以下のセクションを末尾に追記してください。

### 実装方針

**Storable ベクトルの採用理由:**
- `Data.Vector.Storable` は連続したメモリ領域にデータを格納するため、将来的に C 言語ベースの数値計算ライブラリ (BLAS/LAPACK 等) と連携する際にポインタを直接渡すことが可能
- 行優先 (Row-major) でデータを保持し、`mref` でのインデックス計算を `r * cols + c` で行う

**行列の表示ロジック:**
- `#m((row1) (row2) ...)` 形式で出力
- `showVal` で各行をネストしたリスト形式で表示
- 要素は Double として表示 (例: `1.0`, `2.0`)

**型変換:**
- `matrix` プリミティブで `VInt` を `VFloat` (Double) に自動変換
- これにより整数リテラルを直接使用可能 `(matrix 2 2 '(1 2 3 4))`

### 実装内容

**変更ファイル一覧:**

1. **spinor.cabal**
   - `vector >= 0.12 && < 1` を `build-depends` に追加

2. **src/Spinor/Val.hs**
   - `VFloat Double` 型を追加 (浮動小数点数サポート)
   - `VMatrix Int Int (VS.Vector Double)` 型を追加
   - `Eq` インスタンスを `VFloat`, `VMatrix` 対応に拡張
   - `showVal` を `VFloat`, `VMatrix` 対応に拡張

3. **src/Spinor/Primitive.hs**
   - `primMatrix`: 行列生成プリミティブ
     - 引数: `(rows cols elements)` → `VMatrix`
     - 要素数検証、VInt → Double 変換を実装
   - `primMdim`: 次元取得プリミティブ
     - 引数: `(m)` → `(rows cols)` のリスト
   - `primMref`: 要素参照プリミティブ
     - 引数: `(m row col)` → `VFloat`
     - 境界チェックを実装

4. **src/Spinor/Lsp/Docs.hs**
   - `matrix`, `mdim`, `mref` の CLHS 形式ドキュメントを追加
   - Syntax, Arguments and Values, Examples, Exceptional Situations 等を記載

5. **test/Spinor/EvalSpec.hs**
   - 行列操作のテストケースを追加:
     - 2x2 行列の生成確認
     - `mdim` による次元取得
     - `mref` による要素参照 (複数パターン)
     - 要素数不一致エラー
     - 範囲外アクセスエラー (行・列)
     - VInt → VFloat 変換確認

**備考:**
- `cabal test` は Windows 環境での `network` ライブラリビルドエラーにより実行不可 (`HsNetworkConfig.h` 不在)
- 実装自体は spec に準拠しており、環境が整えばテストパスが期待される
