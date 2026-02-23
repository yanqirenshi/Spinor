# Spec 44: Matrix/Tensor Type Foundation

## 概要
Spinor に科学技術計算の基盤となる行列データ型 (`Matrix`) を導入する。
Haskell インタプリタにおいては、高速な連続領域アクセスが可能な `Data.Vector.Storable` をラップしたデータ型として実装し、基本的な生成・アクセスプリミティブを提供する。

## データモデル
`src/Spinor/Val.hs` (または `Val` 型が定義されている場所) の `Val` 型を拡張する。

```haskell
import qualified Data.Vector.Storable as VS

data Val
  = ...
  | VMatrix Int Int (VS.Vector Double) -- 行数, 列数, フラットなデータ
  | ...
```

### 理由
- `Storable` ベクトルを使用することで、将来的に C 言語で実装された BLAS/LAPACK 等の数値計算ライブラリにポインタを直接渡すことが可能になる。
- 内部データは行優先 (Row-major) で保持する。

## プリミティブ仕様

### 1. `(matrix rows cols elements)`
- **引数:**
    - `rows`: 整数 (行数)
    - `cols`: 整数 (列数)
    - `elements`: `Double` (または `Int`) のリスト
- **戻り値:** `VMatrix`
- **エラー条件:** `rows * cols` とリストの長さが一致しない場合。

### 2. `(mdim m)`
- **引数:** `m`: 行列
- **戻り値:** `(rows cols)` のリスト。

### 3. `(mref m r c)`
- **引数:**
    - `m`: 行列
    - `r`: 行インデックス (0-indexed)
    - `c`: 列インデックス (0-indexed)
- **戻り値:** 浮動小数点数 (`Double`)
- **エラー条件:** インデックスが範囲外の場合。

## 文字列表現 (REPL Display)
行列の視認性を高めるため、以下の形式で表示する。
例: 2x2 行列
```lisp
#m((1.0 2.0) (3.0 4.0))
```
- `#m` プレフィックスを使用。
- 各行をリストとしてネストして表示する。

## 考慮事項
- 数値の混在: `matrix` に渡されるリストに `Int` が含まめる場合は、自動的に `Double` にキャストする。
- 境界チェック: インタプリタ側で厳密に行う。
