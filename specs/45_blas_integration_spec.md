# Spec 45: BLAS/LAPACK Integration (Haskell Interpreter)

## 概要
Step 44 で導入した `VMatrix` 型に対し、Haskell の数値計算ライブラリ `hmatrix` を利用した高速な演算プリミティブを追加する。これにより、BLAS/LAPACK をバックエンドとした行列演算が可能になる。

## アーキテクチャ
Haskell インタプリタ側では、`Numeric.LinearAlgebra` (`hmatrix` パッケージ) を演算エンジンとして使用する。

### データ変換
`VMatrix` と `hmatrix` の内部表現は共に `Storable` ベクトルを基盤としているため、オーバーヘッドの少ない相互変換が可能である。

- **VMatrix -> hmatrix:**
  `rows` と `cols` を用いて、`Numeric.LinearAlgebra.(><)` または `Numeric.LinearAlgebra.reshape` で `Matrix Double` を生成する。
- **hmatrix -> VMatrix:**
  `Numeric.LinearAlgebra.flatten` でフラットな `Vector Double` を取得し、`rows` と `cols` 関数で次元を取得する。

## 新規プリミティブ仕様

### 1. `(m+ a b)`
- **説明:** 行列の要素ごとの加算。
- **制約:** `a` と `b` の行数・列数が完全に一致している必要がある。
- **実装:** `hmatrix` の `(+)` 演算子を使用。

### 2. `(m* a b)`
- **説明:** 行列の積 (Matrix Multiplication)。
- **制約:** `a` の列数と `b` の行数が一致している必要がある。
- **実装:** `hmatrix` の `(<>)` 演算子を使用。

### 3. `(transpose m)`
- **説明:** 行列の転置。
- **実装:** `hmatrix` の `tr` 関数を使用。

### 4. `(inverse m)`
- **説明:** 正方行列の逆行列。
- **制約:** 行列が正方であること。行列が特異 (singular) な場合はエラーを返す。
- **実装:** `hmatrix` の `inv` 関数を使用。

## 考慮事項
- **エラーハンドリング:** 次元不一致や特異行列による演算失敗時は、`Left Text` を介して Spinor のランタイムエラーとして報告する。
- **外部依存:** `hmatrix` はシステムに BLAS/LAPACK ライブラリ (OpenBLAS, MKL, Accelerate 等) がインストールされている必要がある。
