# Spec 43g: CLHS Core Chapters Expansion

## 概要
Spinor マニュアルの「Syntax」セクションを再編し、CLHS の構成に基づいたデータ型別の詳細解説ページ（章）を追加する。これにより、言語仕様の網羅性と検索性を向上させる。

## 1. 構成の再定義
`manual/public/docs/syntax/` 配下に以下の章を新設または再編する。

### Data Types (データ型)
- **Numbers (`numbers.md`)**: 整数 (`VInt`)、浮動小数点数 (`VFloat`)、算術演算プリミティブ。
- **Characters (`characters.md`)**: 文字の扱い（エスケープシーケンス含む）。
- **Strings (`strings.md`)**: 文字列 (`VStr`)、連結、部分取得、比較演算。
- **Conses (`conses.md`)**: コンスセル、ドット対、リストの構造、`car`/`cdr` 変換。
- **Arrays & Matrices (`arrays.md`)**: `VMatrix` 型、行列生成、行列演算 (BLAS/LAPACK)、GPGPU 連携。

### System (システム)
- **Packages & Modules (`packages.md`)**: `module` 定義、`import`/`export` ルール、名前空間。

## 2. サイドバーの階層化設計 (`Sidebar.tsx`)
サイドバーの項目を定義するデータ構造を拡張し、中カテゴリ（見出し）をサポートするか、ネストを深くする。

## 3. 各ページの記述ルール
- **概要**: 概念の定義。
- **記法**: S式でのリテラル表現。
- **関連関数**: 章に関連する API Reference へのリンクテーブル。
