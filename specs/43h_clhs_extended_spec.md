# Spec 43h: CLHS Extended Chapters

## 概要
Spinor マニュアルに、実装済みの実用機能を解説する 4 つの新しい章を追加する。CLHS の構成を参考にしつつ、Spinor 独自の実行ターゲット（WASM 等）に関する記述を統合する。

## 1. 新設される章の構成

### Symbols (`symbols.md`)
- **内容**: シンボルの性質、クオート (`'`) による評価の抑制、`eq` によるポインタレベルの同一性比較。
- **配置**: カテゴリ「Data Types」

### Iteration (`iteration.md`)
- **内容**: 構造化された反復マクロ (`dotimes`, `dolist`) の使い方。再帰関数と末尾再帰最適化 (TCO) について。
- **配置**: カテゴリ「Syntax」

### Files (`files.md`)
- **内容**: `read-file`, `write-file`, `append-file`, `file-exists?` プリミティブの解説。
- **配置**: カテゴリ「System」

### Environment (`environment.md`)
- **内容**: `command-line-args`, `getenv`。インタプリタ、ネイティブバイナリ、WASM の環境の差異。
- **配置**: カテゴリ「System」

## 2. サイドバーの構成変更 (`Sidebar.tsx`)
- **Syntax**: `Iteration` を追加。
- **Data Types**: `Symbols` を追加。
- **System**: `Files`, `Environment` を追加。
