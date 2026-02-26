# 43e: Architecture & Internals の実装指示書

## 1. 目的
Spinor の内部構造を詳細に解説するドキュメントを作成し、マニュアルサイトに統合する。

## 2. 実装手順

### Step 1: ドキュメントの執筆
- `manual/public/docs/architecture.md` を作成し、Specs に基づいて以下の内容を執筆する。
    - **Overview:** Haskell インタプリタと C トランスパイラの Dual Implementation のメリット。
    - **Frontend:** パース、環境管理、型システムの詳細。
    - **Backend:** C 言語への変換、TCO の仕組み、ランタイムの役割。
    - **Pipeline:** `spinor build` コマンドが内部で行っていることの図解（テキストベース）。

### Step 2: サイドバーへの登録
- `manual/src/components/Sidebar.tsx` を編集し、`sections` 配列の最後にリンクを追加する。
- ラベルは 「Architecture & Internals」 とする。

### Step 3: 整合性確認
- マニュアルサイトを実行し、目次から正しくアクセスできるか、図やコードブロックが適切に表示されているかを確認する。

### Step 4: ビルド確認
- `cabal run spinor -- docgen` を実行し、マニュアル全体が整合性を持って出力されることを確認。

## 3. 完了条件
- `architecture.md` が作成され、マニュアルのナビゲーションからアクセス可能であること。
- Spinor の主要な技術的特徴 (Dual Implementation, TCO, WASM) が正確に説明されていること。

---
## 実装報告

### 実装方針

言語開発者・コントリビューターを主な対象として、Spinor の技術的なアーキテクチャを体系的に解説するドキュメントを作成。ASCII ベースのパイプライン図を多用し、視覚的に理解しやすい構成とした。

### 実装内容

#### 作成ファイル

**`manual/public/docs/architecture.md`**
- **概要: Dual Implementation** - Haskell インタプリタと C トランスパイラの二重実装のメリットを図解
- **Frontend (Haskell Kernel)**
  - Parser: Megaparsec による S 式パース
  - AST: 位置情報付き構文木
  - Macro Expander: マクロ展開フロー
  - Type Inference: Hindley-Milner 型推論 (制約収集→単一化→汎化)
  - Package System: シンボル解決順序
- **Evaluator (Interpreter Mode)**
  - Val 型: 値の Haskell 表現
  - 評価モナド: StateT + ExceptT + IO
- **Backend (C Transpiler)**
  - トランスパイルパイプライン図
  - C Runtime: Tagged Union パターン
  - TCO: while(1) + continue による末尾再帰最適化
- **WASM Target**
  - Emscripten によるビルドフロー
  - Browser REPL の仕組み
- **拡張機能**
  - HPC & Science 統合 (OpenCL, OpenGL, hmatrix)
  - LSP / SLY 統合
- **ディレクトリ構造**: ソースコード配置の全体像

#### 変更ファイル

**`manual/src/components/Sidebar.tsx`**
- `API Reference` の後に `Architecture & Internals` へのリンクを追加

#### docgen 結果

```
Generated 70 reference files.
Documentation generated successfully.
```

### 完了日
2026-02-26
