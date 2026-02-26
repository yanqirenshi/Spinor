# 43e: Architecture & Internals (内部実装の解説) 仕様書

## 1. 概要
Spinor の設計思想、内部構造、およびトランスパイルパイプラインを技術的に解説する。開発者や、言語実装に興味のあるユーザーを対象とし、Spinor がどのように Haskell の表現力と C の実行性能を両立しているかを明示する。

## 2. アーキテクチャ構成

### 2.1 全体像 (Frontend vs Backend)
- **Frontend (Kernel):** Haskell で実装。対話的な開発 (REPL)、型推論、マクロ展開を担当。
- **Backend (Transpiler):** AST を C 言語へ変換。スタンドアロンバイナリや WASM へのコンパイルを担当。

### 2.2 Frontend (Haskell Kernel)
- **Parser:** `megaparsec` を使用した S 式のパース。
- **Environment:** パッケージシステムと `EvalState` による動的な名前空間管理。
- **Type Inference:** ヒンドリー・ミルナー (HM) 型推論に基づいた静的型付け。
- **Evaluator:** インタプリタ実行時の AST ウォーカー。

### 2.3 Backend (C Transpiler)
- **Translation Strategy:** Lisp の関数を C の関数に、クロージャを構造体と環境ポインタにマッピング。
- **TCO (Tail Call Optimization):** トランポリン (Trampoline) 方式を用いた末尾再帰のループ化。
- **Runtime:** 最小限のランタイム、およびガベージコレクション (Boehm GC 等) または所有権ベースのメモリ管理の将来構想。

## 3. 実行ターゲット (Execution Targets)
- **Native:** GCC/Clang を用いた実行バイナリの生成フロー。
- **WASM:** Emscripten を経由したブラウザ実行環境の構築。

## 4. ナビゲーション
- `Sidebar.tsx` の末尾に「Architecture & Internals」を追加。
- ドキュメントパス: `manual/public/docs/architecture.md`
