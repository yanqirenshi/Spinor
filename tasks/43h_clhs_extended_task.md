# Task 43h: CLHS Extended Chapters の実装

マニュアルに 4 つの新しい章を追加し、サイドバーのナビゲーションを拡張してください。

## ステップ

### 1. Markdown ページの作成
- `manual/public/docs/syntax/` 配下に `symbols.md`, `iteration.md`, `files.md`, `environment.md` を作成し、執筆してください。
- 末尾に関連リファレンスへのリンクを含めてください。

### 2. サイドバーの更新 (`Sidebar.tsx`)
- `sections` 定義を更新し、適切なカテゴリに新ページを組み込んでください。

### 3. 重複記述のクリーンアップ
- `atoms.md` や `control-flow.md` からのリンク誘導を整備してください。

### 4. 動作確認
- 全てのリンクが正しく機能し、コンテンツが表示されることを確認してください。

## 実装報告ルール
実装完了後、**このファイル自体を編集して**、以下のセクションを末尾に追記してください。

### 実装方針

**各章の重点:**
- **symbols.md**: クオートによる評価抑制、`eq` と `equal` の違い、特殊シンボル `nil`/`t` の解説
- **iteration.md**: 再帰による反復パターン、末尾再帰最適化 (TCO)、アキュムレータパターン、高階関数 (map/filter/fold)
- **files.md**: ファイル IO プリミティブの実践的な使用例、エラー処理パターン
- **environment.md**: 実行環境の違い (インタプリタ/WASM)、コマンドライン引数と環境変数

**CLHS との対応:**
- CLHS の Chapter 10 (Symbols)、Chapter 6 (Iteration)、Chapter 21 (Streams)、Chapter 25 (Environment) を参考に、Spinor の実装状況に合わせた内容に調整

### 実装内容

**新規作成ファイル:**

| ファイル | カテゴリ | 内容 |
|---------|---------|------|
| `manual/public/docs/syntax/symbols.md` | Data Types | シンボルの性質、quote、eq/equal |
| `manual/public/docs/syntax/iteration.md` | Syntax | 再帰、TCO、高階関数 |
| `manual/public/docs/syntax/files.md` | System | read-file, write-file, append-file, file-exists? |
| `manual/public/docs/syntax/environment.md` | System | command-line-args, getenv, 実行環境 |

**変更ファイル:**

| ファイル | 変更内容 |
|---------|---------|
| `manual/src/components/Sidebar.tsx` | Syntax に Iteration、Data Types に Symbols、System に Files/Environment を追加 |
| `manual/public/docs/syntax/atoms.md` | シンボル解説を簡略化、symbols.md へのリンクを追加 |
| `manual/public/docs/syntax/control-flow.md` | iteration.md へのリンクを追加 |

**サイドバー構成:**

```
Syntax
├── Atoms / Type System / Evaluation / Definitions
├── Control Flow
├── Iteration (新規)
└── Algebraic Types

Data Types
├── Symbols (新規)
├── Numbers / Characters / Strings / Conses
└── Arrays & Matrices

System
├── Packages & Modules
├── Files (新規)
└── Environment (新規)
```
