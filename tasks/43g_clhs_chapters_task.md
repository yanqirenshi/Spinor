# Task 43g: CLHS Core Chapters Expansion の実装

マニュアルに CLHS 準拠の章を追加し、サイドバーを整理してください。

## ステップ

### 1. Markdown ページの新規作成
- `manual/public/docs/syntax/` に以下のファイルを新規作成し、Spinor の最新仕様に基づいて執筆してください。
    - `packages.md`: モジュールシステムの解説。
    - `numbers.md`: 数値型と演算の解説。
    - `characters.md`: 文字の扱い。
    - `strings.md`: 文字列操作の解説。
    - `conses.md`: リスト構造とコンスセルの解説。
    - `arrays.md`: 行列 (`VMatrix`) の解説。

### 2. サイドバーのカテゴリ再編 (`Sidebar.tsx`)
- `manual/src/components/Sidebar.tsx` のリンク定義を更新し、新規ページを「Data Types」等の適切なカテゴリに分類してください。

### 3. 既存コンテンツの調整
- `atoms.md` 内の重複解説を整理し、各詳細ページへのリンクに置き換えてください。

### 4. 動作確認
- サイドバーのナビゲーションと、新規ページの表示内容を確認してください。

## 実装報告ルール
実装完了後、**このファイル自体を編集して**、以下のセクションを末尾に追記してください。

### 実装方針

**CLHS 構成の適用:**
- CLHS (Common Lisp HyperSpec) のデータ型章を参考に、Spinor の実装に合わせた章立てを採用
- Data Types カテゴリ: Numbers, Characters, Strings, Conses, Arrays & Matrices
- System カテゴリ: Packages & Modules
- 各ページは「概要 → リテラル表記 → 操作 → 型推論 → Symbols」の統一構成

**サイドバー設計:**
- NavItem のネスト機能を活用し、カテゴリ見出しを追加
- "Syntax" 配下の既存ページと、新規 "Data Types" / "System" カテゴリを並列配置
- 既存の "Data Types" ページは "Algebraic Types" にリネームして区別

### 実装内容

**新規作成ファイル:**

| ファイル | 内容 |
|---------|------|
| `manual/public/docs/syntax/numbers.md` | 整数・浮動小数点数、算術演算、比較演算 |
| `manual/public/docs/syntax/characters.md` | 文字の扱い、エスケープシーケンス、Unicode |
| `manual/public/docs/syntax/strings.md` | 文字列操作、連結、部分取得、比較 |
| `manual/public/docs/syntax/conses.md` | コンスセル、car/cdr、リスト構造、パターンマッチ |
| `manual/public/docs/syntax/arrays.md` | Matrix 型、行列演算、GPGPU (OpenCL) 連携 |
| `manual/public/docs/syntax/packages.md` | モジュールシステム、load、名前空間 |

**変更ファイル:**

| ファイル | 変更内容 |
|---------|---------|
| `manual/src/components/Sidebar.tsx` | "Data Types" / "System" カテゴリ追加、既存 "Data Types" を "Algebraic Types" にリネーム |
| `manual/public/docs/syntax/atoms.md` | 数値・文字列の詳細解説を簡略化し、各詳細ページへのリンクを追加 |

**サイドバー構成:**

```
Syntax
├── Atoms
├── Type System
├── Evaluation
├── Definitions
├── Control Flow
└── Algebraic Types

Data Types
├── Numbers
├── Characters
├── Strings
├── Conses
└── Arrays & Matrices

System
└── Packages & Modules
```
