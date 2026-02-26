# Experimental: 所有権システムのプロトタイプ実装指示書

## 1. 目的
静的な所有権解析と、それに基づく C 言語での自動メモリ解放 (`free`) のプロトタイプを構築する。

## 2. 実装手順

### Step 1: 型システムと AST の拡張
- `src/Spinor/Type.hs` に線形修飾子 (`Linearity`) を追加する。
- `src/Spinor/Syntax.hs` の `Expr` に所有権メタデータの保持を検討。

### Step 2: ボローチェッカーのプロトタイプ実装
- 新規モジュール `src/Spinor/BorrowCheck.hs` を作成。
- 変数の使用回数とライフタイムを追跡する静的解析ロジックを実装。

### Step 3: C コード生成の改修
- コンパイラの出力処理を改修し、解析結果に基づいて C コードの適切な位置にメモリ解放 (`free`) コードを挿入する。

### Step 4: 実験的ドキュメントの作成
- `manual/public/docs/syntax/ownership.md` を執筆。
- `Sidebar.tsx` を更新。

### Step 5: テストによる検証
- `test/Spinor/BorrowCheckSpec.hs` を追加し、二重使用やリークの検知を確認。

## 3. 完了条件
- 解析結果に基づき、C コードに適切な `free` が挿入されること。
- 不正な所有権操作が静的にエラーとして報告されること。

---
## 実装報告

### 実装方針

Rust 風の所有権モデルを参考に、最小限の変更で既存システムに統合可能なプロトタイプを実装。型システムに `Linearity` 修飾子を追加し、`BorrowCheck` モジュールで変数の使用回数とライフタイムを静的に追跡する。C コード生成時には解析結果を利用して自動的に `free()` を挿入する設計とした。

### 実装内容

#### 1. 型システムの拡張 (`Type.hs`)

```haskell
-- 線形性修飾子
data Linearity = Linear | Unrestricted

-- 型に線形性修飾子を追加
data Type = ... | TLinear Linearity Type
```

- `Infer.hs` の `Types` インスタンスに `TLinear` のパターンを追加

#### 2. Borrow Checker の実装 (`BorrowCheck.hs`)

新規モジュール `src/Spinor/BorrowCheck.hs` を作成:

- **VarState**: Owned / Borrowed / Consumed / Dropped の状態管理
- **OwnershipInfo**: 変数名、状態、線形性、定義位置、使用回数を追跡
- **BorrowError**: DoubleUse, Unconsumed, UseAfterMove の3種類のエラー
- **checkBorrow**: AST を走査し所有権エラーと drop ポイントを収集
- **finalizeScope**: スコープ終了時に未消費の線形変数を検出

特殊形式:
- `(linear name val)`: 線形変数の宣言
- `(drop name)`: 明示的な解放

#### 3. C バックエンドの改修 (`Codegen.hs`)

```haskell
-- 所有権情報を使ってコンパイル
compileProgramWithOwnership :: [Expr] -> BorrowResult -> CCode

-- drop ポイントに free() を自動挿入
generateFreeStatements :: Map Text SourceSpan -> CCode
```

#### 4. テストの追加 (`BorrowCheckSpec.hs`)

9 件のテストを追加:
- 通常の変数使用ではエラーなし
- 未使用の let 変数は非線形ならエラーなし
- 複数回の変数参照もエラーなし (非線形)
- linear で宣言した変数が未使用ならエラー
- linear 変数を1回使用すればエラーなし
- let 束縛の変数は drop ポイントが記録される
- エラーメッセージのフォーマット確認 (3件)

#### 5. ドキュメントの作成

**`manual/public/docs/syntax/ownership.md`**
- 所有権システムの概要と目的
- 線形変数の宣言方法と制約
- 明示的な解放 (drop)
- C コード生成との連携
- 型システムの拡張
- BorrowCheck モジュールの API
- 制限事項

**`manual/src/components/Sidebar.tsx`**
- 「Experimental」カテゴリを追加
- 「Linear Types & Ownership」へのリンクを配置

#### テスト結果

```
216 examples, 0 failures
```

#### docgen 結果

```
Generated 70 reference files.
Documentation generated successfully.
```

### 完了日
2026-02-26
