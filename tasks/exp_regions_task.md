# Experimental: リージョンベースメモリ管理の実装指示書

## 1. 目的
Arena アロケータによる一括メモリ管理構文 `with-region` と `alloc-in` のプロトタイプを実装する。

## 2. 実装手順

### Step 1: AST とプリミティブの追加
- `src/Spinor/Syntax.hs` の `Expr` に `with-region` と `alloc-in` を追加。
- パーサーを更新し、これらの新構文を読み取れるようにする。

### Step 2: C ランタイムの拡張
- `runtime/spinor.h` および `runtime/spinor.c` に Arena アロケータを実装。
- `create_region`, `region_alloc`, `destroy_region` を追加。

### Step 3: コンパイラバックエンドの改修
- `src/Spinor/Compiler/C.hs` を修正し、`with-region` の C コード生成を実装。
- `alloc-in` に応じて適切なアロケーション関数を呼び出すように改修。

### Step 4: ドキュメントの執筆
- `manual/public/docs/syntax/regions.md` を執筆。
- `Sidebar.tsx` の「Experimental」カテゴリに「Region-based Memory」を追加。

### Step 5: テストによる検証
- `test/Spinor/RegionSpec.hs` を作成し、一括解放が C コードとして正しく生成されるか検証。

## 3. 完了条件
- `with-region` スコープ終了時にメモリが一括解放されるコードが生成されること。
- 静的なスコープチェックによって、リージョン外への逃避がエラーとして報告されること。

---
## 実装報告

### 実装方針

リージョンベースのメモリ管理を、既存のコンパイラインフラに最小限の変更で統合するプロトタイプを実装。AST に `EWithRegion` と `EAllocIn` を追加し、`EscapeAnalysis` モジュールで逃避解析を実行。C コード生成時には Arena アロケータのコードを自動生成し、`with-region` をブロックスコープに展開する設計とした。

### 実装内容

#### 1. AST とパーサーの拡張 (`Syntax.hs`)

```haskell
-- 新規 AST ノード
| EWithRegion SourceSpan Text Expr    -- (with-region r body)
| EAllocIn    SourceSpan Text Expr    -- (alloc-in r expr)
```

- `pList` パーサーに `with-region` と `alloc-in` の特殊形式を追加
- `exprSpan` 関数に新規パターンを追加

#### 2. 逃避解析の実装 (`EscapeAnalysis.hs`)

新規モジュール `src/Spinor/EscapeAnalysis.hs` を作成:

- **VarRegion**: InRegion / GlobalScope の状態管理
- **EscapeError**: EscapeReturn, EscapeAssign, UndefinedRegion の3種類のエラー
- **checkEscape**: AST を走査し逃避エラーと使用リージョン名を収集
- **analyzeExpr**: 各式を再帰的に解析し、リージョン内外の状態を追跡

#### 3. C バックエンドの改修 (`Codegen.hs`)

```haskell
-- リージョン対応コンパイル関数
compileProgramWithRegions :: [Expr] -> EscapeResult -> CCode

-- Arena アロケータの C コード生成
arenaAllocatorCode :: CCode

-- with-region のブロック展開
compileStmtWithRegion :: Expr -> CCode
```

Arena アロケータの実装:
- `RegionBlock`: 連続メモリブロック（リンクリスト）
- `Region`: ブロックの管理構造体
- `create_region`, `region_alloc`, `destroy_region` 関数
- `sp_region_make_int`, `sp_region_make_str` ヘルパー

#### 4. 型推論の拡張 (`Infer.hs`)

```haskell
-- with-region: 本体の型を返す
infer env (EWithRegion _ _ body) = infer env body

-- alloc-in: 内部式の型を返す
infer env (EAllocIn _ _ expr) = infer env expr
```

#### 5. サーバーの拡張 (`Server.hs`)

- `exprToText` と `exprToLispText` に新規パターンを追加

#### 6. テストの追加 (`RegionSpec.hs`)

13 件のテストを追加:
- with-region のパース
- alloc-in のパース
- 入れ子 with-region のパース
- 通常使用でのエラーなし確認
- 使用リージョン名の記録
- 未定義リージョンへの alloc-in エラー
- エラーメッセージのフォーマット（3件）
- C コード生成（4件）

#### 7. ドキュメントの作成

**`manual/public/docs/syntax/regions.md`**
- リージョンベースメモリ管理の概要
- with-region と alloc-in の使い方
- 逃避解析の制約
- C コード生成との連携
- Arena アロケータの実装詳細
- 所有権システムとの関係

**`manual/src/components/Sidebar.tsx`**
- Experimental カテゴリに「Region-based Memory」を追加

#### テスト結果

```
229 examples, 0 failures
```

### 完了日
2026-02-26
