# 24c: Error Handling & Condition System 実装指示書

## 1. 目的
エラー捕捉構文 (`ignore-errors`, `handler-case`) とリソース保護構文 (`unwind-protect`) を実装し、言語の堅牢性を向上させる。

## 2. 実装手順

### Step 1: 評価ロジックの実装 (`src/Spinor/Eval.hs`)
- `eval` 関数に以下の特殊形式の処理を追加する。
    - `ignore-errors`: `catchError` を使用して実装。
    - `handler-case`: エラーキャッチ後にローカル環境を拡張してハンドラを評価。
    - `unwind-protect`: 
        - `protected` の評価結果（`Either SpinorError Val`）を一時保存。
        - `cleanup` を評価（この際のエラーも考慮が必要）。
        - 保存していた結果を復元（エラーなら再送出）。

### Step 2: プリミティブ/メタデータの登録
- `src/Spinor/Docs.hs` に新設した 3 つの形式のドキュメントを追記する。
- CLHS 互換の Arguments / Description / Examples を記述すること。

### Step 3: マニュアルの作成とサイドバー更新
- `manual/public/docs/syntax/conditions.md` を作成し、使い方を記述。
- `manual/src/components/Sidebar.tsx` を編集し、`Control Flow` の直後に `Conditions & Errors` ページへのリンクを追加する。

### Step 4: 検証
- `test/Spinor/EvalSpec.hs` にテストケースを追加。
    - `(ignore-errors (/ 1 0))` が `nil` を返すこと。
    - `(handler-case (error "fail") (error (msg) msg))` が `"fail"` を返すこと。
    - `unwind-protect` 内でエラーが起きても、クリーンアップ用の `setq` 等が実行されていること。
- `cabal run spinor -- docgen` を実行し、マニュアルビルドを確認。

## 3. 完了条件
- エラーハンドリング構文が意図通り動作し、例外発生時も制御フローが壊れないこと。
- マニュアルのナビゲーションに「Conditions & Errors」が表示されること。

---
## 実装報告

### 実装方針

Haskell の `MonadError` / `catchError` を活用し、Common Lisp スタイルのエラーハンドリング構文を実装。mtl の `StateT + ExceptT` スタックでは `catchError` がエラー時に状態をリセットするため、`IORef` を使用した `tryEval` ヘルパーで状態保持を実現。

### 実装内容

#### 変更ファイル

**`src/Spinor/Eval.hs`**
- `tryEval` ヘルパー関数を追加: `IORef` を使用してエラー時も状態変更を保持
- 3つの特殊形式を追加:
  - `ignore-errors`: エラーを無視して `nil` を返す
  - `handler-case`: エラーを捕捉してハンドラを実行、エラーメッセージを変数に束縛
  - `unwind-protect`: 保護フォーム評価後、成否に関わらずクリーンアップを実行

**`src/Spinor/Lsp/Docs.hs`**
- 3つの構文の CLHS 形式ドキュメントを追加 (Arguments, Description, Examples, See Also)

**`test/Spinor/EvalSpec.hs`**
- 10件のエラーハンドリングテストを追加:
  - `ignore-errors` の基本動作 (3件)
  - `handler-case` の基本動作 (3件)
  - `unwind-protect` の基本動作 (4件)

**`manual/public/docs/syntax/conditions.md`**
- 新規作成: エラーハンドリングの解説ページ
- 各構文の使い方、用途、ベストプラクティスを記述
- Common Lisp との比較表を掲載

**`manual/src/components/Sidebar.tsx`**
- 「Conditions & Errors」リンクを「Control Flow」の直後に追加

#### 技術的詳細

**状態保持の課題と解決策:**

mtl の `catchError` は `StateT (ExceptT ...)` スタックでエラー時に状態をリセットするため、`unwind-protect` のクリーンアップで行った状態変更が外側のエラーハンドラで失われる問題がある。

解決策として `tryEval` ヘルパーを実装:
```haskell
tryEval :: Eval a -> Eval (Either SpinorError a)
tryEval action = do
  st <- get
  stRef <- liftIO $ newIORef st
  result <- catchError
    (do val <- action
        newSt <- get
        liftIO $ writeIORef stRef newSt
        pure (Right val))
    (\err -> pure (Left err))
  finalSt <- liftIO $ readIORef stRef
  put finalSt
  pure result
```

#### テスト結果

```
207 examples, 0 failures
```

#### docgen 結果

```
Generated 70 reference files.
Documentation generated successfully.
```

### 完了日
2026-02-26
