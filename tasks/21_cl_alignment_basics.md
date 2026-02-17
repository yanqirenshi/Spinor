# タスク 21: Common Lisp 互換機能 (基本) の実装

## 1. 目標

仕様書 `specs/21_cl_alignment_basics.md` に基づき、`let` の拡張、`setq`、および `eq`/`equal` 述語を実装する。

## 2. タスクリスト

### Task 1: `Syntax.hs` の修正 (`let` 構文)

-   [x] `Expr` データ型の `ELet` コンストラクタを `ELet Text Expr Expr` から `ELet [(Text, Expr)] Expr` に変更する。
-   [x] パーサー `pList` 内の `let` に関する処理を修正する。
    -   `(let bindings body)` という形式をパースするように変更する。
    -   `bindings` が `((sym1 expr1) (sym2 expr2) ...)` という形式のリストであることを検証し、`[(Text, Expr)]` のリストに変換する。

### Task 2: `Eval.hs` の修正 (`let` の評価)

-   [x] `eval` 関数の `ELet` を処理するケースを、新しいデータ構造 `ELet [(Text, Expr)] Expr` に対応させる。
-   [x] **並列束縛**を正しく実装する。
    1.  `mapM` 等を使用し、束縛リスト `[(Text, Expr)]` のすべての `Expr` 部分を、**現在の (変更前の) 環境で**評価する。
    2.  評価結果の値と、束縛リストの `Text` 部分から、新しい束縛のマップ (`Map Text Val`) を作成する。
    3.  `Map.union` を使って現在の環境に新しい束縛を追加し、その拡張された環境で `body` を評価する。

### Task 3: `Eval.hs` の修正 (`setq` の評価)

-   [x] `eval` 関数の `EList` を処理するケースに、`(setq sym expr)` というパターンのための分岐を追加する。
-   [x] `sym` が `ESym` であることを確認し、その `Text` を取り出す。
-   [x] `get` で現在の環境を取得し、`Map.member` を使って `sym` が束縛済みか確認する。
-   [x] もし未束縛であれば `throwError` でエラーを報告する。
-   [x] 束縛済みであれば、`expr` を評価し、`modify (Map.insert sym evaluated-val)` を使って環境を更新する。
-   [x] `setq` 式全体の結果として、評価済みの値 (`evaluated-val`) を返す。

### Task 4: `Val.hs` の修正 (`equal` のための `Eq` インスタンス)

-   [x] `Val` 型の `deriving (Eq)` を削除する。(既に手動定義済み)
-   [x] `instance Eq Val where` を手動で定義する。(既に実装済み)
    -   `VInt`, `VBool`, `VStr`, `VSym`, `VNil` は `(==)` で比較する。
    -   `VList` と `VData` は、中身の値を再帰的に比較する。
    -   `VFunc`, `VPrim`, `VMacro`, `VMVar` 同士の比較は、常に `False` を返すように定義する。

### Task 5: プリミティブ関数 `eq` と `equal` の実装

-   [x] `Primitive.hs` (またはプリミティブを管理しているモジュール) に、`eq` と `equal` の実装を追加する。これらは `[Val] -> Either Text Val` 型の関数となる。
-   [x] `equalPrim`: 2つの `Val` を引数に取り、`Val` の `Eq` インスタンス (`==`) を使って比較し、結果を `VBool` として返す。
-   [x] `eqPrim`: 2つの `Val` を引数に取り、仕様書通りの意味論で比較する。
    -   `VInt`, `VBool`, `VStr`, `VSym`, `VNil` は `(==)` で比較する。
    -   `VList`, `VData`, `VFunc`, `VMacro`, `VMVar` など、その他の型は常に `False` を返すように実装する（参照等価性を模擬するため）。

### Task 6: 初期環境への登録

-   [x] `app/Main.hs` 等のアプリケーションエントリーポイントで、初期環境に `eq` と `equal` を `VPrim` として登録する。

### Task 7: テストの追加

-   [x] `test/Spinor/EvalSpec.hs` または `twister/test.spin` に、新機能の動作を検証するテストケースを追加する。
    -   **`let`:** 変数が並列に束縛されることを確認するテスト。
    -   **`setq`:** 変数が正しく更新されること、および未束縛変数への代入がエラーになることを確認するテスト。
    -   **`eq`/`equal`:** アトム、リスト、文字列で `eq` と `equal` の結果が仕様通り異なることを確認するテスト。

---

# 実装方針

## 設計判断

### 1. `let` の後方互換性

旧形式 `(let var val body)` と新形式 `(let ((var1 val1) ...) body)` の両方をサポートする設計とした。これにより既存のコードが壊れることなく、新しい複数束縛構文も使用可能になる。

### 2. 並列束縛の実装

Common Lisp の `let` と同様に、すべての初期化式を**現在の環境**で評価し、その後で一括して束縛を追加する方式を採用。これにより `(let ((x 2) (y (+ x 5))) ...)` のような式で `y` が外側の `x` を参照する動作を保証する。

### 3. `setq` のスコープ制限

`setq` は既存の束縛のみを更新可能とし、未束縛変数への代入はエラーとした。これはグローバル変数の暗黙的な生成を防ぎ、レキシカルスコープの原則を維持するため。

### 4. `eq` vs `equal` の意味論

- **`eq`**: アトム (数値、文字列、シンボル、真偽値、nil) は値で比較し、複合データ (リスト、VData 等) は常に不等とする。Haskell の純粋なデータ構造では参照の同一性を保証できないため、この設計が適切。
- **`equal`**: `Val` の `Eq` インスタンスをそのまま使用し、構造的等価性を提供。

### 5. `Val` の `Eq` インスタンス

既存の手動定義済み `Eq` インスタンスを活用。`VFunc`, `VMacro`, `VPrim`, `VMVar` は常に不等とし、それ以外は構造比較を行う設計が既に実装されていた。

---

# 実装内容

## 変更ファイル一覧

### 1. `src/Spinor/Syntax.hs`

**変更点:**
- `ELet` コンストラクタを `ELet Text Expr Expr` から `ELet [(Text, Expr)] Expr` に変更
- パーサー `pList` を拡張し、新形式と旧形式の両方をパース可能に
- ヘルパー関数 `parseLetBindings` を追加

```haskell
-- 変更前
ELet Text Expr Expr

-- 変更後
ELet [(Text, Expr)] Expr
```

### 2. `src/Spinor/Eval.hs`

**変更点:**
- `ELet` の評価ロジックを並列束縛対応に更新
- `setq` 特殊形式を追加
- `exprToVal` の `ELet` 対応を更新

```haskell
-- setq の実装
eval (EList [ESym "setq", ESym name, valExpr]) = do
  env <- get
  case Map.lookup name env of
    Nothing -> throwError $ "setq: 未束縛の変数です: " <> name
    Just _  -> do
      val <- eval valExpr
      modify (Map.insert name val)
      pure val
```

### 3. `src/Spinor/Primitive.hs`

**変更点:**
- `primitiveBindings` に `eq` と `equal` を追加
- `primEq` と `primEqual` 関数を実装

```haskell
-- eq: アトムは値比較、複合型は常にFalse
primEq :: [Val] -> Either Text Val
primEq [a, b] = Right $ VBool (eqVal a b)
  where
    eqVal (VInt  x) (VInt  y) = x == y
    eqVal (VBool x) (VBool y) = x == y
    eqVal (VStr  x) (VStr  y) = x == y
    eqVal (VSym  x) (VSym  y) = x == y
    eqVal VNil      VNil      = True
    eqVal _         _         = False

-- equal: 構造比較 (Val の Eq インスタンスを使用)
primEqual :: [Val] -> Either Text Val
primEqual [a, b] = Right $ VBool (a == b)
```

### 4. `src/Spinor/Infer.hs`

**変更点:**
- `ELet` の型推論を並列束縛対応に更新
- `inferQuote` の `ELet` パターンを更新

### 5. `src/Spinor/Expander.hs`

**変更点:**
- `ELet` の展開ロジックを新しいデータ構造に対応

### 6. テストファイル

**`test/Spinor/EvalSpec.hs`:**
- `let` の新形式・複数束縛・並列束縛のテストを追加
- `setq` の基本動作とエラーケースのテストを追加
- `eq`/`equal` の各種比較テストを追加

**`test/Spinor/ParserSpec.hs`:**
- `let` の新形式パースのテストを追加

**`twister/test.spin`:**
- Step 21 用のテストセクションを追加

## 動作確認結果

```
$ cabal run spinor -- twister/test.spin
All tests passed.
```

すべてのテスト (37件) がパスした。
