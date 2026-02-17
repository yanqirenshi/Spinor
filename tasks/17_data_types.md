# タスク: ステップ17 - ユーザー定義データ型 (Algebraic Data Types)

## 目標

Spinor で `(data Name (Con1 arg...) (Con2 ...))` 構文をサポートし、代数的データ型 (ADT) を定義・生成できるようにする。

## 実装詳細指示

### 1. `src/Spinor/Syntax.hs` (拡張)

* `Expr` に `EData` を追加。
    * `EData Text [ConstructorDef]` (型名, コンストラクタ定義リスト)
    * `data ConstructorDef = ConstructorDef Text [Text]` (コンストラクタ名, 引数の型変数名リスト)
* パーサー修正:
    * `(data Name (Con1 a b) (Con2))` 形式をパース。
    * 例: `(data List (Cons a (List a)) (Nil))`

### 2. `src/Spinor/Type.hs` (拡張)

* `Type` に `TCon` と `TApp` を追加（または既存を拡張）。
    * `TCon Text` (具体的な型コンストラクタ名。例: "Maybe", "Int")
    * `TApp Type Type` (型適用。例: `TApp (TCon "Maybe") TInt` -> `Maybe Int`)
    * ※ 既存の `TInt`, `TBool` を `TCon "Int"`, `TCon "Bool"` にリファクタリングするか、共存させるかは実装の容易さで判断して良い。今回は共存でOK。

### 3. `src/Spinor/Val.hs` (拡張)

* `Val` に `VData` を追加。
    * `VData Text [Val]` (コンストラクタ名, 保持する値のリスト)
    * 例: `Just 10` -> `VData "Just" [VInt 10]`

### 4. `src/Spinor/Infer.hs` (拡張)

* `inferTop` (トップレベル推論) で `EData` を処理。
* **コンストラクタの型登録:**
    * 各コンストラクタについて、対応する関数型を生成し、`TypeEnv` に登録する。
    * 例: `Just` -> `forall a. a -> Maybe a`
    * 例: `Nothing` -> `forall a. Maybe a`
* **型チェック:**
    * 引数の型変数 (`a`, `b`...) が定義内で一貫しているかチェック（今回は簡易実装で、宣言された型変数はすべて `forall` に含める方針で良い）。

### 5. `src/Spinor/Eval.hs` (拡張)

* `eval` で `EData` を処理。
    * 実行時の環境 (`Env`) に、コンストラクタ関数を登録する。
    * コンストラクタ関数は、引数を受け取って `VData` を返すプリミティブ関数として生成する。
    * 例: `Just` は `\x -> VData "Just" [x]` という関数になる。

## 確認事項 (REPL & Test)

実装後、以下のコードが動作することを確認してください。

```lisp
;; データ型の定義
(data Maybe (Just a) (Nothing))

;; コンストラクタの利用
(let x (Just 10))
:: Maybe Int
VData "Just" [10]

(let y Nothing)
:: Maybe a
VData "Nothing" []

;; リストの再実装（テスト）
(data MyList (MyCons a (MyList a)) (MyNil))
(let l (MyCons 1 (MyCons 2 MyNil)))
:: MyList Int

```

## 出力要件

* 変更された Haskell ファイル (`Syntax`, `Type`, `Val`, `Infer`, `Eval`)。
* 動作確認ログ。
* （もしあれば）`test/Spinor/DataSpec.hs` などの新規テストコード。

# 実装方針

## 概要

`(data Name (Con1 arg...) (Con2 ...))` 構文で代数的データ型 (ADT) を定義・生成できるようにする。Spinor パイプラインの全レイヤー（パーサー → 展開 → 型推論 → 評価）に対応を追加する。

## 設計判断

### ConstructorDef に TypeExpr を使う

計画時の `[Text]` ではなく `[TypeExpr]` を採用。`(MyCons a (MyList a))` のような型適用（再帰的な型）を表現するため。`TypeExpr` は `TEVar Text` (型変数) と `TEApp Text [TypeExpr]` (型適用) の2コンストラクタ。

### TCon / TApp の追加

`Type` に `TCon Text` (型コンストラクタ名) と `TApp Type Type` (型適用) を追加。既存の `TInt`, `TBool`, `TStr` は共存させる（リファクタリングは将来のオプション）。

### VData コンストラクタ

`VData Text [Val]` — コンストラクタ名とフィールド値のリストを保持。0引数コンストラクタは `VData "Nothing" []` として直接環境に束縛。N引数コンストラクタは `VPrim` で引数数チェック付きの関数として束縛する。

### 型推論での処理

`inferTop` で `EData` を処理:
1. 全コンストラクタ引数から自由型変数を収集
2. 結果型を生成: `foldl TApp (TCon typeName) (map TVar allTypeVars)`
3. 各コンストラクタの型スキームを生成: 引数型を `→` で連結して結果型へ
4. `TypeEnv` に登録

### typeExprToType ヘルパー

`TypeExpr` → `Type` の変換関数。`TEVar v` → `TVar v`, `TEApp name args` → `foldl TApp (TCon name) (map typeExprToType args)`。

## 変更の流れ

1. `src/Spinor/Syntax.hs` (拡張) — `TypeExpr`, `ConstructorDef`, `EData`, パーサー追加
2. `src/Spinor/Type.hs` (拡張) — `TCon`, `TApp` 追加
3. `src/Spinor/Val.hs` (拡張) — `VData` 追加
4. `src/Spinor/Eval.hs` (拡張) — `eval EData`、コンストラクタ登録
5. `src/Spinor/Infer.hs` (拡張) — `inferTop EData`、型推論
6. `src/Spinor/Expander.hs` (拡張) — `expand EData` パススルー
7. テスト — ParserSpec, EvalSpec に ADT テスト追加

# 実装内容

## 変更ファイル一覧 (7ファイル)

### 1. `src/Spinor/Syntax.hs` — AST 拡張 + パーサー

- `TypeExpr` 型を新規追加: コンストラクタ引数の型記述用
  - `TEVar Text` — 型変数 (例: `a`)
  - `TEApp Text [TypeExpr]` — 型適用 (例: `(MyList a)`)
- `ConstructorDef` 型を新規追加: `ConstructorDef Text [TypeExpr]` (コンストラクタ名, 引数の型式リスト)
  - 計画時の `[Text]` ではなく `[TypeExpr]` を採用。`(MyCons a (MyList a))` のような型適用を表現するため
- `Expr` に `EData Text [ConstructorDef]` コンストラクタを追加
- パーサー追加:
  - `pTypeExpr` — 型式パーサー (シンボル単体 → `TEVar`, `(Name args...)` → `TEApp`)
  - `pConstructorDef` — コンストラクタ定義パーサー
  - `pData` — `(data TypeName (Con1 args...) ...)` をパースして `EData` に変換
  - `parseExpr` に `try pData` を先頭に追加
- エクスポートに `TypeExpr(..)`, `ConstructorDef(..)` を追加

### 2. `src/Spinor/Type.hs` — 型コンストラクタ追加

- `Type` に2つのコンストラクタを追加:
  - `TCon Text` — 型コンストラクタ名 (例: `"Maybe"`)
  - `TApp Type Type` — 型適用 (例: `TApp (TCon "Maybe") TInt`)
- 既存の `TInt`, `TBool`, `TStr` は共存させた (リファクタリングは今回しない)
- `showType` に `TCon`, `TApp` のケース追加

### 3. `src/Spinor/Val.hs` — VData 追加

- `Val` に `VData Text [Val]` コンストラクタを追加 (コンストラクタ名, フィールド値のリスト)
- `Eq` インスタンスに `VData` ケース追加 (名前とフィールド値の構造的比較)
- `showVal` に `VData` ケース追加:
  - 0引数: コンストラクタ名のみ表示 (例: `Nothing`)
  - N引数: `(Just 10)` 形式で表示

### 4. `src/Spinor/Eval.hs` — EData 評価

- `FlexibleContexts` プラグマ追加
- `eval (EData _typeName constrs)` を追加:
  - 各コンストラクタを環境に登録
  - 0引数コンストラクタ: `VData "Nothing" []` を直接束縛
  - N引数コンストラクタ: `VPrim` で引数数チェック付きの関数として束縛
  - 戻り値は `VNil`
- `exprToVal` に `EData` ケース追加
- `valToExpr` に `VData` ケース追加

### 5. `src/Spinor/Infer.hs` — 型推論

- `Types Type` インスタンスの `apply`/`ftv` に `TCon`, `TApp` ケース追加
- `unify` に `TCon`, `TApp` ケース追加
- `inferTop` に `EData` 処理を追加:
  - 全コンストラクタ引数から自由型変数を収集
  - 結果型を生成: `foldl TApp (TCon typeName) (map TVar allTypeVars)`
  - 各コンストラクタの型スキームを生成して `TypeEnv` に登録
    - 例: `Just` → `Scheme ["a"] (TArr (TVar "a") (TApp (TCon "Maybe") (TVar "a")))`
    - 例: `Nothing` → `Scheme ["a"] (TApp (TCon "Maybe") (TVar "a"))`
- `typeExprToType :: TypeExpr -> Type` ヘルパー追加
- `infer` / `inferQuote` に `EData` ケース追加 (Unit 型を返す)

### 6. `src/Spinor/Expander.hs` — EData パススルー

- `expand` に `EData` ケース追加 (展開不要、そのまま返す)

### 7. テスト

既存のテストファイルに追加 (新規ファイルは作成せず):

- `test/Spinor/ParserSpec.hs` に `data 式 (ADT)` テスト2件追加:
  - `(data Maybe (Just a) (Nothing))` のパース検証
  - `(data MyList (MyCons a (MyList a)) (MyNil))` のパース検証 (型適用含む)
- `test/Spinor/EvalSpec.hs` に `ユーザー定義データ型 (ADT)` テスト3件追加:
  - 0引数コンストラクタ: `Nothing` → `VData "Nothing" []`
  - 1引数コンストラクタ: `(Just 10)` → `VData "Just" [VInt 10]`
  - ネストしたコンストラクタ: `(MyCons 1 (MyCons 2 MyNil))` → ネストした `VData`
- `evalMulti` ヘルパー追加 (複数式を順次評価する)

## テスト結果

```
46 examples, 0 failures
Test suite spinor-test: PASS
```

`cabal build` および `cabal test` ともに成功。
