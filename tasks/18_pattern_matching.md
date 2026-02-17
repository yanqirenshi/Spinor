# タスク: ステップ18 - パターンマッチ (Pattern Matching)

## 目標
ユーザー定義データ型 (ADT) を分解し、分岐処理を行うための `match` 式を実装する。

## 実装詳細指示

### 1. `src/Spinor/Syntax.hs` (拡張)
* `Expr` に `EMatch` を追加。
    * `EMatch Expr [(Pattern, Expr)]` (ターゲット式, (パターン, 本体) のリスト)
* `Pattern` データ型を定義。
    * `PVar Text`       (変数パターン: `x`)
    * `PCon Text [Pattern]` (コンストラクタパターン: `(Just x)`, `(Cons x xs)`)
    * `PLit Val`        (リテラルパターン: `1`, `#t`)
    * `PWild`           (ワイルドカード: `_`)
* パーサー修正:
    * `(match target (pat1 body1) ...)` 形式をパース。
    * パターンのパース処理 (`pPattern`) を実装。

### 2. `src/Spinor/Infer.hs` (拡張)
* `infer` に `EMatch` の処理を追加。
    1. ターゲット式 (`target`) を推論し、型 `tTarget` を得る。
    2. 各分岐 `(pat, body)` について:
        * パターン `pat` が `tTarget` と一致するか検証 (unify)。
        * パターン内の変数 (`PVar`) の型を特定し、環境 (`TypeEnv`) に追加。
        * 新しい環境で `body` を推論。
    3. 全ての `body` の型が一致することを検証 (unify)。

### 3. `src/Spinor/Eval.hs` (拡張)
* `eval` に `EMatch` の処理を追加。
    1. ターゲット式を評価して値 `v` を得る。
    2. 上から順に `matchPattern v pat` を試行。
    3. マッチしたら、束縛された変数を環境 (`Env`) に追加して `body` を評価。
    4. マッチしなければ次の分岐へ。どれもマッチしなければエラー。

### 4. `src/Spinor/Expander.hs` (拡張)
* `expand` に `EMatch` の処理を追加。
    * ターゲットと各 `body` を再帰的に展開する。
    * パターン自体は展開不要。

## 確認事項 (REPL & Test)
以下のコードが動作することを確認してください。

```lisp
;; 1. Maybe のマッチ
(let x (Just 42))
(match x
  ((Just v) (+ v 1))
  (Nothing 0))
:: Int
43

;; 2. リスト (Cons/Nil) のマッチ
(data List (Cons a (List a)) (Nil))
(let ls (Cons 1 (Cons 2 Nil)))
(match ls
  ((Cons x xs) x)
  (Nil 0))
:: Int
1

;; 3. ワイルドカードとリテラル
(match 10
  (0 "zero")
  (_ "non-zero"))
:: String
"non-zero"
```

# 実装方針
## 概要

既存の ADT (VData) を分解・分岐するための `match` 式を、Spinor パイプラインの全レイヤー（パーサー → 展開 → 型推論 → 評価）に追加する。変更対象は 4 つのコアモジュールと 2 つのテストファイル。

## 設計判断

### PLit に Expr を使う理由

`Pattern` 型は `Syntax.hs` で定義するが、`Syntax` モジュールは `Val` を import しない（循環依存を避けるため）。
そのためリテラルパターン `PLit` は `Val` ではなく `Expr` を保持し、評価時に `exprToVal` で変換して比較する。

### パターン内の大文字/小文字判定

パーサーでコンストラクタと変数を区別するために `Data.Char.isUpper` を使用する:
- 大文字始まりの裸シンボル → 0引数コンストラクタ `PCon name []`（例: `Nothing`）
- 小文字始まりの裸シンボル → 変数パターン `PVar name`（例: `x`）
- 括弧で囲まれた形式 → 引数付きコンストラクタ `PCon name pats`（例: `(Just x)`）

### 型推論の戦略

Algorithm W に沿った approach:
1. target を推論して `tTarget` を得る
2. フレッシュ型変数 `tResult` を全分岐共通の結果型とする
3. 各分岐で `inferPattern` により パターン変数の型を推論・環境に追加し、body の型を `tResult` と unify
4. `PCon` パターンでは環境からコンストラクタのスキームを instantiate し、`splitArrType` で引数型リストと結果型に分解して再帰推論

### 評価の戦略

上から順にパターンマッチを試行する first-match 方式。
`matchPattern :: Val -> Pattern -> Maybe Env` が `Just bindings` を返したら、その束縛を環境に追加して body を評価。
全分岐にマッチしない場合は実行時エラー。

## 変更の流れ

1. **Syntax.hs**: `Pattern` 型定義 → `EMatch` コンストラクタ追加 → `pMatch` / `pPattern` パーサー実装 → エクスポート追加
2. **Eval.hs**: `eval EMatch` ケース → `matchBranches` / `matchPattern` 関数 → `exprToVal` EMatch ケース
3. **Infer.hs**: `infer EMatch` ケース → `inferPattern` 関数 → `splitArrType` ヘルパー → `inferQuote` EMatch ケース
4. **Expander.hs**: `expand EMatch` ケース（target と body のみ展開、パターンは展開不要）
5. **テスト**: ParserSpec に 3 件、EvalSpec に 5 件追加
6. **検証**: `cabal build && cabal test` で全テスト通過を確認

# 実装内容

## 変更ファイル一覧

| ファイル | 変更種別 |
|---|---|
| `src/Spinor/Syntax.hs` | 拡張 |
| `src/Spinor/Eval.hs` | 拡張 |
| `src/Spinor/Infer.hs` | 拡張 |
| `src/Spinor/Expander.hs` | 拡張 |
| `test/Spinor/ParserSpec.hs` | テスト追加 |
| `test/Spinor/EvalSpec.hs` | テスト追加 |

## 1. `src/Spinor/Syntax.hs`

### Pattern 型の追加

```haskell
data Pattern
  = PVar  Text             -- 変数パターン: x (任意の値にマッチし束縛)
  | PCon  Text [Pattern]   -- コンストラクタパターン: (Just x), (Cons x xs)
  | PLit  Expr             -- リテラルパターン: 1, #t, "hello"
  | PWild                  -- ワイルドカード: _
  deriving (Show, Eq)
```

`PLit` は `Val` ではなく `Expr` を保持する（Syntax モジュールは Val を import しないため）。

### Expr に EMatch コンストラクタ追加

```haskell
| EMatch Expr [(Pattern, Expr)]   -- (match target (pat1 body1) (pat2 body2) ...)
```

### パーサー追加

- `pMatch`: `(match target (pat body) ...)` をパース。`parseExpr` に `try pMatch` を追加。
- `pPattern`: パターンのパーサー。以下を判定:
  - `_` → `PWild`
  - `(ConName pat...)` → `PCon`（括弧で囲まれた形式）
  - `#t` / `#f` → `PLit (EBool ...)`
  - `"..."` → `PLit (EStr ...)`
  - 整数 → `PLit (EInt ...)`
  - 大文字始まりの裸シンボル → `PCon name []`（0引数コンストラクタ）
  - それ以外のシンボル → `PVar`

大文字判定には `Data.Char.isUpper` を使用。

### エクスポート

`Pattern(..)` をモジュールエクスポートに追加。

## 2. `src/Spinor/Eval.hs`

### eval に EMatch ケース追加

```haskell
eval (EMatch targetExpr branches) = do
  targetVal <- eval targetExpr
  matchBranches targetVal branches
```

### matchBranches

分岐リストを上から順に走査し、最初にマッチしたパターンの body を評価。
マッチ時はパターン変数の束縛を環境に追加してから body を評価し、評価後に環境を復元する。
全分岐にマッチしない場合はエラー。

### matchPattern

```haskell
matchPattern :: Val -> Pattern -> Maybe Env
```

- `PVar name` → 常にマッチ、`Map.singleton name val`
- `PWild` → 常にマッチ、空の束縛
- `PLit expr` → `exprToVal expr` と値を比較、一致したら空の束縛
- `PCon conName pats` → `VData conName vals` と照合。コンストラクタ名が一致し、各フィールドが再帰的にマッチしたら束縛を合成

### exprToVal / valToExpr

`EMatch` ケースを追加（`exprToVal (EMatch _ _) = VSym "<match>"`）。

## 3. `src/Spinor/Infer.hs`

### infer に EMatch ケース追加

1. target を推論 → `tTarget` を取得
2. フレッシュ型変数 `tResult` を生成（全分岐の body の共通型）
3. 各 `(pat, body)` について:
   - `inferPattern` でパターン内変数の型環境を取得
   - 拡張環境で body を推論
   - body の型を `tResult` と unify
4. 最終的な置換と `tResult` を返す

### inferPattern

```haskell
inferPattern :: TypeEnv -> Type -> Pattern -> Infer (Subst, TypeEnv)
```

- `PVar name` → フレッシュ型変数を割り当て、tTarget と unify、環境に追加
- `PWild` → 環境変更なし
- `PLit expr` → リテラルの型を推論し tTarget と unify、環境変更なし
- `PCon conName pats` → 環境から conName のスキームを instantiate し、結果型を tTarget と unify、各引数の型でサブパターンを再帰推論

### splitArrType ヘルパー追加

関数型 `a -> b -> c` を `([a, b], c)` に分解するユーティリティ。
コンストラクタ型を引数リストと結果型に分離するために使用。

### inferQuote に EMatch ケース追加

`inferQuote (EMatch _ _) = TVar "_match"`

## 4. `src/Spinor/Expander.hs`

### expand に EMatch ケース追加

```haskell
expand (EMatch target branches) = do
  target' <- expand target
  branches' <- mapM (\(pat, body) -> do { body' <- expand body; pure (pat, body') }) branches
  pure $ EMatch target' branches'
```

target と各 body を再帰展開。パターン自体は展開不要。

## 5. テスト

### `test/Spinor/ParserSpec.hs` (3件追加)

- `(match x ((Just v) (+ v 1)) (Nothing 0))` のパース検証
- ワイルドカードパターン `_` のパース検証
- リテラルパターンのパース検証

### `test/Spinor/EvalSpec.hs` (5件追加)

- Maybe のマッチ: `(Just 42)` → `(+ v 1)` → `43`
- 0引数コンストラクタマッチ: `Nothing` → `0`
- ワイルドカードとリテラル: `(match 10 (0 "zero") (_ "non-zero"))` → `"non-zero"`
- リテラルマッチ: `(match 0 (0 "zero") (_ "non-zero"))` → `"zero"`
- ネストしたパターン: `(MyCons x xs)` マッチ → `1`

## 検証結果

```
54 examples, 0 failures
Test suite spinor-test: PASS
1 of 1 test suites (1 of 1 test cases) passed.
```

全54テスト通過。
