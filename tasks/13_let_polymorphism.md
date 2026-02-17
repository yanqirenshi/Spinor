# タスク: ステップ13 - Let多相 (Let-Polymorphism) の実装

## 現在の状況

* トップレベルの `define` は多相化 (`generalize`) されている。
* しかし、`let` は `((fn (x) ...) val)` というラムダ式に展開されるマクロであるため、多相化されない（単相性制限）。
* 例: `(let f (fn (x) x) ...)` の中で `f` を `Int` と `Bool` 両方に使うとエラーになる。

## 目標

**`let` をマクロから「カーネルの特殊形式」に昇格させ、ローカル変数でも多相性を有効にする。**

## 実装詳細指示

### 1. `src/Spinor/Syntax.hs` (拡張)

* `Expr` 型に `ELet` コンストラクタを追加。
    * `ELet Text Expr Expr`  (変数名, 値の式, 本体の式)
* パーサー (`parseExpr`) を修正。
    * `(let name val body)` という形式をパースし、`ELet` を生成するように変更。

### 2. `src/Spinor/Infer.hs` (修正: 重要)

* `infer` 関数に `ELet` のパターンマッチを追加 (Algorithm W の核心)。
    ```haskell
    infer env (ELet name val body) = do
      (s1, t1) <- infer env val
      let env' = apply s1 env
      let t1'  = generalize env' t1  -- ここで多相化！
      let env'' = Map.insert name t1' env'
      (s2, t2) <- infer env'' body
      return (s1 `compose` s2, t2)
    ```
    ※ 上記は概念コードです。既存の `Infer` モナドや `Subst` の実装に合わせて調整してください。

### 3. `src/Spinor/Eval.hs` (修正)

* `eval` 関数に `ELet` の処理を追加。
    * 動作は `define` + `local` のようなイメージ。
    * `val` を評価し、その結果を環境に追加して `body` を評価する。

### 4. `src/Spinor/Expander.hs` (修正)

* `expand` 関数で `ELet` の中身（`val` と `body`）も再帰的に展開するように修正。

### 5. `twister/core.spin` (修正)

* 既存の `let` マクロの定義を削除する（カーネル側の構文と競合するため）。

## 確認事項 (REPL)

実装後、以下の「ローカル多相」コードが動作することを確認してください。

```lisp
SPINOR> (let f (fn (x) x)
          (if (f #t) (f 1) 0))
:: Int
1

```

※ マクロ版の `let` では、これは型エラーになっていました。

## 出力要件

* `Syntax.hs`, `Infer.hs`, `Eval.hs` の変更点。
* `twister/core.spin` の削除指示。
* 日本語での解説。

# 実装方針

## 概要

`let` をマクロからカーネルの特殊形式に昇格させ、ローカル変数でも多相性を有効にする (Let-polymorphism)。Algorithm W の核心部分の実装。

## 設計判断

### マクロから特殊形式への昇格理由

マクロ版 `let` は `((fn (x) ...) val)` に展開されるため、ラムダ引数は単相 (`Scheme []`) で束縛される。これでは `(let f (fn (x) x) ...)` の `f` を `Int` と `Bool` の両方に使うと型エラーになる。

カーネル版 `ELet` では、`val` の推論結果を `generalize` して多相型スキームに変換するため、`body` 内で異なる型に具体化できる。

### ELet コンストラクタ

`ELet Text Expr Expr` — 変数名、値の式、本体の式。パーサーで `(let name val body)` → `ELet` に変換。

### 型推論ロジック

```
1. val を推論 → (s1, t1)
2. apply s1 env → env'
3. generalize env' t1 → scheme (多相化)
4. Map.insert name scheme env' → env''
5. body を env'' で推論 → (s2, t2)
6. (composeSubst s2 s1, t2) を返す
```

### twister/core.spin の修正

既存の `let` マクロ定義を削除。カーネル側の構文と競合するため。

## 変更の流れ

1. `src/Spinor/Syntax.hs` (修正) — `ELet` コンストラクタ追加、パーサー修正
2. `src/Spinor/Infer.hs` (修正) — `infer` に `ELet` パターン追加
3. `src/Spinor/Eval.hs` (修正) — `eval` に `ELet` パターン追加、`exprToVal` 対応
4. `src/Spinor/Expander.hs` (修正) — `expand` に `ELet` パターン追加
5. `twister/core.spin` (修正) — `let` マクロ定義削除

# 実装内容

## 変更ファイル

| ファイル | 操作 | 概要 |
|---|---|---|
| `src/Spinor/Syntax.hs` | 修正 | `ELet Text Expr Expr` コンストラクタ追加。パーサーで `(let name val body)` → `ELet` 変換 |
| `src/Spinor/Infer.hs` | 修正 | `infer` に `ELet` パターン追加 (Algorithm W の let-polymorphism) |
| `src/Spinor/Eval.hs` | 修正 | `eval` に `ELet` パターン追加 (ローカル変数束縛+スコープ復元)、`exprToVal` に `ELet` 対応 |
| `src/Spinor/Expander.hs` | 修正 | `expand` に `ELet` パターン追加 (val と body を再帰展開) |
| `twister/core.spin` | 修正 | `let` マクロ定義を削除 (カーネル特殊形式に昇格のため) |

## 設計メモ

### let がマクロから特殊形式に昇格した理由

マクロ版 `let`:
```lisp
(def let (mac (var val body) (list (list 'fn (list var) body) val)))
```
これは `(let f (fn (x) x) ...)` を `((fn (f) ...) (fn (x) x))` に展開する。
型推論上、ラムダ引数は単相（`Scheme []`）で束縛されるため、`f` を異なる型で使うとエラーになる。

カーネル版 `ELet`:
```haskell
infer env (ELet name val body) = do
  (s1, t1) <- infer env val
  let env'   = apply s1 env
      scheme = generalize env' t1  -- ここで多相化
      env''  = Map.insert name scheme env'
  (s2, t2) <- infer env'' body
  pure (composeSubst s2 s1, t2)
```
`val` の推論結果を `generalize` して多相型スキーム (`Scheme`) に変換するため、`body` 内で異なる型に具体化できる。

### 型推論フロー例

```
(let f (fn (x) x) (if (f #t) (f 1) 0))

1. val = (fn (x) x) を推論 → t0 -> t0
2. generalize → forall t0. t0 -> t0
3. body 内の (f #t):
   - instantiate f → t1 -> t1 (フレッシュ)
   - #t :: Bool と unify → Bool -> Bool
4. body 内の (f 1):
   - instantiate f → t2 -> t2 (別のフレッシュ)
   - 1 :: Int と unify → Int -> Int
5. if の then/else: Int と Int → OK
6. 最終型: Int
```

### Syntax.hs のパーサー変更

`pList` でパース後、`[ESym "let", ESym name, val, body]` パターンに合致すれば `ELet name val body` に変換。
```haskell
pList = do
  xs <- between (lexeme (char '(')) (lexeme (char ')')) (many parseExpr)
  case xs of
    [ESym "let", ESym name, val, body] -> pure $ ELet name val body
    _ -> pure $ EList xs
```

### Eval.hs の let 評価

```haskell
eval (ELet name valExpr body) = do
  val <- eval valExpr
  savedEnv <- get
  modify (Map.insert name val)
  result <- eval body
  put savedEnv  -- レキシカルスコープ: 元の環境を復元
  pure result
```

## テスト結果

```
$ cabal run spinor
Spinor REPL (step12)
Loading Twister environment...
Twister loaded.
spinor> (let f (fn (x) x) (if (f #t) (f 1) 0))
:: Int
1
spinor> (let x 10 (* x x))
:: Int
100
spinor> (let id (fn (x) x) (id 42))
:: Int
42
spinor> (let id (fn (x) x) (id #t))
:: Bool
#t
spinor> (define double (fn (x) (+ x x)))
:: (Int -> Int)
<function>
spinor> (double 5)
:: Int
10
spinor> (+ 1 2)
:: Int
3
```
