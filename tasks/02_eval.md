# タスク: ステップ2 - 評価器 (Evaluator) と 環境 (Env) の実装

## 現在の状況

* `src/Spinor/Syntax.hs`: AST (`Expr`) とパーサーが実装済み。
* `app/Main.hs`: パース結果を表示するだけの REPL が動作中。

## 目標

**「計算ができる Lisp」** にすること。
1. 評価結果を表す型 (`Val`) と、変数を保持する環境 (`Env`) を定義する。
2. 式を評価する `eval` 関数を実装する。
3. REPL で `(+ 1 2)` や `(define x 10)` が動くようにする。

## 実装詳細指示

### 1. `src/Spinor/Val.hs` (新規作成)

`Expr` は「構文」ですが、計算結果には「値」が必要です。
* 必要なモジュール: `src/Spinor/Syntax.hs` をインポート。
* **データ型 `Val`:**
    * `VInt Integer`
    * `VBool Bool`
    * `VPrim (Text -> [Val] -> Eval Val)`  <-- **重要:** プリミティブ関数（`+`, `-` 等）をラップする値。
    * `VList [Val]` (リスト)
    * `VNil` (空リスト/False相当)
    * (今回はまだ Lambda/UserFunc は実装しなくて良いです)

### 2. `src/Spinor/Eval.hs` (新規作成)

評価ロジックのコア部分です。
* **モナドスタック:**
    * 状態（変数）とエラーハンドリングが必要です。
    * `type Env = Map.Map Text Val`
    * `newtype Eval a = Eval { runEval :: StateT Env (ExceptT Text IO) a }`
    * `DerivingVia` や `GeneralizedNewtypeDeriving` を使って `MonadState`, `MonadError`, `MonadIO` を導出すると綺麗です。
* **`eval :: Expr -> Eval Val`:**
    * `EInt`, `EBool`: 対応する `Val` に変換。
    * `ESym`: `Env` から検索 (`lookup`)。見つからなければエラー。
    * `EList (x:xs)`:
        1. 特殊形式 `define` のチェック: `(define sym expr)` なら `expr` を評価して `Env` を更新。
        2. 特殊形式 `if` のチェック。
        3. 関数適用: 先頭 `x` を評価して `VPrim f` を取り出し、残りの `xs` を評価してから `f` に適用する。

### 3. `src/Spinor/Primitive.hs` (新規作成 または Eval.hs に記述)

* 基本的な関数 (`+`, `-`, `*`) を定義。
* `primitiveBindings :: Env` を作成し、初期状態で `+` などが使えるようにする。

### 4. `app/Main.hs` (修正)

* `eval` 関数を使って、入力を計算結果 (`Val`) に変換して表示するようにループを変更。
* `runEval` でモナドを実行し、`Left err` ならエラー表示、`Right val` なら結果表示。

## 制約

* 既存の `src/Spinor/Syntax.hs` を破壊しないこと。
* エラー処理を適切に行うこと（未知のシンボル参照など）。
* 日本語で、各ファイルのコード全体を提示してください。


# 実装方針

## 概要

パーサーの上に評価器を構築し、「計算ができる Lisp」にする。値の型 (`Val`)、変数環境 (`Env`)、評価モナド (`Eval`)、プリミティブ関数を実装する。

## 設計判断

### Val と Expr の分離

`Expr` (構文) と `Val` (値) を分離することで、評価前後の区別を型レベルで保証する。Lisp の多くの実装では構文と値を同一型にするが、Spinor は静的型付けを目指すため早い段階で分離する。

### Eval モナドの設計

`newtype Eval a = Eval (StateT Env (ExceptT Text IO) a)` とする:
- `StateT Env` — 変数環境の読み書き（`define` で環境を更新）
- `ExceptT Text` — エラーハンドリング（未定義シンボル参照など）
- `IO` — 将来の拡張用（ファイル読み込み等）

`GeneralizedNewtypeDeriving` で `MonadState`, `MonadError`, `MonadIO` を自動導出。

### VPrim の型設計

`VPrim Text ([Val] -> Either Text Val)` とし、純粋な関数として定義する。`Eval` モナドに依存しないことで、`Val.hs` と `Eval.hs` の循環参照を回避。

### 特殊形式の判別

`eval (EList (x:xs))` 内でパターンマッチにより `define`, `if` を判別。関数適用は先頭を eval して `VPrim` を得てから残りの引数を eval して適用する。

## 変更の流れ

1. `src/Spinor/Val.hs` (新規) — `Val` 型と `Env` 型エイリアスを定義
2. `src/Spinor/Eval.hs` (新規) — `Eval` モナド、`eval` 関数、`define`/`if` 特殊形式
3. `src/Spinor/Primitive.hs` (新規) — `+`, `-`, `*`, `=`, `<`, `>` プリミティブと `primitiveBindings`
4. `app/Main.hs` (修正) — REPL を eval ベースに変更、環境を引き回す

# 実装結果

## 追加・変更ファイル

| ファイル | 操作 | 概要 |
|---|---|---|
| `src/Spinor/Val.hs` | 新規 | 値型 `Val` (`VInt`, `VBool`, `VPrim`, `VList`, `VNil`) と `Show` インスタンス |
| `src/Spinor/Eval.hs` | 新規 | `Eval` モナド (`StateT Env (ExceptT Text IO)`)、`eval` 関数、`define`/`if` 特殊形式 |
| `src/Spinor/Primitive.hs` | 新規 | プリミティブ関数 (`+`, `-`, `*`, `=`, `<`, `>`) と `primitiveBindings` |
| `app/Main.hs` | 修正 | REPL を eval ベースに変更。環境を引き回して `define` が永続するように |
| `spinor.cabal` | 修正 | 新モジュール追加、`containers` / `mtl` 依存追加 |

## 設計メモ

- `VPrim` の型は `Text ([Val] -> Either Text Val)` とし、純粋な関数として定義。Eval モナドに依存しないため循環参照を回避。
- `Eval` モナドの `runEval` は `Env -> Eval a -> IO (Either Text (a, Env))` で、REPL 側で `env'` を次のループに渡すことで `define` の状態を維持。
- `Syntax.hs` は一切変更なし。

## テスト結果

```
spinor> 42          => 42
spinor> #t          => #t
spinor> (+ 1 2)     => 3
spinor> (* 3 (+ 4 5)) => 27
spinor> (define x 10)  => 10
spinor> x              => 10
spinor> (+ x 20)       => 30
spinor> (if #t 1 2)    => 1
spinor> (if #f 1 2)    => 2
spinor> (= 3 3)        => #t
spinor> (< 1 5)        => #t
spinor> (> 10 3)       => #t
```
