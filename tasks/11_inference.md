# タスク: ステップ11 - 型推論の実装 (Inference / Algorithm W)

## 現在の状況

* 型の定義 (`Type`) と単一化 (`unify`) は `Infer.hs` に実装済み。
* AST (`Expr`) は `Syntax.hs` にある。

## 目標

**AST を走査して型を決定する `infer` 関数を実装し、REPL で型を表示できるようにする。**

## 実装詳細指示

### 1. `src/Spinor/Infer.hs` (拡張)

型推論のメインロジックを追加します。

* **型環境 (`TypeEnv`):**
    * `type TypeEnv = Map.Map Text Scheme`
    * 変数名と、その型スキームのマッピング。
* **Infer モナド:**
    * 型推論には「新しい型変数の生成 (`Supply`)」と「エラー処理 (`Except`)」が必要です。
    * `newtype Infer a = Infer { runInfer :: StateT Int (Either Text) a }` (または類似の構成)
    * `fresh :: Infer Type` (新しい型変数 `t0`, `t1`... を返す) を実装。
* **`infer` 関数の実装:**
    * `infer :: TypeEnv -> Expr -> Infer (Subst, Type)`
    * **ロジック:**
        * `EInt _`: `(nullSubst, TInt)` を返す。
        * `EBool _`: `(nullSubst, TBool)` を返す。
        * `ESym x`: `TypeEnv` から検索し、`instantiate` (型変数をフレッシュにする) して返す。
        * `EList [ESym "if", cond, thn, els]`:
            * `cond` を推論し、`TBool` と単一化 (`unify`) する。
            * `thn` と `els` を推論し、それらの型同士を単一化する。
        * `EList [ESym "fn", EList args, body]`:
            * 引数 (`args`) ごとに `fresh` で新しい型変数を生成する。
            * それらを `TypeEnv` に追加して `body` を推論する。
            * 結果は `TArr` (関数型) になる。
        * `EList (func : args)` (関数適用):
            * `func` を推論する -> `tFunc`
            * `args` を順に推論する -> `tArgs`
            * 戻り値用の新しい型変数 `tRet` を作る。
            * `tFunc` と `tArgs -> tRet` を単一化する。
* **ヘルパー関数:**
    * `instantiate`: `Scheme` 内の量子化変数 (`forall a.`) を具体的なフレッシュ型変数に置き換える。
    * `generalize`: 環境に含まれない自由な型変数を `Scheme` (`forall`) に昇格させる。

### 2. `src/Spinor/Type.hs` (調整)

* 必要であれば `TypeEnv` などの定義をこちらに移しても良い（循環参照回避のため）。
* プリミティブ関数の型を定義する準備（次のステップで使用）。

### 3. `app/Main.hs` (修正)

* **初期型環境 (`baseTypeEnv`) の作成:**
    * プリミティブ関数の型を登録しておく。
    * `+`, `-`, `*`: `Int -> Int -> Int`
    * `=`: `Int -> Int -> Bool` (簡単のため一旦 Int 同士のみとするか、ポリモーフィックにする)
    * `cons`: `forall a. a -> [a] -> [a]`
    * `car`: `forall a. [a] -> a`
    * `cdr`: `forall a. [a] -> [a]`
    * `null?`: `forall a. [a] -> Bool`
* **REPL ループの変更:**
    * `read` -> `expand` -> **`infer`** -> `eval`
    * 推論に成功したら、型を表示してから実行結果を表示する。
    * 推論に失敗したら、**実行せずに** 型エラーを表示する。

## 確認事項 (REPL)

実装後、以下のような挙動になることを確認してください。

```text
SPINOR> (+ 1 2)
:: Int
3

SPINOR> (cons 1 (quote ()))
:: [Int]
(1)

SPINOR> (if #t 1 #f)
Error: Type mismatch: Int vs Bool
```

## 出力要件

* `Infer.hs` の全コード（特に `infer` 関数のパターンマッチ）。
* `Main.hs` の `baseTypeEnv` 定義と REPL ループの変更点。
* 日本語での解説。

# 実装方針

## 概要

Algorithm W に基づく `infer` 関数を実装し、AST を走査して型を推論する。REPL のフローを `Read → Expand → Infer → Eval` に変更し、型を表示してから実行する。型エラー時は実行しない。

## 設計判断

### Infer モナド

`newtype Infer a = Infer (StateT Int (Either Text) a)` — IO 不要の純粋な計算。
- `StateT Int`: フレッシュ型変数のカウンタ (`t0`, `t1`, `t2`, ...)
- `Either Text`: 型エラーの報告

### infer 関数の設計

`infer :: TypeEnv -> Expr -> Infer (Subst, Type)` — AST を走査して (置換, 型) のペアを返す。各 AST ノードごとにパターンマッチで処理。

### 多引数の関数適用

Spinor の `(f a b c)` は型推論上はカリー化: `f :: a → b → c → ret`。引数を左から順に推論し、`tFunc` と `tArg1 → tArg2 → ... → tRet` を unify。

### instantiate / generalize

- `instantiate`: `Scheme` の量子化変数をフレッシュ型変数に置き換え（多相関数の呼び出しごとに独立した型変数）
- `generalize`: 環境に含まれない自由型変数を `forall` で量子化

### baseTypeEnv

プリミティブ関数の型を `TypeEnv` として定義。`+` は `Int → Int → Int`、`=` は `forall a. a → a → Bool`、`cons` は `forall a. a → [a] → [a]` 等。

### TypeEnv の分離

`TypeEnv` を `Type.hs` に定義し、循環参照を回避。

## 変更の流れ

1. `src/Spinor/Type.hs` (修正) — `TypeEnv` 型エイリアスと `showType` 追加
2. `src/Spinor/Infer.hs` (大幅拡張) — `Infer` モナド、`infer`、`instantiate`、`generalize`、`baseTypeEnv`
3. `app/Main.hs` (修正) — REPL フローを `Read → Expand → Infer → Eval` に変更

# 実装内容

## 変更・新規ファイル

| ファイル | 操作 | 概要 |
|---|---|---|
| `src/Spinor/Type.hs` | 修正 | `TypeEnv` 型エイリアスと `showType` 関数を追加 (循環参照回避のため `Infer.hs` から分離) |
| `src/Spinor/Infer.hs` | 大幅拡張 | `Infer` モナド、`infer` (Algorithm W)、`instantiate`、`generalize`、`baseTypeEnv` を追加 |
| `app/Main.hs` | 修正 | REPL フローを `Read → Expand → Infer → Eval` に変更。型推論成功時は型表示+実行、失敗時は型エラー表示のみ |

## 設計メモ

### Infer モナド

```haskell
newtype Infer a = Infer (StateT Int (Either Text) a)
  deriving (Functor, Applicative, Monad, MonadState Int, MonadError Text)
```

- `StateT Int`: フレッシュ型変数のカウンタ (`t0`, `t1`, `t2`, ...)
- `Either Text`: 型エラーの報告 (IO 不要 — 型推論は純粋な計算)

### infer 関数のパターンマッチ

`infer :: TypeEnv -> Expr -> Infer (Subst, Type)` は AST を走査して (置換, 型) のペアを返す。

| パターン | 推論ロジック |
|---|---|
| `EInt _` | `(∅, TInt)` |
| `EBool _` | `(∅, TBool)` |
| `EStr _` | `(∅, TStr)` |
| `ESym x` | TypeEnv から検索 → `instantiate` でフレッシュ化 |
| `EList []` | `(∅, TList α)` (α はフレッシュ) |
| `(quote expr)` | リテラルの型を静的に決定 |
| `(if cond thn els)` | cond を `TBool` と unify → thn と els の型を unify |
| `(def name body)` | フレッシュ型変数で再帰対応 → body を推論 → unify |
| `(fn (params) body)` | params にフレッシュ型変数割当 → body 推論 → `TArr` 構築 |
| `(fn param body)` | 全引数キャプチャ: `TList α → body型` |
| `(func args...)` | func 推論 → args 順次推論 → `arg1 → arg2 → ... → ret` と unify |

### 多引数の関数適用

Spinor の関数適用 `(f a b c)` は、型推論上はカリー化された関数 `f :: a → b → c → ret` として扱う。

1. `f` を推論 → `tFunc`
2. 引数を左から順に推論し、置換を累積 → `[tArg1, tArg2, ...]`
3. フレッシュな戻り値型 `tRet` を生成
4. `tFunc` と `tArg1 → tArg2 → ... → tRet` を unify
5. 結果の型は `apply sFinal tRet`

### instantiate / generalize

- **`instantiate`**: `Scheme` の量子化された変数をフレッシュ型変数に置き換える。多相関数を呼び出すたびに独立した型変数で具体化するために必要。
  - 例: `forall a. a -> a` → `t0 -> t0` (呼び出しごとに異なる `t0`)
- **`generalize`**: 環境に含まれない自由型変数を `forall` で量子化して `Scheme` にする。
  - 例: 環境 `{}`, 型 `a -> a` → `forall a. a -> a`

### baseTypeEnv (プリミティブの型)

| 関数 | 型 |
|---|---|
| `+`, `-`, `*`, `%` | `Int -> Int -> Int` |
| `<`, `>` | `Int -> Int -> Bool` |
| `=` | `forall a. a -> a -> Bool` |
| `cons` | `forall a. a -> [a] -> [a]` |
| `car` | `forall a. [a] -> a` |
| `cdr` | `forall a. [a] -> [a]` |
| `null?`, `empty?` | `forall a. [a] -> Bool` |
| `print` | `forall a. a -> a` |

### REPL フロー

```
Read → Expand → Infer → Eval
                  ↓
           成功: 型表示 → 評価実行
           失敗: 型エラー表示 (実行しない)
```

## テスト結果

```
$ cabal run spinor
Spinor REPL (step11)
Loading Twister environment...
Twister loaded.
spinor> (+ 1 2)
:: Int
3
spinor> (cons 1 (quote ()))
:: [Int]
(1)
spinor> (if #t 1 #f)
型エラー: 型が一致しません: Int と Bool
spinor> (fn (x) (+ x 1))
:: (Int -> Int)
<function>
spinor> (null? (quote ()))
:: Bool
#t
spinor> 42
:: Int
42
spinor> #t
:: Bool
#t
spinor> "hello"
:: Str
"hello"
spinor> (if #t 1 2)
:: Int
1
spinor> (let x 10 (* x x))
:: Int
100
```
