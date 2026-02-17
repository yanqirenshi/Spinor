# タスク: ステップ9 - マクロ展開フェーズの分離 (Static Typing への準備)

## 現在の状況

* マクロ展開は `eval` 関数の中で動的に行われている。
* これでは、実行前に型チェックを行うことができない。

## 目標

**「評価 (Eval)」と「展開 (Expand)」を分離する。**
1. 式を受け取り、マクロをすべて展開しきった純粋な AST を返す `expand` 関数を実装する。
2. REPL の処理フローを `Read -> Expand -> Eval` に変更する。

## 実装詳細指示

### 1. `src/Spinor/Expander.hs` (新規作成)

* `eval` 関数からマクロ展開ロジックを切り出します。
* `expand :: Expr -> Eval Expr`
    * `eval` と同様に `Eval` モナド内で動作します（環境からマクロ定義を探すため）。
    * **ロジック:**
        * アトム (Int, Bool, Sym): そのまま返す。
        * リスト `(x:xs)`:
            1. `x` がシンボルで、環境内で `VMacro` に束縛されている場合:
                * マクロ関数を適用して新しい式 (`Expr`) を生成する。
                * 生成された式に対して、**再帰的に `expand` を呼ぶ**（マクロがマクロを生成する場合に対応）。
            2. それ以外の場合 (関数呼び出しや特殊形式):
                * リストの要素すべてに対して再帰的に `expand` を適用する。
                * `mapM expand (x:xs)`

### 2. `src/Spinor/Eval.hs` (修正)

* `eval` 関数内の「マクロ展開ロジック」を削除する。
    * もはや `eval` に `VMacro` が渡ってくることはない（`expand` で処理済みのため）前提とする。
    * もし `eval` 中に `VMacro` に遭遇したらエラーを出しても良い。

### 3. `app/Main.hs` (修正)

* REPL のループ処理を変更する。
    * 変更前: `eval expr`
    * 変更後: `expanded <- expand expr` -> `eval expanded`

## 確認事項 (REPL)

実装後、既存のマクロ (`cond`, `when`, `let`) が今まで通り動作することを確認してください。
ユーザーから見え方は変わりませんが、内部では「展開済みのコード」が実行されるようになります。

## 出力要件

* `src/Spinor/Expander.hs` のコード全体。
* `Eval.hs` と `Main.hs` の変更点。
* 日本語での解説（特に `expand` の再帰ロジックについて）。

# 実装方針

## 概要

マクロ展開を `eval` から分離し、独立した `expand` フェーズとして実装する。REPL の処理フローを `Read → Expand → Eval` に変更。静的型付けへの準備として、展開済み AST に対して型チェックを行える状態を目指す。

## 設計判断

### expand の型

`expand :: Expr -> Eval Expr` — `Eval` モナド内で動作する。環境からマクロ定義を探す必要があるため、純粋な関数にはできない。

### expand の再帰ロジック

- アトム: そのまま返す
- `(quote expr)`: 内部を展開せず返す
- `(def name body)` / `(fn params body)`: body のみ再帰展開
- マクロ呼び出し: 未評価引数でマクロ適用 → 結果を Expr 逆変換 → **再帰展開**（マクロがマクロを生成する場合に対応）
- その他のリスト: `mapM expand` で全要素を再帰展開

### load の移動

`load` は expand + eval の両方が必要なため、`Eval.hs` に残すと循環依存が発生する。`Expander.hs` に移すことで、依存方向を `Expander → Eval` の一方向に統一する。

### expandAndEval ユーティリティ

`expandAndEval :: Expr -> Eval Val` を提供し、展開→評価のパイプラインを1関数で実行可能にする。`Main.hs` の REPL ループや boot ロードで使用。

## 変更の流れ

1. `src/Spinor/Expander.hs` (新規) — `expand`, `expandAndEval`、`load` 特殊形式
2. `src/Spinor/Eval.hs` (修正) — マクロ展開ロジック削除、`load` 削除、ヘルパー関数のエクスポート追加
3. `app/Main.hs` (修正) — REPL と boot ロードで `expandAndEval` を使用

# 実装内容

## 変更・新規ファイル

| ファイル | 操作 | 概要 |
|---|---|---|
| `src/Spinor/Expander.hs` | 新規 | `expand :: Expr -> Eval Expr` (マクロ展開) と `expandAndEval` を実装。`load` 特殊形式もここで処理 |
| `src/Spinor/Eval.hs` | 修正 | `applyClosureBody`, `exprToVal`, `valToExpr` をエクスポート。`eval (EList (x:xs))` からマクロ展開ロジックを削除、`load` 特殊形式を削除 |
| `app/Main.hs` | 修正 | REPL と boot loading で `expandAndEval` を使用。REPL バナーを step9 に更新 |
| `spinor.cabal` | 修正 | `Spinor.Expander` を exposed-modules に追加 |

## 設計メモ

### 処理フローの変更

```
変更前: Read → Eval (マクロ展開も内包)
変更後: Read → Expand → Eval (マクロ展開を分離)
```

### expand の再帰ロジック

`expand :: Expr -> Eval Expr` は以下のルールで再帰的にマクロを展開する:

| 入力 | 処理 |
|---|---|
| アトム (Int, Bool, Str, Sym) | そのまま返す |
| `(quote expr)` | 内部を展開せずそのまま返す |
| `(def name body)` / `(define name body)` | body のみ再帰展開 |
| `(fn params body)` / `(mac params body)` | body のみ再帰展開 (params は展開しない) |
| `(load "file")` | ファイルを読み込み、各式を expandAndEval (副作用あり) |
| `(macroName args...)` (マクロ呼び出し) | 引数を未評価のまま Val 変換 → マクロ本体を適用 → 結果を Expr 逆変換 → **再帰展開** |
| その他のリスト | 全要素に `mapM expand` |

### 循環依存の回避

`load` は expand + eval の両方が必要なため、`Eval.hs` に残すと循環依存が発生する。`load` を `Expander.hs` に移すことで、依存方向を `Expander → Eval` の一方向に統一した。

### マクロ本体の事前展開

`(def cond (mac args body))` の `body` は定義時に展開される。body 内の `let` マクロ呼び出しは展開フェーズでインライン化され、`cond` 実行時には `eval` がマクロを処理する必要がない。

## テスト結果

```
$ cabal run spinor
Spinor REPL (step9)
Loading Twister environment...
Twister loaded.
spinor> (when #t 42)                             => 42
spinor> (when #f 42)                             => #f
spinor> (let x 10 (* x x))                      => 100
spinor> (cond (#f 1) (#t 2) (#t 3))             => 2
spinor> (map (fn (x) (* x x)) '(1 2 3 4 5))    => (1 4 9 16 25)
spinor> (reverse '(a b c))                       => ("c" "b" "a")
spinor> (fib 10)                                 => 55
spinor> (fact 5)                                 => 120
spinor> ((fn (a . rest) rest) 1 2 3 4)           => (2 3 4)
spinor> (print "hello from step9")               => "hello from step9"
```
