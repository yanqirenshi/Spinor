# タスク: ステップ7 - ユーザー定義マクロ (mac)

## 目標
Lisp の真髄である「マクロ」を実装し、制御構文 (`let`, `and`, `when`) をユーザーランド (Twister) で定義できるようにする。

## 実装詳細指示

### 1. `src/Spinor/Val.hs` (拡張)
* `Val` 型に `VMacro` コンストラクタを追加。
    * 構造は `VFunc` とほぼ同じです（引数名、本体、環境）。
    * `VMacro [Text] Expr Env`

### 2. `src/Spinor/Eval.hs` (修正)
* **特殊形式 `mac` (defmacro) の実装:**
    * `(mac (name args...) body)` という構文をパースする。
    * `fn` と同様にクロージャを作成するが、`VMacro` として環境に登録する。
* **評価ロジック (`eval`) の拡張:**
    * リスト評価時 (`eval (EList (x:xs))`) の分岐を変更:
        1. `x` (関数名) を環境からルックアップする。
        2. もし **`VMacro`** だったら:
            * 引数 `xs` を **評価せずに** マクロ関数に適用する。
            * 返ってきた結果 (`Val` 型の S式相当) を、**AST (`Expr`) に逆変換** する。
            * その AST を再度 `eval` する。
        3. もし `VFunc` / `VPrim` だったら:
            * 引数 `xs` を **評価してから** 適用する（既存の動作）。

### 3. `src/Spinor/Syntax.hs` (拡張)
* `valToExpr :: Val -> Expr` 関数が必要になります。
    * マクロの実行結果 (`Val`) を、再評価のために `Expr` に戻す必要があるため。
    * `VList` -> `EList`, `VInt` -> `EInt` などの逆変換。

### 4. `twister/core.spin` (拡張)
マクロを使って、以下の制御構文を定義してください。
(バッククォートがないため、`list`, `cons`, `quote` を駆使して実装します)

* **`(def when (mac (cond body) (list 'if cond body #f)))`**
* **`(def let (mac (var val body) (list (list 'fn (list var) body) val)))`**
    * ※簡易版 `let`: `(let x 1 body)` のような 1変数のみ対応で構いません。

## 確認事項 (REPL)
実装後、以下が動くことを確認してください。
* `(when #t 42)` -> `42`
* `(let x 10 (* x x))` -> `100`

## 出力要件
* Haskell 側の変更点 (`Eval.hs`, `Val.hs`, `Syntax.hs`)。
* `twister/core.spin` の追加コード。
* マクロ展開のロジック（評価せずに適用 -> 結果を再評価）が正しく実装されているか解説してください。

# 実装内容

## 変更ファイル

| ファイル | 操作 | 概要 |
|---|---|---|
| `src/Spinor/Val.hs` | 修正 | `VMacro [Text] Expr Env` コンストラクタ追加、`showVal` に `<macro>` 表示追加 |
| `src/Spinor/Eval.hs` | 修正 | `mac` 特殊形式、マクロ展開ロジック、`valToExpr` ヘルパー追加、`apply` を `applyClosureBody` で共通化、`extractSym` をトップレベルに移動 |
| `twister/core.spin` | 修正 | `when` / `let` マクロを追加 |

## 設計メモ

### マクロ展開のフロー

`(when #t 42)` を例に説明:

1. **`eval (EList (x:xs))`** で `x` = `ESym "when"`, `xs` = `[EBool True, EInt 42]`
2. `eval x` → 環境から `when` を検索 → `VMacro ["cond", "body"] (list 'if cond body #f) env`
3. **VMacro と判定** → 引数を **評価せず** `exprToVal` で Val に変換: `[VBool True, VInt 42]`
4. `apply` でマクロ本体を評価: `(list 'if cond body #f)` → `VList [VSym "if", VBool True, VInt 42, VBool False]`
5. `valToExpr` で Expr に逆変換: `EList [ESym "if", EBool True, EInt 42, EBool False]`
6. 逆変換結果を **再 eval**: `(if #t 42 #f)` → `42`

### `valToExpr` の配置

タスクでは `Syntax.hs` に配置する指示だが、`Syntax.hs` → `Val.hs` の循環依存を避けるため `Eval.hs` に配置（`exprToVal` と対になるヘルパーとして）。

### `applyClosureBody` の共通化

`VFunc` と `VMacro` の適用ロジック（引数束縛→本体評価→環境復元）は同一のため、`applyClosureBody` に抽出して共有。

## テスト結果

```
$ cabal run spinor
Spinor REPL (step5)
Loading Twister environment...
Twister loaded.
spinor> (when #t 42)            => 42
spinor> (when #f 42)            => #f
spinor> (let x 10 (* x x))     => 100
spinor> (let name 5 (+ name 3)) => 8
```
