# タスク: ステップ4 - プリミティブ関数、リスト操作、そして再帰

## 現在の状況

* `fn` (lambda) と変数定義は動いている。
* まだ比較演算子やリスト操作関数がないため、実用的なプログラムが書けない。
* 再帰呼び出し（自分自身の参照）が動かない可能性がある。

## 目標

**「Lisp としてコードが書ける状態」** にする。

1. 比較演算子 (`=`, `<`, `>`) を実装する。
2. リスト操作 (`cons`, `car`, `cdr`, `quote`) を実装する。
3. 再帰呼び出しが動作するように変数参照ロジックを調整する。

## 実装詳細指示

### 1. `src/Spinor/Primitive.hs` (拡張)

以下のプリミティブ関数を追加して `primitiveBindings` に登録してください。

* **比較:**
    * `=`: `VInt` 同士、または `VBool` 同士の比較。戻り値は `VBool`。
    * `<`: `VInt` の比較。
    * `>`: `VInt` の比較。
* **リスト操作:**
    * `cons`: `val` と `list` を受け取り、新しいリストを作る。
    * `car`: リストの先頭要素を返す (空リストならエラー)。
    * `cdr`: リストの先頭以外を返す (空リストならエラー)。
    * `list`: 引数をそのままリスト (`VList`) として返す。
    * `empty?` (または `null?`): リストが空なら `#t`、それ以外なら `#f`。

### 2. `src/Spinor/Eval.hs` (修正)

* **特殊形式 `quote` の実装:**
    * `eval (EList [ESym "quote", expr])`: `expr` を評価せずに、そのまま `Val` に変換して返す。
        * AST (`Expr`) から `Val` への変換ヘルパー関数 (`exprToVal`) が必要になります。
* **変数参照 (`lookup`) のロジック変更 (再帰対応):**
    * 現状の `eval (ESym name)` は、おそらくクロージャ環境のみ、または現在の環境のみを見ている可能性があります。
    * **再帰を可能にするための探索順序:**
        1. ローカルスコープ（関数適用時に拡張された環境）。
        2. クロージャ環境（`VFunc` が持っている環境）。
        3. **グローバル環境（現在の `State` が持っている環境）** ← これが重要！
    * ※ 実装方針: `VFunc` にグローバル環境全体を持たせるのは重いため、`eval` 実行時に「見つからなかったら現在の State (`get`) から探す」というフォールバック処理を入れてください。

## 確認用コード (REPL)

実装後、以下の再帰コードが動くことを確認してください。

```lisp
(define fact (fn (n)
  (if (= n 0)
      1
      (* n (fact (- n 1))))))

(fact 5) ; -> 120
```

# 実装方針

## 概要

比較演算子、リスト操作、`quote` を実装し、再帰呼び出しを可能にする。これにより「Lisp としてコードが書ける状態」にする。

## 設計判断

### VSym の追加

`quote` でシンボルを第一級値として返すために `VSym Text` を `Val` に追加。Lisp として自然な設計であり、マクロ実装の基盤にもなる。

### exprToVal ヘルパー

`quote` の実装に必要な `exprToVal :: Expr -> Val` を追加。AST (`Expr`) を評価せずに値 (`Val`) に変換する。

### 再帰対応

`apply` 内の環境構築を `Map.union bindings closureEnv` から `Map.union bindings (Map.union closureEnv savedEnv)` に変更。グローバル環境をフォールバックとして参照可能にすることで、再帰呼び出し時に自分自身を環境から発見できるようにする。

### リスト操作プリミティブ

`cons`, `car`, `cdr`, `list`, `null?`/`empty?` を `VPrim` として実装。全て純粋関数であり `Eval` モナドに依存しない。

## 変更の流れ

1. `src/Spinor/Val.hs` (修正) — `VSym Text` 追加
2. `src/Spinor/Primitive.hs` (修正) — 比較演算拡張、リスト操作プリミティブ追加
3. `src/Spinor/Eval.hs` (修正) — `quote` 特殊形式、`exprToVal`、`apply` の環境マージ修正

# 実装内容

## 変更ファイル

| ファイル | 操作 | 概要 |
|---|---|---|
| `src/Spinor/Val.hs` | 修正 | `VSym Text` コンストラクタ追加。`showVal` に VSym の表示を追加 |
| `src/Spinor/Primitive.hs` | 修正 | `numEq` を VBool 対応に拡張。`cons`/`car`/`cdr`/`list`/`null?` プリミティブを追加 |
| `src/Spinor/Eval.hs` | 修正 | `quote` 特殊形式追加、`exprToVal` ヘルパー追加、`apply` の環境マージで再帰対応 |

## 設計メモ

- `quote` でシンボルを第一級値として返すため `VSym Text` を `Val` に追加（案A: 最も Lisp として自然）
- `exprToVal :: Expr -> Val` で AST を評価せずに値に変換する
- 再帰対応: `apply` 内の環境構築を `Map.union bindings closureEnv` → `Map.union bindings (Map.union closureEnv savedEnv)` に変更。グローバル環境をフォールバックとして参照可能にした
- `=` 演算子を VInt のみ → VInt/VBool 両対応に拡張

## テスト結果

```
spinor> (cons 1 (list 2 3))   => (1 2 3)
spinor> (car (list 1 2 3))    => 1
spinor> (cdr (list 1 2 3))    => (2 3)
spinor> (null? (list))         => #t
spinor> (null? (list 1))       => #f
spinor> (quote (1 2 3))        => (1 2 3)
spinor> (quote hello)          => "hello"
spinor> (= #t #t)              => #t
spinor> (= #t #f)              => #f
spinor> (define fact (fn (n) (if (= n 0) 1 (* n (fact (- n 1))))))  => <function>
spinor> (fact 5)               => 120
```
