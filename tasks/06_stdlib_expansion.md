# タスク: ステップ6 - Twister ライブラリの拡張 (リスト操作と数学)

## 現在の状況

* `load` 機能により `twister/boot.spin` が読み込まれている。
* `core.spin`, `list.spin` は最小限の状態。

## 目標

Twister (標準ライブラリ) を拡張し、より高度なリスト操作と計算を行えるようにする。

## 実装詳細指示

### 1. `src/Spinor/Primitive.hs` (カーネル拡張)

* 算術演算子に **剰余 (`%`)** を追加してください。
    * `VInt` 同士の演算。
    * Haskell の `mod` または `rem` を使用。

### 2. `twister/list.spin` (拡張・上書き)

以下の関数を追加・実装してください。既存の `map` 等も保持すること。

* `(def length (fn (xs) (if (null? xs) 0 (+ 1 (length (cdr xs))))))`
* `(def append (fn (xs ys) (if (null? xs) ys (cons (car xs) (append (cdr xs) ys)))))`
* `(def reverse (fn (xs) (foldl (fn (acc x) (cons x acc)) (quote ()) xs)))`
* `(def foldl (fn (f acc xs) (if (null? xs) acc (foldl f (f acc (car xs)) (cdr xs)))))`
* `(def foldr (fn (f acc xs) (if (null? xs) acc (f (car xs) (foldr f acc (cdr xs))))))`

### 3. `twister/math.spin` (新規作成)

* `(def even? (fn (n) (= (% n 2) 0)))`
* `(def odd? (fn (n) (not (even? n))))`
* `(def fact (fn (n) (if (= n 0) 1 (* n (fact (- n 1))))))`
* `(def fib (fn (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))))`

### 4. `twister/boot.spin` (修正)

* 新しく作成した `math.spin` もロードするように行を追加してください。

## 確認事項 (REPL)

実装後、REPL で以下が動くことを確認してください。
* `(reverse '(1 2 3))` -> `(3 2 1)`
* `(even? 4)` -> `#t`
* `(fib 10)` -> `55`

## 出力要件

* `twister/` 配下の変更された各ファイルの内容。
* Haskell 側の変更点。
* 簡単な動作確認ログ。

# 実装方針

## 概要

Twister ライブラリを拡張し、高度なリスト操作 (`foldl`, `foldr`, `reverse`, `length`, `append`) と数学関数 (`even?`, `odd?`, `fact`, `fib`) を追加する。カーネル側には剰余演算 `%` と quote 略記 `'` を追加。

## 設計判断

### `%` プリミティブ

既存の `numBinOp` ヘルパーを再利用し `mod` をラップ。最小限の変更で追加可能。

### quote 略記 `'`

`pQuote` パーサーで `'expr` を `EList [ESym "quote", expr]` に変換。Lisp の基本的なリーダーマクロとして実装。`isSymChar` から `'` と `"` をシンボル文字から除外する。

### Twister 関数の定義順序

依存関係を考慮: `foldl` → `foldr` → `reverse` の順で定義（`reverse` が `foldl` に依存するため）。

### math.spin の分離

数学関数は `list.spin` とは概念的に異なるため、別ファイル `math.spin` として分離。`boot.spin` に追加ロードを記述。

## 変更の流れ

1. `src/Spinor/Primitive.hs` (修正) — `%` プリミティブ追加
2. `src/Spinor/Syntax.hs` (修正) — `pQuote` パーサー追加、`isSymChar` 修正
3. `twister/list.spin` (修正) — `length`, `append`, `foldl`, `foldr`, `reverse` 追加
4. `twister/math.spin` (新規) — `even?`, `odd?`, `fact`, `fib`
5. `twister/boot.spin` (修正) — `math.spin` のロード追加

# 実装内容

## 変更・新規ファイル

| ファイル | 操作 | 概要 |
|---|---|---|
| `src/Spinor/Primitive.hs` | 修正 | 剰余演算 `%` (`mod`) プリミティブを追加 |
| `src/Spinor/Syntax.hs` | 修正 | quote 略記 `'expr` → `(quote expr)` パーサー (`pQuote`) 追加、`isSymChar` から `'` `"` を除外 |
| `twister/list.spin` | 修正 | `length`, `append`, `foldl`, `foldr`, `reverse` を追加。`map` の `(quote ())` を `'()` に簡略化 |
| `twister/math.spin` | 新規 | `even?`, `odd?`, `fact`, `fib` 関数定義 |
| `twister/boot.spin` | 修正 | `math.spin` のロードを追加 |

## 設計メモ

- **`%` プリミティブ**: 既存の `numBinOp` ヘルパーを再利用し `mod` をラップ。1行追加で実装完了
- **quote 略記 `'`**: `pQuote` パーサーで `'expr` を `EList [ESym "quote", expr]` に変換。Lisp の基本的なリーダーマクロとして実装
- **`isSymChar` の修正**: `'` と `"` をシンボル文字から除外。`'` は quote 略記、`"` は文字列リテラルの開始文字として予約
- **`list.spin` の定義順序**: `foldl` → `foldr` → `reverse` の順で定義。`reverse` が `foldl` に依存するため

## テスト結果

```
$ cabal run spinor
Spinor REPL (step5)
Loading Twister environment...
Twister loaded.
spinor> (reverse '(1 2 3))         => (3 2 1)
spinor> (even? 4)                  => #t
spinor> (fib 10)                   => 55
spinor> (length '(a b c d e))      => 5
spinor> (append '(1 2) '(3 4))     => (1 2 3 4)
spinor> (foldl + 0 '(1 2 3 4 5))   => 15
spinor> (foldr cons '() '(1 2 3))  => (1 2 3)
spinor> (odd? 7)                   => #t
spinor> (even? 7)                  => #f
spinor> (fact 10)                  => 3628800
spinor> (% 17 5)                   => 2
```
