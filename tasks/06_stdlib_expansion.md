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

# 実装内容
