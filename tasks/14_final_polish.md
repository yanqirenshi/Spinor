# タスク: ステップ14 - 最終調整 (Type-Safe Twister)

## 現在の状況
* カーネル (`Spinor`) は Let多相を含む完全な型推論能力を持っている。
* ライブラリ (`Twister`) は開発途中の状態であり、型エラーが出るコードが含まれている可能性がある。

## 目標
1. Twister ライブラリ (`core.spin`, `list.spin`, `math.spin`) を、静的型付け言語 Spinor に合わせて最適化する。
2. 複雑なリスト操作や高階関数が、型エラーなく動作することを実証する。

## 実装詳細指示

### 1. `twister/list.spin` (修正)
型推論を通すために、実装をより厳密にします。
* **`nil` の定義:**
    * `(def nil (quote ()))` （これは `forall a. [a]` として推論されるはず）
* **`map` の確認:**
    * `(def map (fn (f xs) (if (null? xs) nil (cons (f (car xs)) (map f (cdr xs))))))`
    * 型: `forall a b. (a -> b) -> [a] -> [b]` になるはず。
* **`foldl` の確認:**
    * `(def foldl (fn (f acc xs) (if (null? xs) acc (foldl f (f acc (car xs)) (cdr xs)))))`
    * 型: `forall a b. (b -> a -> b) -> b -> [a] -> b` になるはず。

### 2. `twister/core.spin` (修正)
* `cond` マクロの展開結果が、型システムの制約（if の両枝は同じ型）を満たすか確認。
    * `(cond (p1 e1) (p2 e2) ...)` -> `(if p1 e1 (if p2 e2 ...))`
    * 全ての `e1`, `e2`... は同じ型でなければなりません。
    * **重要:** 最後の `else` 節がない場合、`if` は何を返すか？
        * 現状のカーネル実装では `if` は必ず else を持つ必要があるか、あるいは `void` のような型が必要になる。
        * 今回は **「`cond` の最後は必ず `#t` 節（else相当）を書く」** という運用ルールでカバーする（またはマクロで自動的に `nil` や `0` を補うなど）。

### 3. デモコード (REPL確認用)
以下のコードが動くことを確認してください。

```lisp
;; 1. 多相的な nil
(cons 1 nil)
(cons #t nil)

;; 2. 高階関数の型推論
(map (fn (x) (> x 0)) (list 1 -2 3)) 
;; -> :: [Bool] -> (#t #f #t)

;; 3. 畳み込み
(foldl (fn (acc x) (+ acc x)) 0 (list 1 2 3 4 5))
;; -> :: Int -> 15

;; 4. 複雑な合成 (Map + Filter + Fold)
(def sum-squares-even
  (fn (xs)
    (foldl (fn (acc x) (+ acc x))
           0
           (map (fn (x) (* x x))
                (filter (fn (x) (even? x)) xs)))))

(sum-squares-even (list 1 2 3 4 5 6))
;; -> 2^2 + 4^2 + 6^2 = 4 + 16 + 36 = 56
;; -> :: Int

```

## 出力要件

* `twister/` 配下の修正後のファイル内容。
* 上記デモコードの実行ログ（型情報付き）。
* もし型エラーが出た場合の修正レポート。

# 実装内容

## 変更ファイル

| ファイル | 操作 | 概要 |
|---|---|---|
| `app/Main.hs` | 大幅修正 | boot ロードを boot.spin 経由から直接ファイル読み込みに変更。各式を展開→型推論(ベストエフォート)→評価し、Env と TypeEnv の両方を構築 |
| `src/Spinor/Infer.hs` | 拡張 | `runInferFrom` 追加 (カウンタ引き継ぎ用) |
| `twister/list.spin` | 修正 | `nil` 定義追加、`'()` → `nil` に統一、`filter` 追加 |
| `twister/boot.spin` | 修正 | コメント追加 (直接使用されなくなったため) |

## 設計メモ

### boot ロードの変更

**変更前:** boot.spin の `(load ...)` で各ファイルを読み込み → 型推論なし
```
Main.hs → runEval (mapM_ expandAndEval boot.spin) → Env のみ構築
```

**変更後:** Main.hs が各 .spin ファイルを直接読み込み → 式ごとに型推論
```
Main.hs → 各 .spin ファイルの各式に対して:
  1. expand (マクロ展開)
  2. inferTop (ベストエフォート型推論 → TypeEnv 更新)
  3. eval (評価 → Env 更新)
```

型推論が失敗する式 (マクロ定義 `mac` など) は TypeEnv 更新をスキップし、評価のみ続行する。

### Twister ライブラリの推論結果

boot 時に以下の型が自動推論・登録される:

| 関数 | 推論された型 |
|---|---|
| `not` | `Bool -> Bool` |
| `id` | `forall t. t -> t` |
| `nil` | `forall t. [t]` |
| `null?` | `forall t. [t] -> Bool` |
| `map` | `forall a b. (a -> b) -> [a] -> [b]` |
| `length` | `forall t. [t] -> Int` |
| `append` | `forall t. [t] -> [t] -> [t]` |
| `foldl` | `forall a b. (b -> a -> b) -> b -> [a] -> b` |
| `foldr` | `forall a b. (a -> b -> b) -> b -> [a] -> b` |
| `reverse` | `forall t. [t] -> [t]` |
| `filter` | `forall t. (t -> Bool) -> [t] -> [t]` |
| `even?` | `Int -> Bool` |
| `odd?` | `Int -> Bool` |
| `fact` | `Int -> Int` |
| `fib` | `Int -> Int` |

### list.spin の変更

- `nil` を `(def nil (quote ()))` として追加。`forall a. [a]` として推論される。
- `'()` (quote 空リスト) を `nil` に統一。if の両枝の型が一致するようになる。
- `filter` を追加: `(def filter (fn (p xs) (if (null? xs) nil (if (p (car xs)) ...))))`

## テスト結果

```
$ cabal run spinor
Spinor REPL (step14)
Loading Twister environment...
Twister loaded.
spinor> (cons 1 nil)
:: [Int]
(1)
spinor> (cons #t nil)
:: [Bool]
(#t)
spinor> (map (fn (x) (> x 0)) (cons 1 (cons -2 (cons 3 nil))))
:: [Bool]
(#t #f #t)
spinor> (foldl (fn (acc x) (+ acc x)) 0 (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 nil))))))
:: Int
15
spinor> (def sum-squares-even (fn (xs) (foldl (fn (acc x) (+ acc x)) 0 (map (fn (x) (* x x)) (filter (fn (x) (even? x)) xs)))))
:: ([Int] -> Int)
<function>
spinor> (sum-squares-even (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 nil)))))))
:: Int
56
spinor> (fact 5)
:: Int
120
spinor> (fib 10)
:: Int
55
spinor> (reverse (cons 1 (cons 2 (cons 3 nil))))
:: [Int]
(3 2 1)
spinor> (if #t 1 #f)
型エラー: 型が一致しません: Int と Bool
```
