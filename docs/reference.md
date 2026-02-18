# Spinor Language Reference

Spinor の言語仕様リファレンスです。

## Special Forms (特殊形式)

### `defun` / `define` — 関数定義

トップレベルで関数を定義します。

```lisp
(defun square (x) (* x x))

(defun fact (n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))
```

### `fn` — 無名関数 (Lambda)

無名関数を生成します。

```lisp
(fn (x) (* x x))

(map (fn (x) (+ x 1)) (list 1 2 3))
;; => (2 3 4)
```

### `if` — 条件分岐

```lisp
(if condition then-expr else-expr)

(if (> x 0) "positive" "non-positive")
```

### `let` — ローカル束縛 (Let多相)

ローカル変数を束縛します。Let多相により、束縛された変数は多相的に使用できます。

```lisp
(let id (fn (x) x)
  (if (id #t) (id 1) (id 0)))
;; id は Bool -> Bool としても Int -> Int としても使える
```

### `match` — パターンマッチング

代数的データ型に対するパターンマッチングを行います。

```lisp
(data Maybe a
  (Nothing)
  (Just a))

(defun fromMaybe (default m)
  (match m
    ((Nothing) default)
    ((Just x)  x)))
```

### `data` — 代数的データ型定義

新しいデータ型を定義します。

```lisp
(data Bool
  (True)
  (False))

(data List a
  (Nil)
  (Cons a (List a)))

(data Either a b
  (Left a)
  (Right b))
```

### `mac` — マクロ定義

ハイジニックマクロを定義します。マクロは評価前の展開フェーズで処理されます。

```lisp
(mac when (cond body)
  (list 'if cond body 'nil))

(when (> x 0)
  (print "positive"))
;; 展開結果: (if (> x 0) (print "positive") nil)
```

### `quote` — クォート

式を評価せずにそのまま返します。

```lisp
(quote (1 2 3))  ;; => (1 2 3)
'(1 2 3)         ;; 同上 (シンタックスシュガー)
```

## Primitive Functions (プリミティブ関数)

### 算術演算

| 関数 | 型 | 説明 |
|------|----|----|
| `+` | `Int -> Int -> Int` | 加算 |
| `-` | `Int -> Int -> Int` | 減算 |
| `*` | `Int -> Int -> Int` | 乗算 |
| `/` | `Int -> Int -> Int` | 整数除算 |
| `%` | `Int -> Int -> Int` | 剰余 |

### 比較演算

| 関数 | 型 | 説明 |
|------|----|----|
| `=` | `Int -> Int -> Bool` | 等価 |
| `<` | `Int -> Int -> Bool` | 小なり |
| `>` | `Int -> Int -> Bool` | 大なり |
| `<=` | `Int -> Int -> Bool` | 以下 |
| `>=` | `Int -> Int -> Bool` | 以上 |

### リスト操作

| 関数 | 型 | 説明 |
|------|----|----|
| `cons` | `a -> [a] -> [a]` | リストの先頭に要素を追加 |
| `car` | `[a] -> a` | リストの先頭要素を取得 |
| `cdr` | `[a] -> [a]` | リストの残りを取得 |
| `nil` | `[a]` | 空リスト |
| `null?` | `[a] -> Bool` | 空リストかどうか |
| `list` | `a... -> [a]` | 引数からリストを構築 |

### 入出力

| 関数 | 型 | 説明 |
|------|----|----|
| `print` | `a -> ()` | 値を標準出力に表示 |

## Twister Library (標準ライブラリ)

Spinor 言語自身で記述された標準ライブラリです。

### リスト操作 (`twister/list.spin`)

```lisp
(map f xs)      ;; 各要素に関数を適用
(filter p xs)   ;; 条件を満たす要素を抽出
(foldl f z xs)  ;; 左畳み込み
(foldr f z xs)  ;; 右畳み込み
(append xs ys)  ;; リストの連結
(reverse xs)    ;; リストの反転
(length xs)     ;; リストの長さ
```

### 論理演算 (`twister/core.spin`)

```lisp
(not x)         ;; 論理否定
(and a b)       ;; 論理積 (マクロ)
(or a b)        ;; 論理和 (マクロ)
```

### 制御構造 (`twister/core.spin`)

```lisp
(cond           ;; 多分岐条件 (マクロ)
  (cond1 expr1)
  (cond2 expr2)
  (#t default))

(when cond body) ;; 条件が真のときのみ実行 (マクロ)
```

### 数学関数 (`twister/math.spin`)

```lisp
(fact n)        ;; 階乗
(fib n)         ;; フィボナッチ数
(even? n)       ;; 偶数判定
(odd? n)        ;; 奇数判定
(abs n)         ;; 絶対値
(max a b)       ;; 最大値
(min a b)       ;; 最小値
```

## Types (型)

### 基本型

- `Int` — 整数
- `Bool` — 真偽値 (`#t`, `#f`)
- `()` — Unit 型

### 型構築子

- `[a]` — リスト型 (例: `[Int]`, `[[Bool]]`)
- `a -> b` — 関数型 (例: `Int -> Bool`)

### 型推論

Spinor は Hindley-Milner 型推論 (Algorithm W) を採用しています。
型注釈なしでも、式の型は自動的に推論されます。

```lisp
spinor> (fn (x) (+ x 1))
:: Int -> Int

spinor> (fn (f) (fn (x) (f (f x))))
:: (a -> a) -> a -> a
```
