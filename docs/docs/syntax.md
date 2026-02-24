# Syntax

Spinor の基本的な構文を解説します。

## アトム (Atoms)

Spinor の最も基本的なデータ型です。

### 数値 (Numbers)

整数リテラルをサポートしています。

```lisp
42        ; 正の整数
-17       ; 負の整数
0         ; ゼロ
```

### シンボル (Symbols)

変数名や関数名として使用される識別子です。

```lisp
x
my-variable
calculate-sum
string->list    ; 矢印を含むシンボル
null?           ; 疑問符で終わるシンボル (述語)
```

### 文字列 (Strings)

ダブルクオートで囲まれたテキストです。

```lisp
"Hello, World!"
"日本語も使えます"
"Line 1\nLine 2"    ; エスケープシーケンス
```

### 特殊シンボル

| シンボル | 意味 |
|:---------|:-----|
| `nil` | 空リスト / 偽値 |
| `t` | 真値 |

## 型システム (Type System)

Spinor は Hindley-Milner 型推論に基づく静的型付け言語です。

### 型推論

明示的な型注釈なしで、コンパイル時に型が推論されます。

```lisp
spinor> (+ 1 2)
:: Int
3

spinor> (cons 1 '(2 3))
:: (List Int)
(1 2 3)

spinor> (fn (x) (* x 2))
:: (-> Int Int)
#<fn>
```

### 基本型

| 型 | 説明 | 例 |
|:---|:-----|:---|
| `Int` | 整数 | `42`, `-17` |
| `Float` | 浮動小数点数 | `3.14`, `-2.5` |
| `String` | 文字列 | `"hello"` |
| `Bool` | 真偽値 | `t`, `nil` |
| `(List a)` | リスト | `'(1 2 3)` |
| `(-> a b)` | 関数型 | `(fn (x) x)` |

### 型表示: `::`

REPL では評価結果の前に `::` で型が表示されます。

```lisp
spinor> (list 1 2 3)
:: (List Int)
(1 2 3)

spinor> (def id (fn (x) x))
:: (-> a a)
id
```

### 多相性

型パラメータ (`a`, `b` など) による多相関数をサポートしています。

```lisp
;; 恒等関数: 任意の型を受け取り、そのまま返す
(def id (fn (x) x))
;; :: (-> a a)

;; リスト操作: 任意の要素型のリストを処理
(def length (fn (lst)
  (match lst
    (nil 0)
    ((cons _ xs) (+ 1 (length xs))))))
;; :: (-> (List a) Int)
```

## リストと評価 (Lists and Evaluation)

### リスト

括弧で囲まれた要素の並びがリストです。

```lisp
(1 2 3)           ; 数値のリスト
(a b c)           ; シンボルのリスト
((1 2) (3 4))     ; ネストしたリスト
()                ; 空リスト (nil と同じ)
```

### 評価規則

リストの最初の要素が関数として評価され、残りが引数として渡されます。

```lisp
(+ 1 2)           ; => 3  (1 + 2)
(* (+ 1 2) 4)     ; => 12 ((1 + 2) * 4)
```

### クオート (Quote)

評価を抑制し、式をそのままデータとして扱います。

```lisp
(quote (1 2 3))   ; => (1 2 3)
'(1 2 3)          ; 省略記法
'x                ; => x (シンボルとして)
```

## 定義 (Definitions)

### 変数定義: `def`

トップレベルに変数を定義します。

```lisp
(def x 42)
(def message "Hello")
```

### 関数定義: `fn`

無名関数 (ラムダ式) を作成します。

```lisp
;; 基本形
(fn (x) (* x 2))

;; 名前付き関数
(def double (fn (x) (* x 2)))
(double 5)  ; => 10

;; 複数引数
(def add (fn (a b) (+ a b)))
(add 3 4)   ; => 7
```

### クロージャ

関数は定義時の環境を捕捉します。

```lisp
(def make-counter (fn ()
  (let ((count 0))
    (fn ()
      (setq count (+ count 1))
      count))))

(def counter (make-counter))
(counter)  ; => 1
(counter)  ; => 2
```

### マクロ定義: `mac`

コンパイル時に展開されるマクロを定義します。

```lisp
(def when (mac (condition body)
  (list 'if condition body nil)))

(when (> 5 3)
  (print "yes"))  ; => "yes"
```

## 制御構造 (Control Flow)

### 条件分岐: `if`

```lisp
(if condition
    then-expr
    else-expr)

;; 例
(if (> x 0)
    "positive"
    "non-positive")
```

**注意:** Spinor では `nil` のみが偽です。`0` や空文字列も真として扱われます。

### ローカル変数: `let`

```lisp
(let ((var1 val1)
      (var2 val2))
  body)

;; 例
(let ((x 10)
      (y 20))
  (+ x y))  ; => 30
```

束縛は逐次的に行われます (Common Lisp の `let*` と同様)。

```lisp
(let ((x 5)
      (y (* x 2)))   ; x を参照可能
  y)  ; => 10
```

### 逐次実行: `begin` / `progn`

複数の式を順に実行し、最後の値を返します。

```lisp
(begin
  (print "Step 1")
  (print "Step 2")
  42)  ; => 42
```

### 代入: `setq`

既存の変数の値を変更します。

```lisp
(def x 1)
(setq x 2)
x  ; => 2
```

## データ型 (Data Types)

### 代数的データ型: `data`

カスタムデータ型を定義します。

```lisp
;; Maybe 型
(data Maybe
  (Just val)
  Nothing)

;; 二分木
(data Tree
  (Leaf val)
  (Node left right))

;; 使用例
(Just 42)
Nothing
(Node (Leaf 1) (Leaf 2))
```

### パターンマッチ: `match`

データ型の構造に基づいて分岐します。

```lisp
(def safe-div (fn (a b)
  (if (= b 0)
      Nothing
      (Just (/ a b)))))

(def show-result (fn (result)
  (match result
    ((Just x) (print x))
    (Nothing  (print "Error")))))

(show-result (safe-div 10 2))   ; 出力: 5
(show-result (safe-div 10 0))   ; 出力: Error
```

リストのパターンマッチ:

```lisp
(def sum-list (fn (lst)
  (match lst
    (nil 0)
    ((cons x xs) (+ x (sum-list xs))))))

(sum-list '(1 2 3 4 5))  ; => 15
```

## 次のステップ

- [API Reference](api-index) - 組み込み関数の詳細
- [Introduction](introduction) - インストールと環境設定
