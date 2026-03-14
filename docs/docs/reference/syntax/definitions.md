# Definitions

変数、関数、マクロの定義方法を解説します。

## 変数定義: `def`

トップレベルに変数を定義します。

```lisp
(def x 42)
(def message "Hello")
```

## 関数定義: `fn`

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

### 高階関数

関数を引数として受け取ったり、返したりできます。

```lisp
;; 関数を引数に取る
(def apply-twice (fn (f x) (f (f x))))
(apply-twice double 3)  ; => 12

;; 関数を返す
(def make-adder (fn (n)
  (fn (x) (+ x n))))
(def add-10 (make-adder 10))
(add-10 5)  ; => 15
```

## クロージャ

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
(counter)  ; => 3
```

クロージャにより、関数は自身が定義された環境の変数にアクセスし続けることができます。

## キーワード引数: `&key`

関数やマクロにキーワード引数を定義できます。キーワード引数は `:name value` の形式で名前付きで指定します。

```lisp
;; キーワード引数を持つ関数
(def make-rect (fn (&key width height)
  (list width height)))

(make-rect :width 100 :height 50)  ; => (100 50)
(make-rect :height 50 :width 100)  ; => (100 50)  順序は任意
```

### デフォルト値

キーワード引数にはデフォルト値を指定できます。

```lisp
(def http-request (fn (url &key (method "GET") (timeout 30))
  (list url method timeout)))

(http-request "https://example.com")
; => ("https://example.com" "GET" 30)

(http-request "https://example.com" :method "POST" :timeout 60)
; => ("https://example.com" "POST" 60)
```

デフォルト値を指定しない場合、引数が渡されなかったときは `nil` になります。

### 必須引数との組み合わせ

必須引数とキーワード引数を組み合わせることができます。

```lisp
(def send-email (fn (to subject &key (from "noreply@example.com") (cc nil))
  (list :to to :subject subject :from from :cc cc)))

(send-email "user@example.com" "Hello" :cc "admin@example.com")
; => (:to "user@example.com" :subject "Hello" :from "noreply@example.com" :cc "admin@example.com")
```

## マクロ定義: `mac`

コンパイル時に展開されるマクロを定義します。

```lisp
(def when (mac (condition body)
  (list 'if condition body nil)))

(when (> 5 3)
  (print "yes"))  ; => "yes"
```

### マクロと関数の違い

| | 関数 | マクロ |
|:--|:-----|:------|
| 評価タイミング | 実行時 | コンパイル時 |
| 引数の評価 | 事前に評価 | 評価されない |
| 返り値 | 値 | コード (展開結果) |

```lisp
;; マクロは引数を評価せずに受け取る
(def unless (mac (condition body)
  (list 'if condition nil body)))

(unless nil (print "executed"))  ; => "executed"
```

## Symbols

| Type | Name | Description |
|:-----|:-----|:------------|
| Special Form | [def](../api/def) | トップレベル変数・関数を定義 |
| Special Form | [fn](../api/fn) | 無名関数 (ラムダ式) を作成 |
| Special Form | [mac](../api/mac) | マクロを定義 |
| Special Form | [let](../api/let) | ローカル変数を束縛 (クロージャで使用) |
| Special Form | [setq](../api/setq) | 変数の値を更新 |
| Function | [list](../api/list) | マクロ展開でコードを生成 |
| Special Form | [quote](../api/quote) | マクロでシンボルをそのまま扱う |
