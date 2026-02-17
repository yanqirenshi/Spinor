# タスク: ステップ8 - 可変長引数 (Varargs) と cond マクロ

## 現在の状況

* 固定引数の関数とマクロは動作している。
* `cond` のような可変長の引数を取る構文が作れない。

## 目標

1. `(fn (x . xs) ...)` のような **ドット記法** による可変長引数をサポートする。
2. Twister (ユーザーランド) で `cond` マクロを実装する。

## 実装詳細指示

### 1. `src/Spinor/Eval.hs` (修正: 引数バインディング)

関数適用時に、仮引数（定義側の引数名リスト）と実引数（渡された値リスト）を結びつけるロジック (`bindArgs` のようなヘルパー関数があるはずです) を修正してください。

* **変更前:** リストの長さが一致しないとエラー。
* **変更後:**
    * 仮引数リストの中にシンボル `.` が含まれている場合を検知する。
    * 例: 仮引数が `["x", ".", "xs"]` で、実引数が `[1, 2, 3, 4]` の場合:
        * `x` = `1`
        * `xs` = `[2, 3, 4]` (残りをリストとして束縛)
    * `.` がない場合は、従来どおり固定長として扱う。

### 2. `twister/core.spin` (拡張: condの実装)

可変長引数が使えるようになったので、`cond` マクロを実装してください。
再帰的なマクロになります。

```lisp
; cond の実装イメージ
; (cond (p1 e1) (p2 e2) ...)
; -> (if p1 e1 (cond (p2 e2) ...))

(def cond
  (mac args
    (if (null? args)
        (quote ()) ; 該当なしなら nil (または #f)
        (let clause (car args)
          (let rest (cdr args)
            (let pred (car clause)
              (let expr (car (cdr clause))
                (list 'if pred expr (cons 'cond rest)))))))))

```

※ 現在 `let` は1変数しか束縛できない簡易版のはずなので、`nested let` を使うか、必要であれば `let` も強化してください。上記のコードは `nested let` スタイルです。

### 3. `twister/list.spin` (拡張)

* `list` 関数を可変長引数対応にする（もし現在プリミティブでないなら）。
* `(def list (fn args args))` で実装できます（引数をそのまま返すだけ）。

## 確認事項 (REPL)

実装後、以下が動くことを確認してください。

```lisp
;; 可変長引数のテスト
(def my-list (fn args args))
(my-list 1 2 3) ; -> (1 2 3)

;; cond のテスト
(def x 10)
(cond ((= x 5) 'five)
      ((= x 10) 'ten)
      (#t 'other))
; -> 'ten

```

## 出力要件

* `Eval.hs` の引数バインディングロジック（Haskellコード）。
* `twister/core.spin` の `cond` 実装部分。
* 日本語での解説。

# 実装方針

## 概要

ドット記法による可変長引数をサポートし、それを活用して `cond` マクロを Twister で実装する。

## 設計判断

### bindArgs による統一処理

仮引数リストを `"."` で `break` し、3パターンを統一処理:
- **固定長** `["a", "b"]`: 引数数が一致する必要がある
- **ドット記法** `["a", ".", "rest"]`: 固定部分をマッチさせ、残りをリストで束縛
- **全キャプチャ** `[".", "args"]`: 全引数をリストとして束縛

全キャプチャは `(fn name body)` / `(mac name body)` 構文で、内部的に `[".", name]` として格納。ドット記法の固定部分が 0 個のケースとして統一処理。

### cond マクロの設計

`(mac args ...)` で全引数をリストとして受け取り、再帰的に展開:
`(cond (p1 e1) (p2 e2) ...)` → `(if p1 e1 (cond (p2 e2) ...))`

`car`/`cdr` で分解し、`list` と `cons` で新しい S式を構築する。引数なしの場合は `nil` を返す。

## 変更の流れ

1. `src/Spinor/Eval.hs` (修正) — `(fn name body)` / `(mac name body)` 全引数キャプチャ対応、`bindArgs` でドット記法可変長引数に対応
2. `twister/core.spin` (修正) — `cond` マクロ追加

# 実装内容

## 変更ファイル

| ファイル | 操作 | 概要 |
|---|---|---|
| `src/Spinor/Eval.hs` | 修正 | `(fn name body)` / `(mac name body)` 全引数キャプチャ対応、`applyClosureBody` を `bindArgs` で書き直しドット記法可変長引数に対応 |
| `twister/core.spin` | 修正 | `cond` マクロを追加 (可変長引数マクロ) |

## 設計メモ

### 可変長引数の3パターン

`bindArgs` ヘルパーで仮引数リストを `"."` で `break` し、以下の3パターンを統一処理:

| パターン | 仮引数 (内部表現) | 例 | 束縛結果 |
|---|---|---|---|
| 固定長 | `["a", "b"]` | `(fn (a b) ...)` | `a=1, b=2` |
| ドット記法 | `["a", ".", "rest"]` | `(fn (a . rest) ...)` | `a=1, rest=(2 3 4)` |
| 全キャプチャ | `[".", "args"]` | `(fn args ...)` / `(mac args ...)` | `args=(1 2 3 4)` |

全キャプチャは内部的に `[".", name]` として格納し、ドット記法の固定部分が0個のケースとして統一処理。

### cond マクロの展開

`(cond (p1 e1) (p2 e2) ...)` → `(if p1 e1 (cond (p2 e2) ...))` と再帰的に展開。`(mac args ...)` で全引数をリストとして受け取り、`car`/`cdr` で分解して `list` で新しい S 式を構築する。

## テスト結果

```
$ cabal run spinor
Spinor REPL (step5)
Loading Twister environment...
Twister loaded.

;; cond マクロ
spinor> (cond (#t 42))                          => 42
spinor> (cond (#f 1) (#t 2) (#t 3))             => 2
spinor> (cond (#f 1) (#f 2))                     => nil

;; cond を使った関数
spinor> (def classify (fn (n) (cond ((< n 0) -1) ((= n 0) 0) (#t 1))))
spinor> (classify -5)                            => -1
spinor> (classify 0)                             => 0
spinor> (classify 99)                            => 1

;; ドット記法 varargs
spinor> ((fn (x . xs) xs) 1 2 3 4)               => (2 3 4)
spinor> ((fn (a b . rest) (list a b rest)) 10 20 30 40 50)  => (10 20 (30 40 50))

;; 全引数キャプチャ
spinor> ((fn args (length args)) 1 2 3)          => 3
```
