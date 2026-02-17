# Task 22: 制御構文マクロの実装

## 担当者

Claude Code

## 背景 (Background)

プロジェクトのロードマップ (TODO.md) に基づき、Common Lisp互換の基本的な制御構文を Spinor の標準ライブラリに実装します。これにより、より複雑なロジックを簡潔に記述できるようになります。

詳細は仕様書 `specs/22_control_flow.md` を確認してください。

## 変更対象ファイル (File to Modify)

-   `twister/core.spin`

## 実装内容 (Implementation Details)

`twister/core.spin` を開き、以下のマクロを追加・更新してください。既存の `when` と `cond` は削除し、新しい実装に置き換えてください。

```lisp
; twister/core.spin

(module twister/core (export not id defun when unless cond and or))

(def not (fn (x) (if x #f #t)))
(def id  (fn (x) x))

; (defun name (args...) body...) -> (def name (fn (args...) body...))
(def defun
  (mac (name params . body)
    (list 'def name (cons 'fn (cons params body)))))

; (when condition body...) -> (if condition (progn body...) #f)
; progn is simulated with an immediately-invoked function expression (IIFE).
(def when
  (mac (condition . body)
    (list 'if condition (cons (list 'fn '()) body) '#f)))

; (unless condition body...) -> (if condition #f (progn body...))
(def unless
  (mac (condition . body)
    (list 'if condition '#f (cons (list 'fn '()) body))))

; (cond (p1 e1...) (p2 e2...) ...) -> (if p1 (progn e1) (if p2 (progn e2) ...))
; If body is missing, the predicate's value is returned.
(def cond
  (mac args
    (if (null? args)
        '#f
        (let clause (car args)
          (let pred (car clause)
            (let exprs (cdr clause))
            (let next-cond (cons 'cond (cdr args)))
            (if (null? exprs)
                ; Case: (pred) -> (or pred (next-cond))
                ; Since `or` is also being defined, we build its expansion manually.
                ; This evaluates `pred` twice if it's truthy, a known limitation without `let`.
                (list 'if pred pred next-cond)
                ; Case: (pred expr ...) -> (if pred (progn expr ...) (next-cond))
                (list 'if pred (cons (list 'fn '()) exprs) next-cond)))))))

; (and form1 form2 ...)
(def and
  (mac args
    (if (null? args)
        '#t
        (if (null? (cdr args))
            (car args)
            (list 'if (car args) (cons 'and (cdr args)) '#f)))))

; (or form1 form2 ...)
(def or
  (mac args
    (if (null? args)
        '#f
        (if (null? (cdr args))
            (car args)
            ; Note: This evaluates (car args) twice if it is truthy.
            ; This is a necessary trade-off in the absence of a `let` primitive
            ; to bind the result without re-evaluation.
            (list 'if (car args) (car args) (cons 'or (cdr args)))))))
```
**注意:** `twister/core.spin` 内の `(let ...)` は、現在実装中のマクロ内での可読性のためのものであり、Spinor言語の `let` 構文に展開されるわけではありません。上記の `cond` の実装は、`let` を使わずに `car`/`cdr` を直接使って書き直す必要があります。

**`let` を使わない `cond` の実装:**
```lisp
(def cond
  (mac args
    (if (null? args)
        '#f ; No clauses, return false.
        ; (list 'if pred expr next-chain)
        (list 'if
              (car (car args)) ; Predicate
              (if (null? (cdr (car args)))
                  (car (car args)) ; Body is missing, use predicate's value.
                  (cons (list 'fn '()) (cdr (car args))) ; Body exists, wrap in progn.
              )
              (cons 'cond (cdr args)) ; Next recursive call to cond
        )
    )
  ))
```
最終的にはこちらの `let` を使わない `cond` の実装を使用してください。

## 確認手順 (Verification)

1.  `twister/test.spin` を開き、以下のテストケースを追加します。

    ```lisp
    ;;; Task 22: Control Flow Tests

    (defun add (x y) (+ x y))
    (assert-equal "(defun add (x y) (+ x y))" (add 10 20) 30)

    (def test-when 0)
    (when #t (set! test-when 1) (set! test-when 2))
    (assert-equal "(when #t ...)" test-when 2)
    (when #f (set! test-when 99))
    (assert-equal "(when #f ...)" test-when 2)

    (def test-unless 0)
    (unless #f (set! test-unless 1) (set! test-unless 2))
    (assert-equal "(unless #f ...)" test-unless 2)
    (unless #t (set! test-unless 99))
    (assert-equal "(unless #t ...)" test-unless 2)

    (assert-equal "(cond ...)"
                  (cond ((= 1 2) 'a)
                        ((= 2 2) 'b)
                        (else 'c)) ; `else` is just a truthy symbol
                  'b)

    (assert-equal "(cond with only pred)" (cond ((< 1 0)) ((> 1 0))) #t)

    (assert-equal "(and #t #t)" (and #t #t) #t)
    (assert-equal "(and #t 'ok)" (and #t 'ok) 'ok)
    (assert-equal "(and #f (error \"fail\"))" (and #f (error "fail")) #f)
    (assert-equal "(and)" (and) #t)

    (assert-equal "(or #f 'ok)" (or #f 'ok) 'ok)
    (assert-equal "(or #t (error \"fail\"))" (or #t (error "fail")) #t)
    (assert-equal "(or #f #f 'last)" (or #f #f 'last) 'last)
    (assert-equal "(or)" (or) #f)
    ```

2.  ターミナルで以下のコマンドを実行し、すべてのテストがパスすることを確認します。

    ```sh
    cabal run spinor -- twister/test.spin
    ```

    `All tests passed.` と表示されれば完了です。

# 実装方針

## 設計判断

### 1. `begin` 特殊形式の活用

タスク指示書では IIFE (即時実行関数式) パターンを使用して `progn` をシミュレートする方針が示されていましたが、Spinor カーネルには既に `begin` 特殊形式が実装されていることが判明しました（`Eval.hs:126`）。

**元の指示:**
```lisp
(list 'if condition (cons (list 'fn '()) body) '#f)
; 展開結果: (if cond ((fn ()) e1 e2 ...) #f) — 構文的に不正
```

**実際の問題:**
- Spinor の `fn` は正確に3要素 `(fn params body)` を期待する
- 複数 body をサポートしていない
- IIFE パターン `((fn () e1 e2 ...))` は構文エラーとなる

**採用した解決策:**
```lisp
(list 'if condition (cons 'begin body) '#f)
; 展開結果: (if cond (begin e1 e2 ...) #f) — 正しい構文
```

`begin` を直接使用することで、より単純で堅牢な実装となりました。

### 2. `setq` vs `set!`

タスク指示書のテストでは `set!` が使用されていましたが、Step 21 で実装されたのは `setq` であるため、テストコードを `setq` に修正しました。

### 3. `assert-equal` の3引数化

既存の `assert-equal` は2引数 `(expected actual)` でしたが、タスク指示書のテストは3引数 `(label actual expected)` 形式を使用していたため、以下の変更を行いました：

- `assert-equal` 関数を3引数形式に変更
- 比較関数を `=` から `equal` に変更（構造比較対応）
- 既存のテストケースをすべて新形式に更新

# 実装内容

## 変更ファイル

### 1. `twister/core.spin`

**変更内容:**
- `module` の export リストを更新: `defun`, `unless`, `and`, `or` を追加
- 既存の `when` マクロを複数 body 対応版に置き換え
- 既存の `cond` マクロを `let` を使わない版に置き換え
- 新規マクロ追加: `defun`, `unless`, `and`, `or`

**最終的な実装:**
```lisp
; core.spin — 基本論理関数とマクロ
(module twister/core (export not id defun when unless cond and or))

(def not (fn (x) (if x #f #t)))
(def id  (fn (x) x))

; (defun name (args...) body...) -> (def name (fn (args...) body...))
(def defun
  (mac (name params . body)
    (list 'def name (cons 'fn (cons params body)))))

; (when condition body...) -> (if condition (begin body...) #f)
(def when
  (mac (condition . body)
    (list 'if condition (cons 'begin body) '#f)))

; (unless condition body...) -> (if condition #f (begin body...))
(def unless
  (mac (condition . body)
    (list 'if condition '#f (cons 'begin body))))

; (cond (p1 e1...) (p2 e2...) ...) -> (if p1 (begin e1...) (if p2 (begin e2...) ...))
; If body is missing, the predicate's value is returned.
(def cond
  (mac args
    (if (null? args)
        '#f
        (list 'if
              (car (car args))
              (if (null? (cdr (car args)))
                  (car (car args))
                  (cons 'begin (cdr (car args))))
              (cons 'cond (cdr args))))))

; (and form1 form2 ...)
(def and
  (mac args
    (if (null? args)
        '#t
        (if (null? (cdr args))
            (car args)
            (list 'if (car args) (cons 'and (cdr args)) '#f)))))

; (or form1 form2 ...)
(def or
  (mac args
    (if (null? args)
        '#f
        (if (null? (cdr args))
            (car args)
            (list 'if (car args) (car args) (cons 'or (cdr args)))))))
```

### 2. `twister/test.spin`

**変更内容:**
- `assert-equal` を3引数 `(label actual expected)` 形式に変更
- 既存テストを新形式に更新
- Step 22 の制御構文マクロテストを追加 (14テストケース)
  - `defun`: 1テスト
  - `when`: 2テスト
  - `unless`: 2テスト
  - `cond`: 2テスト
  - `and`: 4テスト
  - `or`: 4テスト
- テスト終了メッセージ `"\nAll tests passed.\n"` を追加

## テスト結果

```
$ cabal run spinor -- twister/test.spin
Loading Twister environment...
Twister loaded.
....................................................

All tests passed.
```

52個のテストがすべてパスしました。
