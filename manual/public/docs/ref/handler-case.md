# handler-case
**Kind:** Special Form  
**Signature:** `(Expr, Handler) -> Val`
### Syntax:
```lisp
(handler-case expression (error (var) handler-body...))
```
### Arguments and Values:
- `expression` -- 評価する式
- `error` -- エラーハンドラのキーワード (現在は全エラーを捕捉)
- `var` -- エラーメッセージが束縛される変数名
- `handler-body` -- エラー時に評価される式
- 戻り値: 成功時は `expression` の値、エラー時はハンドラの結果
### Description:
式を評価し、エラーが発生した場合は指定されたハンドラを実行します。エラーメッセージは指定した変数に束縛されます。
### Examples:
```lisp
;; エラーメッセージを取得
(handler-case
  (error "something went wrong")
  (error (msg)
    (string-append "Caught: " msg)))
; => "Caught: something went wrong"

;; 正常時はそのまま値を返す
(handler-case
  (+ 1 2)
  (error (e) "error occurred"))
; => 3

;; デフォルト値を返す
(handler-case
  (/ 10 0)
  (error (_) 0))  ; => 0
```
### Side Effects:
ハンドラ内の副作用が実行される可能性があります。
### Exceptional Situations:
ハンドラ形式が不正な場合、エラーを返します。
### See Also:
[ignore-errors](ref/ignore-errors), [unwind-protect](ref/unwind-protect)
### Notes:
Common Lisp の `handler-case` の簡易版です。現時点では全てのエラーを `error` キーワードで捕捉します。将来的にエラー型による分岐をサポート予定です。