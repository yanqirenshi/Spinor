# ignore-errors
**Kind:** Special Form  
**Signature:** `(Expr...) -> Val | nil`
### Syntax:
```lisp
(ignore-errors body...)
```
### Arguments and Values:
- `body` -- 評価する式 (0個以上)
- 戻り値: 成功時は最後の式の値、エラー時は `nil`
### Description:
本体の評価中にエラーが発生した場合、そのエラーを無視して `nil` を返します。エラーが発生しなければ最後の式の結果を返します。
### Examples:
```lisp
;; ゼロ除算エラーを無視
(ignore-errors (/ 1 0))  ; => nil

;; 正常時は値を返す
(ignore-errors (+ 1 2))  ; => 3

;; 複数の式
(ignore-errors
  (print "start")
  (error "fail")
  (print "end"))  ; => nil ("start" のみ出力)
```
### Side Effects:
本体の副作用はエラー発生前まで実行されます。
### Exceptional Situations:
None. (全てのエラーを捕捉します)
### See Also:
[handler-case](ref/handler-case), [unwind-protect](ref/unwind-protect)
### Notes:
Common Lisp の `ignore-errors` に相当します。詳細なエラー処理が必要な場合は `handler-case` を使用してください。