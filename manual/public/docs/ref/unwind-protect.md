# unwind-protect
**Kind:** Special Form  
**Signature:** `(Expr, Expr...) -> Val`
### Syntax:
```lisp
(unwind-protect protected-form cleanup-form...)
```
### Arguments and Values:
- `protected-form` -- 保護される式 (1つ)
- `cleanup-form` -- 必ず実行されるクリーンアップ式 (0個以上)
- 戻り値: `protected-form` の結果 (エラー時は再送出)
### Description:
保護フォームを評価し、その成否に関わらずクリーンアップフォームを必ず実行します。ファイル入出力やリソース解放に必須の構文です。
### Examples:
```lisp
;; 必ずクリーンアップが実行される
(def cleaned nil)
(ignore-errors
  (unwind-protect
    (error "fail")
    (setq cleaned t)))
cleaned  ; => t

;; ファイル操作の典型例
(unwind-protect
  (begin
    (def data (read-file "input.txt"))
    (process data))
  (print "cleanup completed"))

;; 正常終了時も実行
(unwind-protect
  (+ 1 2)
  (print "done"))  ; "done" が出力され、3 を返す
```
### Side Effects:
クリーンアップフォームの副作用が必ず実行されます。
### Exceptional Situations:
クリーンアップ中のエラーは無視されます。保護フォームでエラーが発生した場合、クリーンアップ後にエラーが再送出されます。
### See Also:
[ignore-errors](ref/ignore-errors), [handler-case](ref/handler-case)
### Notes:
Common Lisp の `unwind-protect` と同様の動作をします。リソースリークを防ぐために、ファイルハンドルやロックの解放には必ずこの構文を使用してください。