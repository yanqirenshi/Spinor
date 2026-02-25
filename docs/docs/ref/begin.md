# begin
**Kind:** Special Form  
**Signature:** `(Expr...) -> Val`
### Syntax:
```lisp
(begin expr1 expr2 ... exprN)
```
### Arguments and Values:
- `exprs` -- 評価する式のシーケンス
- 戻り値: 最後の式の評価結果
### Description:
複数の式を順次評価し、最後の式の値を返します。
### Examples:
```lisp
(begin
  (print "first")
  (print "second")
  42)  ; => 42 ("first" "second" が出力される)
```
### See Also:
[progn](ref/progn), [let](ref/let)