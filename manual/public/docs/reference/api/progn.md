# progn
**Kind:** Special Form  
**Signature:** `(Expr...) -> Val`
### Syntax:
```lisp
(progn expr1 expr2 ... exprN)
```
### Arguments and Values:
- `exprs` -- 評価する式のシーケンス
- 戻り値: 最後の式の評価結果
### Description:
`begin` のエイリアス。Common Lisp スタイル。
### Examples:
```lisp
(progn (setq x 1) (setq x (+ x 1)) x)  ; => 2
```
### See Also:
[begin](begin)