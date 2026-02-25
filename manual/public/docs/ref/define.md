# define
**Kind:** Special Form  
**Signature:** `(Symbol, Expr) -> Val`
### Syntax:
```lisp
(define name expr)
```
### Arguments and Values:
- `name` -- 変数名
- `expr` -- 束縛する式
- 戻り値: 束縛された値
### Description:
変数を定義します。`def` のエイリアス。
### Examples:
```lisp
(define pi 3.14159)
```
### See Also:
[def](ref/def)