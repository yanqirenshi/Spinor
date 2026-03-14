# quote
**Kind:** Special Form  
**Signature:** `(Expr) -> Val`
### Syntax:
```lisp
(quote expr)  ; または 'expr
```
### Arguments and Values:
- `expr` -- クオートする式
- 戻り値: 評価されていない式そのもの
### Description:
式を評価せずにそのままデータとして返します。
### Examples:
```lisp
(quote (+ 1 2))  ; => (+ 1 2)
'(a b c)         ; => (a b c)
```
### See Also:
[list](list), [mac](mac)