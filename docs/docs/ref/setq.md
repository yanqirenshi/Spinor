# setq
**Kind:** Special Form  
**Signature:** `(Symbol, Expr) -> Val`
### Syntax:
```lisp
(setq name expr)
```
### Arguments and Values:
- `name` -- 代入先の変数名
- `expr` -- 新しい値
- 戻り値: 代入された値
### Description:
既存の変数に新しい値を代入します。変数が存在しない場合はエラーになります。
### Examples:
```lisp
(def counter 0)
(setq counter (+ counter 1))
counter  ; => 1
```
### See Also:
[def](ref/def)