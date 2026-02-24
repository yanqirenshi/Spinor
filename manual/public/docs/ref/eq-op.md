# =
**Kind:** Function  
**Signature:** `(a, a) -> Bool`
### Syntax:
```lisp
(= a b)
```
### Arguments and Values:
- `a`, `b` -- 比較する値
- 戻り値: 等しければ `t`、そうでなければ `nil`
### Description:
2つの値が等しいかを判定します。
### Examples:
```lisp
(= 1 1)      ; => t
(= "a" "b")  ; => nil
```
### See Also:
[lt](lt), [gt](gt), [eq](eq), [equal](equal)