# >
**Kind:** Function  
**Signature:** `(Int, Int) -> Bool`
### Syntax:
```lisp
(> a b)
```
### Arguments and Values:
- `a`, `b` -- 比較する整数
- 戻り値: `a > b` なら `t`
### Description:
左辺が右辺より大きいかを判定します。
### Examples:
```lisp
(> 5 3)  ; => t
(> 1 2)  ; => nil
```
### See Also:
[lt](lt), [eq-op](eq-op)