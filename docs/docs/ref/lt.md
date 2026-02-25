# <
**Kind:** Function  
**Signature:** `(Int, Int) -> Bool`
### Syntax:
```lisp
(< a b)
```
### Arguments and Values:
- `a`, `b` -- 比較する整数
- 戻り値: `a < b` なら `t`
### Description:
左辺が右辺より小さいかを判定します。
### Examples:
```lisp
(< 1 2)  ; => t
(< 5 3)  ; => nil
```
### See Also:
[gt](ref/gt), [eq-op](ref/eq-op)