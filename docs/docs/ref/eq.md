# eq
**Kind:** Function  
**Signature:** `(a, a) -> Bool`
### Syntax:
```lisp
(eq a b)
```
### Arguments and Values:
- `a`, `b` -- 比較する値
- 戻り値: 同一なら `t`
### Description:
2つの値がポインタレベルで同一かを判定します (アトムのみ)。
### Examples:
```lisp
(eq 'a 'a)    ; => t
(eq 1 1)      ; => t
```
### See Also:
[equal](equal), [eq-op](eq-op)