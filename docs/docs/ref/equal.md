# equal
**Kind:** Function  
**Signature:** `(a, a) -> Bool`
### Syntax:
```lisp
(equal a b)
```
### Arguments and Values:
- `a`, `b` -- 比較する値
- 戻り値: 構造的に等しければ `t`
### Description:
2つの値が構造的に等しいかを判定します。リストも再帰的に比較します。
### Examples:
```lisp
(equal '(1 2) '(1 2))  ; => t
(equal '(1 2) '(1 3))  ; => nil
```
### See Also:
[eq](eq), [eq-op](eq-op)