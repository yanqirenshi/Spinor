# cdr
**Kind:** Function  
**Signature:** `([a]) -> [a]`
### Syntax:
```lisp
(cdr lst)
```
### Arguments and Values:
- `lst` -- 対象のリスト
- 戻り値: 先頭を除いたリスト
### Description:
リストの先頭以外 (tail) を返します。
### Examples:
```lisp
(cdr '(1 2 3))  ; => (2 3)
(cdr '(a))      ; => nil
```
### See Also:
[car](car), [cons](cons)