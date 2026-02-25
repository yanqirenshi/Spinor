# car
**Kind:** Function  
**Signature:** `([a]) -> a`
### Syntax:
```lisp
(car lst)
```
### Arguments and Values:
- `lst` -- 対象のリスト
- 戻り値: リストの最初の要素
### Description:
リストの先頭要素 (head) を返します。
### Examples:
```lisp
(car '(1 2 3))  ; => 1
(car '(a))      ; => a
```
### See Also:
[cdr](ref/cdr), [cons](ref/cons)