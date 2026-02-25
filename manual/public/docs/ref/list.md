# list
**Kind:** Function  
**Signature:** `(a...) -> [a]`
### Syntax:
```lisp
(list elem1 elem2 ...)
```
### Arguments and Values:
- `elems` -- リストの要素
- 戻り値: 引数を含むリスト
### Description:
引数をリストにまとめます。
### Examples:
```lisp
(list 1 2 3)      ; => (1 2 3)
(list 'a 'b)      ; => (a b)
(list)            ; => nil
```
### See Also:
[cons](ref/cons), [quote](ref/quote)