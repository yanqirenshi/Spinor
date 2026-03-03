# empty?
**Kind:** Function  
**Signature:** `(a) -> Bool`
### Syntax:
```lisp
(empty? x)
```
### Arguments and Values:
- `x` -- 判定する値
- 戻り値: 空なら `t`
### Description:
`nil?` のエイリアス。
### Examples:
```lisp
(empty? nil)  ; => t
```
### See Also:
[nil-p](ref/nil-p)