# null?
**Kind:** Function  
**Signature:** `(a) -> Bool`
### Syntax:
```lisp
(null? x)
```
### Arguments and Values:
- `x` -- 判定する値
- 戻り値: `nil` なら `t`、それ以外は `nil`
### Description:
引数が空リスト (nil) かどうかを判定します。
### Examples:
```lisp
(null? nil)       ; => t
(null? '())       ; => t
(null? '(1 2))    ; => nil
```
### See Also:
[empty-p](ref/empty-p), [cons](ref/cons)