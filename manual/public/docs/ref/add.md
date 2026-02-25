# +
**Kind:** Function  
**Signature:** `(Int, Int) -> Int`
### Syntax:
```lisp
(+ a b)
```
### Arguments and Values:
- `a` -- 第一オペランド (整数)
- `b` -- 第二オペランド (整数)
- 戻り値: `a + b` の結果 (整数)
### Description:
2つの整数を加算し、その和を返します。
### Examples:
```lisp
(+ 1 2)      ; => 3
(+ 10 -3)    ; => 7
(+ 0 0)      ; => 0
```
### Exceptional Situations:
引数が整数でない場合、型エラーが発生します。
### See Also:
[sub](sub), [mul](mul), [mod](mod)