# print
**Kind:** Function  
**Signature:** `(a) -> a`
### Syntax:
```lisp
(print expr)
```
### Arguments and Values:
- `expr` -- 表示する値
- 戻り値: 入力と同じ値
### Description:
値を標準出力に表示し、その値をそのまま返します。デバッグに便利です。
### Examples:
```lisp
(print "Hello, World!")  ; 出力: Hello, World!
(+ 1 (print 2))          ; 出力: 2, 戻り値: 3
```
### Side Effects:
標準出力にテキストを出力します。
### See Also:
[write-file](ref/write-file)