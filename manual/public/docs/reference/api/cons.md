# cons
**Kind:** Function  
**Signature:** `(a, [a]) -> [a]`
### Syntax:
```lisp
(cons x lst)
```
### Arguments and Values:
- `x` -- リストの先頭に追加する値
- `lst` -- 既存のリスト
- 戻り値: `x` を先頭に持つ新しいリスト
### Description:
値をリストの先頭に追加し、新しいリストを返します。Lisp の基本的なリスト構築関数です。
### Examples:
```lisp
(cons 1 '(2 3))     ; => (1 2 3)
(cons 'a nil)       ; => (a)
(cons 1 (cons 2 (cons 3 nil)))  ; => (1 2 3)
```
### See Also:
[car](car), [cdr](cdr), [list](list)
### Notes:
`cons` は "construct" の略です。