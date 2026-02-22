# list

**Kind:** Function

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

### Side Effects:

None.

### Affected By:

None.

### Exceptional Situations:

None.

### See Also:

[cons](cons), [quote](quote)
