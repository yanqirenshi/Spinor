# quote

**Kind:** Special Form

### Syntax:

```lisp
(quote expr)  ; または 'expr
```

### Arguments and Values:

- `expr` -- クオートする式
- 戻り値: 評価されていない式そのもの

### Description:

式を評価せずにそのままデータとして返します。

### Examples:

```lisp
(quote (+ 1 2))  ; => (+ 1 2)
'(a b c)         ; => (a b c)
```

### Side Effects:

None.

### Affected By:

None.

### Exceptional Situations:

None.

### See Also:

[list](list), [mac](mac)
