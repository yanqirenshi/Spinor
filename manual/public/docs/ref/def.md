# def

**Kind:** Special Form

### Syntax:

```lisp
(def name expr)
```

### Arguments and Values:

- `name` -- 定義する変数名 (シンボル)
- `expr` -- 束縛する値を生成する式
- 戻り値: 束縛された値

### Description:

トップレベルに変数を定義します。シンボル `name` に式 `expr` の評価結果を束縛します。

### Examples:

```lisp
;; 数値の定義
(def x 42)
x  ; => 42

;; 関数の定義
(def square (fn (n) (* n n)))
(square 5)  ; => 25
```

### Side Effects:

グローバル環境に新しい束縛を追加します。

### Affected By:

None.

### Exceptional Situations:

None.

### See Also:

[fn](fn), [let](let), [setq](setq)

### Notes:

`define` は `def` のエイリアスです。Scheme スタイルの構文を好む場合に使用できます。
