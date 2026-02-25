# mac
**Kind:** Special Form  
**Signature:** `(Params, Body) -> Macro`
### Syntax:
```lisp
(mac (param1 param2 ...) body)
```
### Arguments and Values:
- `params` -- 仮引数のリスト
- `body` -- マクロ本体
- 戻り値: マクロオブジェクト
### Description:
マクロを作成します。引数は評価されずにそのまま渡され、返り値が評価されます。
### Examples:
```lisp
(def when (mac (cond body)
  (list 'if cond body nil)))
(when (> 5 3) (print "yes"))  ; => "yes"
```
### See Also:
[fn](ref/fn), [quote](ref/quote)