# match
**Kind:** Special Form  
**Signature:** `(Expr, Branches...) -> Val`
### Syntax:
```lisp
(match expr (pattern1 body1) (pattern2 body2) ...)
```
### Arguments and Values:
- `expr` -- マッチ対象の式
- `patterns` -- パターンと本体のペア
- 戻り値: マッチした分岐の本体の評価結果
### Description:
パターンマッチを行います。式の値に最初にマッチするパターンの本体を評価します。
### Examples:
```lisp
(data Maybe (Just val) Nothing)

(def safe-head (fn (lst)
  (match lst
    ((cons x _) (Just x))
    (nil Nothing))))
```
### See Also:
[if](if), [data](data)