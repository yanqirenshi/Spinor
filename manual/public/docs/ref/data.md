# data
**Kind:** Special Form  
**Signature:** `(TypeName, Constructors...) -> ()`
### Syntax:
```lisp
(data TypeName (Ctor1 field1 ...) (Ctor2 field2 ...) ...)
```
### Arguments and Values:
- `TypeName` -- 型の名前
- `Ctors` -- コンストラクタ定義のリスト
- 戻り値: ()
### Description:
代数的データ型 (ADT) を定義します。
### Examples:
```lisp
(data Maybe (Just val) Nothing)
(data Tree (Leaf val) (Node left right))
```
### See Also:
[match](match)