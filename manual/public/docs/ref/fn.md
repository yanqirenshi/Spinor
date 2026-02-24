# fn
**Kind:** Special Form  
**Signature:** `(Params, Body) -> Function`
### Syntax:
```lisp
(fn (param1 param2 ...) body)
```
### Arguments and Values:
- `params` -- 仮引数のリスト (シンボルのリスト)
- `body` -- 関数本体の式
- 戻り値: 関数オブジェクト
### Description:
無名関数 (ラムダ式) を作成します。`params` は仮引数リスト、`body` は関数本体です。クロージャとして定義時の環境を捕捉します。
### Examples:
```lisp
;; 基本的なラムダ
((fn (x) (* x 2)) 5)  ; => 10

;; 複数引数
(def add (fn (a b) (+ a b)))
(add 3 4)  ; => 7

;; クロージャ
(def make-adder (fn (n) (fn (x) (+ x n))))
(def add10 (make-adder 10))
(add10 5)  ; => 15
```
### Exceptional Situations:
引数の数が一致しない場合、実行時エラーが発生します。
### See Also:
[def](def), [mac](mac), [let](let)
### Notes:
Spinor の関数は正格評価 (call-by-value) です。