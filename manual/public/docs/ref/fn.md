# fn
**Kind:** Special Form
**Signature:** `(Params, Body) -> Function`
### Syntax:
```lisp
;; 固定長引数
(fn (param1 param2 ...) body)

;; 可変長引数 (ドット記法)
(fn (param1 param2 . rest) body)

;; キーワード引数
(fn (param1 &key key1 key2) body)

;; キーワード引数 (デフォルト値付き)
(fn (param1 &key (key1 default1) key2) body)
```
### Arguments and Values:
- `params` -- 仮引数のリスト (シンボルのリスト、`&key` 含む)
- `body` -- 関数本体の式
- 戻り値: 関数オブジェクト

#### 引数の種類:
- **必須引数**: 呼び出し時に必ず指定する位置引数
- **可変長引数 (`. rest`)**: 残りの引数をリストとして受け取る
- **キーワード引数 (`&key`)**: `:name value` 形式で名前付きで指定する引数

### Description:
無名関数 (ラムダ式) を作成します。`params` は仮引数リスト、`body` は関数本体です。クロージャとして定義時の環境を捕捉します。

**キーワード引数** (`&key`) を使用すると、呼び出し時に引数を名前付きで指定できます。キーワード引数は `:name value` の形式で渡します。デフォルト値を指定しない場合、引数が渡されなかったときは `nil` になります。

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

;; キーワード引数
(def greet (fn (name &key greeting)
             (string-append greeting ", " name "!")))
(greet "World" :greeting "Hello")  ; => "Hello, World!"

;; キーワード引数 (デフォルト値付き)
(def make-rect (fn (&key (width 100) (height 50))
                 (list width height)))
(make-rect)                    ; => (100 50)
(make-rect :width 200)         ; => (200 50)
(make-rect :height 75 :width 150)  ; => (150 75)

;; 必須引数とキーワード引数の組み合わせ
(def http-request (fn (url &key (method "GET") (headers nil))
                    (list url method headers)))
(http-request "https://api.example.com" :method "POST")
; => ("https://api.example.com" "POST" nil)
```
### Exceptional Situations:
- 必須引数の数が一致しない場合、実行時エラーが発生します。
- 未知のキーワードが渡された場合も、現在は無視されます (将来のバージョンでエラーになる可能性があります)。
### See Also:
[def](ref/def), [mac](ref/mac), [let](ref/let)
### Notes:
- Spinor の関数は正格評価 (call-by-value) です。
- キーワード引数は Common Lisp の `&key` 構文と互換性があります。
- キーワード引数の順序は任意です。