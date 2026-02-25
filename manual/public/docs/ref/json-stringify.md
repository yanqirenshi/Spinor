# json-stringify
**Kind:** Function  
**Signature:** `(Val) -> String`
### Syntax:
```lisp
(json-stringify value)
```
### Arguments and Values:
- `value` -- JSON に変換する値
- 戻り値: JSON 文字列

変換規則:
    - `VInt` → JSON Number (整数)
    - `VFloat` → JSON Number (浮動小数)
    - `VStr` → JSON String
    - `VBool` → JSON Boolean (`#t` → `true`, `#f` → `false`)
    - `VNil` → JSON Null (`null`)
    - `VList` → JSON Array (通常のリスト) または JSON Object (Alist)
    - `VSym` → JSON String (シンボルは文字列として変換)
### Description:
Spinor の値を JSON 文字列に変換します。連想リスト (Alist) は JSON オブジェクトとして出力されます。
### Examples:
```lisp
;; 基本的な変換
(json-stringify 42)          ; => "42"
(json-stringify 3.14)        ; => "3.14"
(json-stringify "hello")    ; => "\"hello\""
(json-stringify #t)          ; => "true"
(json-stringify nil)         ; => "null"

;; 配列の変換
(json-stringify (list 1 2 3))  ; => "[1,2,3]"

;; オブジェクトの変換 (Alist から)
(json-stringify '(("name" "Alice") ("age" 30)))
; => "{\"name\":\"Alice\",\"age\":30}"
```
### Exceptional Situations:
- 関数、マクロ、MVar など JSON に変換できない型が含まれる場合、エラーを返します。
- 行列 (VMatrix)、CLContext、CLBuffer、Window も変換不可です。
### See Also:
[json-parse](ref/json-parse)
### Notes:
Alist として認識されるには、リストの全要素が `("key" value)` の形式である必要があります。