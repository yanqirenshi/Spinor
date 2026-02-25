# json-parse
**Kind:** Function  
**Signature:** `(String) -> Val`
### Syntax:
```lisp
(json-parse json-string)
```
### Arguments and Values:
- `json-string` -- パースする JSON 文字列
- 戻り値: Spinor の値
    - JSON Number (整数) → `VInt`
    - JSON Number (浮動小数) → `VFloat`
    - JSON String → `VStr`
    - JSON Boolean → `VBool` (`true` → `#t`, `false` → `#f`)
    - JSON Null → `VNil` (`nil`)
    - JSON Array → `VList`
    - JSON Object → `VList` (Alist: `(("key1" val1) ("key2" val2) ...)`)
### Description:
JSON 文字列をパースして Spinor の値に変換します。オブジェクトは連想リスト (Alist) として表現されます。
### Examples:
```lisp
;; 基本的なパース
(json-parse "42")           ; => 42
(json-parse "3.14")         ; => 3.14
(json-parse "\"hello\"")    ; => "hello"
(json-parse "true")         ; => #t
(json-parse "null")         ; => nil

;; 配列のパース
(json-parse "[1, 2, 3]")    ; => (1 2 3)

;; オブジェクトのパース (Alist として表現)
(json-parse "{\"name\": \"Alice\", \"age\": 30}")
; => (("name" "Alice") ("age" 30))

;; ネストしたデータ
(json-parse "{\"items\": [1, 2, 3]}")
; => (("items" (1 2 3)))
```
### Exceptional Situations:
- 不正な JSON 文字列の場合、パースエラーを返します。
- 引数が文字列でない場合、エラーを返します。
### See Also:
[json-stringify](ref/json-stringify)
### Notes:
内部的に aeson パッケージを使用しています。