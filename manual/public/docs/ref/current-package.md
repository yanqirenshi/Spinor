# current-package
**Kind:** Function  
**Signature:** `() -> String`
### Syntax:
```lisp
(current-package)
```
### Arguments and Values:
- 引数なし
- 戻り値: 現在のパッケージ名 (文字列)
### Description:
現在のパッケージ名を返します。
### Examples:
```lisp
(current-package)  ; => "user" (デフォルト)

(defpackage "mylib")
(in-package "mylib")
(current-package)  ; => "mylib"
```
### Affected By:
`in-package` の呼び出しに影響されます。
### See Also:
[in-package](ref/in-package), [defpackage](ref/defpackage)
### Notes:
初期状態では "user" パッケージが現在のパッケージです。