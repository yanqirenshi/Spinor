# in-package
**Kind:** Special Form  
**Signature:** `(String) -> String`
### Syntax:
```lisp
(in-package "pkg-name")
```
### Arguments and Values:
- `pkg-name` -- 切り替え先のパッケージ名 (文字列)
- 戻り値: 切り替え先のパッケージ名
### Description:
現在の評価コンテキストを指定したパッケージに切り替えます。以降の `def` はこのパッケージに定義されます。
### Examples:
```lisp
(defpackage "mylib")
(in-package "mylib")
(def x 42)  ; mylib パッケージに定義

(in-package "user")
(def y 100)  ; user パッケージに定義
```
### Side Effects:
現在のパッケージコンテキストを変更します。
### Exceptional Situations:
指定されたパッケージが存在しない場合、エラーを返します。
### See Also:
[defpackage](ref/defpackage), [current-package](ref/current-package), [use-package](ref/use-package)
### Notes:
REPL での実験的な開発に便利です。