# use-package
**Kind:** Special Form  
**Signature:** `(String) -> Bool`
### Syntax:
```lisp
(use-package "pkg-name")
```
### Arguments and Values:
- `pkg-name` -- インポートするパッケージ名 (文字列)
- 戻り値: `#t`
### Description:
指定したパッケージの公開シンボルを現在のパッケージから参照可能にします。
### Examples:
```lisp
;; math-lib の公開シンボルを使えるようにする
(defpackage "math-lib" (:export "square"))
(in-package "math-lib")
(def square (fn (x) (* x x)))
(export "square")

(defpackage "my-app")
(in-package "my-app")
(use-package "math-lib")
(square 5)  ; => 25
```
### Side Effects:
現在のパッケージの使用パッケージリストを更新します。
### Exceptional Situations:
指定されたパッケージが存在しない場合、エラーを返します。
### See Also:
[defpackage](ref/defpackage), [in-package](ref/in-package), [export](ref/export)
### Notes:
公開 (export) されたシンボルのみがインポートされます。