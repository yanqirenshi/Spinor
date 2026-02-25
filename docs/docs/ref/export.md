# export
**Kind:** Special Form  
**Signature:** `(String...) -> Bool`
### Syntax:
```lisp
(export "sym1" "sym2" ...)
```
### Arguments and Values:
- `syms` -- 公開するシンボル名 (文字列、可変長)
- 戻り値: `#t`
### Description:
現在のパッケージから指定したシンボルを公開 (エクスポート) します。
### Examples:
```lisp
(defpackage "mylib")
(in-package "mylib")
(def public-fn (fn (x) (* x 2)))
(def private-fn (fn (x) (+ x 1)))
(export "public-fn")  ; public-fn のみ公開

;; 別パッケージから
(defpackage "app")
(in-package "app")
(use-package "mylib")
(public-fn 5)   ; => 10 (アクセス可)
;; private-fn は見えない
```
### Side Effects:
現在のパッケージのエクスポートリストを更新します。
### Exceptional Situations:
シンボル名が文字列でない場合、エラーを返します。
### See Also:
[defpackage](ref/defpackage), [use-package](ref/use-package), [in-package](ref/in-package)
### Notes:
`defpackage` の `:export` オプションでも定義時にエクスポートを指定できます。