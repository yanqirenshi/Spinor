# defpackage
**Kind:** Special Form  
**Signature:** `(String, Options...) -> String`
### Syntax:
```lisp
(defpackage "pkg-name" (:use "base-pkg") (:export "sym1" "sym2"))
```
### Arguments and Values:
- `pkg-name` -- 定義するパッケージ名 (文字列)
- `:use` -- (オプション) インポートするパッケージのリスト
- `:export` -- (オプション) 外部に公開するシンボル名のリスト
- 戻り値: 作成されたパッケージ名
### Description:
新しいパッケージを定義します。パッケージは名前空間を提供し、シンボルの衝突を防ぎます。
### Examples:
```lisp
;; シンプルなパッケージ定義
(defpackage "mylib")

;; エクスポートを指定
(defpackage "math-utils" (:export "square" "cube"))

;; 他のパッケージを use
(defpackage "my-app" (:use "math-utils") (:export "main"))
```
### Side Effects:
新しいパッケージをパッケージレジストリに登録します。
### Exceptional Situations:
パッケージ名が文字列でない場合、エラーを返します。
### See Also:
[in-package](ref/in-package), [use-package](ref/use-package), [export](ref/export), [current-package](ref/current-package)
### Notes:
すべてのパッケージは暗黙的に "spinor" パッケージを use します (コアプリミティブへのアクセス)。