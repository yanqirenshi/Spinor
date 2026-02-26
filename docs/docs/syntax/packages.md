# Packages & Modules

Spinor におけるモジュールシステムと名前空間について解説します。

## モジュールシステム概要

Spinor のモジュールシステムは、コードを論理的な単位に分割し、名前空間を管理するための仕組みを提供します。

## ファイルのロード

### load 式

外部の Spinor ファイルを現在の環境にロードします。

```lisp
(load "path/to/module.spin")
```

ロードされたファイル内の定義は、現在の環境で使用可能になります。

## 標準ライブラリ

Spinor の標準ライブラリは `twister/` ディレクトリに格納されています。

### ディレクトリ構成

```
twister/
├── core.spin       ; コア関数
├── list.spin       ; リスト操作
├── string.spin     ; 文字列操作
└── test.spin       ; テストユーティリティ
```

### ロード例

```lisp
;; 標準ライブラリのロード
(load "twister/core.spin")
(load "twister/list.spin")
```

## 定義のエクスポート

トップレベルで `def` を使用して定義された関数や変数は、そのファイルをロードした環境で使用可能になります。

```lisp
;; mymodule.spin
(def my-function (fn (x) (* x 2)))
(def my-constant 42)
```

```lisp
;; main.spin
(load "mymodule.spin")
(my-function 10)    ; => 20
my-constant         ; => 42
```

## パッケージシステム

Spinor は Common Lisp スタイルの動的パッケージシステムを提供します。パッケージは名前空間を分離し、シンボルの衝突を防ぎます。

### 組み込みパッケージ

Spinor には以下の組み込みパッケージがあります：

- `spinor` - すべてのプリミティブ関数を含むコアパッケージ
- `user` - デフォルトのユーザーパッケージ（`spinor` を use）

### defpackage - パッケージの定義

新しいパッケージを定義します。

```lisp
;; 基本的なパッケージ定義
(defpackage "my-lib")

;; use オプションで他のパッケージをインポート
(defpackage "my-app" :use ("my-lib"))

;; export オプションでエクスポートするシンボルを宣言
(defpackage "utils" :export (helper format-string))
```

**構文:**
```
(defpackage name [:use (pkg ...)] [:export (sym ...)])
```

### in-package - カレントパッケージの切り替え

カレントパッケージを切り替えます。

```lisp
(defpackage "my-lib")
(in-package "my-lib")

;; 以降の定義は my-lib パッケージに属する
(def helper (fn (x) (* x 2)))
```

### use-package - パッケージのインポート

他のパッケージのエクスポートされたシンボルを現在のパッケージで使用可能にします。

```lisp
(defpackage "math-utils" :export (square cube))
(in-package "math-utils")
(def square (fn (x) (* x x)))
(def cube (fn (x) (* x x x)))

(in-package "user")
(use-package "math-utils")
(square 5)  ; => 25
```

### export - シンボルのエクスポート

パッケージ定義後に追加でシンボルをエクスポートします。

```lisp
(in-package "my-lib")
(def internal-fn (fn (x) x))
(def public-fn (fn (x) (internal-fn x)))
(export public-fn)  ; public-fn のみが外部から見える
```

### current-package - カレントパッケージ名の取得

現在のパッケージ名を文字列で返します。

```lisp
(current-package)  ; => "user"
(in-package "my-lib")
(current-package)  ; => "my-lib"
```

### シンボル解決の順序

シンボルは以下の順序で検索されます：

1. **レキシカルスコープ** - `let` や `fn` の引数
2. **カレントパッケージ** - 現在のパッケージの定義
3. **use されたパッケージ** - エクスポートされたシンボルのみ
4. **spinor パッケージ** - プリミティブ関数

```lisp
(defpackage "my-pkg")
(in-package "my-pkg")

;; + は spinor パッケージから自動的に見える
(+ 1 2)  ; => 3

;; ローカル変数が優先される
(let ((+ (fn (a b) (* a b))))
  (+ 2 3))  ; => 6 (掛け算になる)
```

### REPL での使用例

```lisp
spinor> (defpackage "geometry" :export (area perimeter))
#t
spinor> (in-package "geometry")
#t
spinor> (def area (fn (r) (* 3.14159 r r)))
<closure>
spinor> (def perimeter (fn (r) (* 2 3.14159 r)))
<closure>
spinor> (in-package "user")
#t
spinor> (use-package "geometry")
#t
spinor> (area 5)
78.53975
spinor> (perimeter 5)
31.4159
```

### パッケージの隔離

異なるパッケージで同じ名前の定義を持つことができます。

```lisp
(defpackage "pkg-a")
(in-package "pkg-a")
(def value 100)

(defpackage "pkg-b")
(in-package "pkg-b")
(def value 200)

(in-package "pkg-a")
value  ; => 100

(in-package "pkg-b")
value  ; => 200
```

## ブートストラップ

Spinor は起動時に `twister/core.spin` を自動的にロードし、基本的なユーティリティ関数を提供します。これらの関数は `spinor` パッケージに定義され、`user` パッケージから自動的に使用可能です。

## Symbols

| Type | Name | Description |
|:-----|:-----|:------------|
| Special Form | [def](ref/def) | トップレベル変数・関数を定義 |
| Special Form | [fn](ref/fn) | 関数を作成 |
| Special Form | [defpackage](ref/defpackage) | 新しいパッケージを定義 |
| Special Form | [in-package](ref/in-package) | カレントパッケージを切り替え |
| Special Form | [use-package](ref/use-package) | 他パッケージのシンボルをインポート |
| Special Form | [export](ref/export) | シンボルをエクスポート |
| Function | [current-package](ref/current-package) | カレントパッケージ名を取得 |
