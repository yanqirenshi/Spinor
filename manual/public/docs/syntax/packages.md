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

## 名前空間

現在の Spinor では、すべての定義はグローバルな名前空間に配置されます。名前の衝突を避けるために、プレフィックスを使用することを推奨します。

```lisp
;; 推奨: モジュール名をプレフィックスとして使用
(def math/square (fn (x) (* x x)))
(def math/cube (fn (x) (* x x x)))

(def string/empty? (fn (s) (= (string-length s) 0)))
```

## ブートストラップ

Spinor は起動時に `twister/core.spin` を自動的にロードし、基本的なユーティリティ関数を提供します。

## 将来の拡張

今後のバージョンでは、以下の機能が計画されています：

- `module` 宣言によるモジュール定義
- `import` / `export` による明示的なインターフェース
- 修飾名による名前空間の分離

## Symbols

| Type | Name | Description |
|:-----|:-----|:------------|
| Special Form | [def](../ref/def) | トップレベル変数・関数を定義 |
| Special Form | [fn](../ref/fn) | 関数を作成 |
