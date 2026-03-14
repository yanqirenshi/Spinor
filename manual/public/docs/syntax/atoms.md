# Atoms

Spinor の最も基本的なデータ型です。

## 数値 (Numbers)

整数と浮動小数点数をサポートしています。

```lisp
42        ; 整数 (Int)
3.14      ; 浮動小数点数 (Float)
```

詳細は [Numbers](numbers) を参照してください。

## シンボル (Symbols)

変数名や関数名として使用される識別子です。

```lisp
x
my-variable
calculate-sum
```

詳細は [Symbols](symbols) を参照してください。

## 文字列 (Strings)

ダブルクオートで囲まれたテキストです。

```lisp
"Hello, World!"
"日本語も使えます"
```

詳細は [Strings](strings) を参照してください。

## キーワードシンボル (Keyword Symbols)

コロン `:` で始まるシンボルは「キーワードシンボル」と呼ばれ、自己評価します。通常のシンボルとは異なり、環境から値を検索せずに、そのシンボル自身を返します。

```lisp
:foo          ; => :foo (自己評価)
:headers      ; => :headers
:emacs-rex    ; => :emacs-rex

;; 通常のシンボルは環境から値を検索
x             ; 環境で x が定義されていなければエラー

;; キーワードは未定義でもエラーにならない
:undefined    ; => :undefined (自己評価)
```

### 用途

キーワードシンボルは以下の用途に適しています：

- **連想リストのキー**: `((:name . "Alice") (:age . 30))`
- **オプション引数**: `(http-request url :headers headers-alist)`
- **プロトコルメッセージ**: `(:emacs-rex ...)`

```lisp
;; 連想リストでの使用例
(def person (list (cons :name "Alice")
                  (cons :age 30)))

;; if 式での使用 (quote 不要)
(if condition :yes :no)
```

### 型

キーワードシンボルは `Keyword` 型を持ちます。

```lisp
:foo    ; :: Keyword
```

## 特殊シンボル

| シンボル | 意味 |
|:---------|:-----|
| `nil` | 空リスト / 偽値 |
| `t` | 真値 |

## 関連ページ

- [Symbols](symbols) - シンボルの性質と比較の詳細
- [Numbers](numbers) - 数値型と算術演算の詳細
- [Characters](characters) - 文字とエスケープシーケンス
- [Strings](strings) - 文字列操作の詳細
- [Conses](conses) - リスト構造とコンスセル

## Symbols

| Type | Name | Description |
|:-----|:-----|:------------|
| Function | [eq](../api/eq) | シンボルの同一性を比較 |
| Function | [equal](../api/equal) | 値の等価性を比較 |
| Function | [nil?](../api/nil-p) | `nil` かどうかを判定 |
| Function | [print](../api/print) | 値を出力 |
