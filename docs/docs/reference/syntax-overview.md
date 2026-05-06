# Syntax

Spinor の構文リファレンスです。以下のセクションに分かれています。

## セクション

- [Atoms](syntax/atoms) - 数値、シンボル、文字列、特殊シンボル
- [Type System](syntax/type-system) - 型推論、多相性、基本型
- [Evaluation](syntax/evaluation) - リストの評価規則、クオート
- [Definitions](syntax/definitions) - def, fn, mac, クロージャ
- [Control Flow](syntax/control-flow) - if, let, begin, setq
- [Data Types](syntax/data-types) - data, match, パターンマッチ

## クイックリファレンス

### 基本型

| 型 | 説明 | 例 |
|:---|:-----|:---|
| `Int` | 整数 | `42`, `-17` |
| `Float` | 浮動小数点数 | `3.14` |
| `String` | 文字列 | `"hello"` |
| `Bool` | 真偽値 | `t`, `nil` |
| `(List a)` | リスト | `'(1 2 3)` |
| `(-> a b)` | 関数型 | `(fn (x) x)` |

### 主要な構文

```lisp
;; 変数定義
(def x 42)

;; 関数定義
(def double (fn (x) (* x 2)))

;; 条件分岐
(if (> x 0) "positive" "non-positive")

;; ローカル変数
(let ((x 10) (y 20)) (+ x y))

;; データ型
(data Maybe (Just val) Nothing)

;; パターンマッチ
(match value
  ((Just x) x)
  (Nothing  0))
```

