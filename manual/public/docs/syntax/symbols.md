# Symbols

Spinor におけるシンボルの性質と操作について解説します。

## シンボルとは

シンボルは Lisp における基本的なデータ型で、名前を持つ識別子です。変数名、関数名、定数名などとして使用されます。

```lisp
x               ; シンボル x
my-variable     ; ハイフンを含むシンボル
calculate-sum   ; 複合語のシンボル
string->list    ; 矢印を含むシンボル
null?           ; 疑問符で終わるシンボル (述語の慣例)
*global-var*    ; アスタリスクで囲む (グローバル変数の慣例)
```

## シンボルの評価

シンボルが評価されると、そのシンボルに束縛された値が返されます。

```lisp
(def x 42)
x              ; => 42 (x の値)

(def greet "Hello")
greet          ; => "Hello"
```

## クオート (Quote)

クオートを使用すると、シンボルの評価を抑制し、シンボル自体をデータとして扱えます。

```lisp
'x             ; => x (シンボル x 自体)
(quote x)      ; => x (同じ意味)

'hello         ; => hello
'my-symbol     ; => my-symbol
```

### クオートの用途

```lisp
;; シンボルをデータとして扱う
(def sym 'hello)
sym            ; => hello

;; リスト内のシンボル
'(a b c)       ; => (a b c)

;; 条件分岐の結果として
(if t 'yes 'no)  ; => yes
```

## シンボルの比較

### eq - ポインタレベルの同一性

`eq` は2つの値がメモリ上で同一のオブジェクトかどうかを判定します。シンボルの比較に最適です。

```lisp
(eq 'x 'x)           ; => t (同じシンボル)
(eq 'x 'y)           ; => nil (異なるシンボル)
(eq 'hello 'hello)   ; => t

;; 数値や文字列には eq を使わない
(eq 1 1)             ; 実装依存 (使用非推奨)
```

### equal - 構造的等価性

`equal` は値の内容が等しいかどうかを判定します。

```lisp
(equal 'x 'x)        ; => t
(equal '(a b) '(a b)) ; => t (リストの内容が同じ)
(equal "abc" "abc")  ; => t (文字列の内容が同じ)
```

## 特殊シンボル

### nil

`nil` は空リストと偽値の両方を表す特殊なシンボルです。

```lisp
nil            ; => nil
'()            ; => nil (空リスト)
(null? nil)    ; => t
(if nil 'yes 'no)  ; => no
```

### t

`t` は真値を表す特殊なシンボルです。

```lisp
t              ; => t
(if t 'yes 'no)  ; => yes
```

## シンボルの内部表現

Spinor では、シンボルは内部的に Haskell の `Text` 型として表現されます。同じ名前のシンボルは同一のオブジェクトとして扱われます (インターン)。

```lisp
;; 同じ名前のシンボルは eq で t
(eq 'foo 'foo)   ; => t (必ず真)
```

## Symbols

| Type | Name | Description |
|:-----|:-----|:------------|
| Special Form | [quote](../ref/quote) | 評価を抑制しシンボルをデータとして扱う |
| Function | [eq](../ref/eq) | ポインタレベルの同一性を比較 |
| Function | [equal](../ref/equal) | 構造的等価性を比較 |
| Function | [null?](../ref/null-p) | `nil` かどうかを判定 |
