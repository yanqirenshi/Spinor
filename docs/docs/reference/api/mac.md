# mac
**Kind:** Special Form
**Signature:** `(Params, Body) -> Macro`
### Syntax:
```lisp
;; 固定長引数
(mac (param1 param2 ...) body)

;; 可変長引数 (ドット記法)
(mac (param1 param2 . rest) body)

;; キーワード引数
(mac (param1 &key key1 key2) body)
```
### Arguments and Values:
- `params` -- 仮引数のリスト (シンボルのリスト、`&key` 含む)
- `body` -- マクロ本体
- 戻り値: マクロオブジェクト

#### 引数の種類:
- **必須引数**: マクロ呼び出し時に必ず指定する位置引数
- **可変長引数 (`. rest`)**: 残りの引数をリストとして受け取る
- **キーワード引数 (`&key`)**: `:name value` 形式で名前付きで指定する引数

### Description:
マクロを作成します。引数は評価されずにそのまま渡され、返り値が評価されます。

**キーワード引数** (`&key`) を使用すると、マクロ呼び出し時に引数を名前付きで指定できます。引数の種類は `fn` と同様です。

### Examples:
```lisp
;; 基本的なマクロ
(def when (mac (cond body)
  (list 'if cond body nil)))
(when (> 5 3) (print "yes"))  ; => "yes"

;; キーワード引数を使用したマクロ
(def with-options (mac (body &key (debug nil))
  (if debug
    (list 'begin (list 'print "DEBUG: starting") body)
    body)))
(with-options (+ 1 2) :debug #t)  ; DEBUG: starting が表示され 3 を返す
```
### See Also:
[fn](fn), [quote](quote)
### Notes:
- マクロの引数は評価されずにそのままの形 (AST) で渡されます。
- キーワード引数の構文は `fn` と同じです。