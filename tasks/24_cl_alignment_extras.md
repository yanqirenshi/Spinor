# Task 24: その他のCL互換機能の実装

## 1. カーネルの実装 (Haskell)

### `src/Spinor/Primitive.hs` または `Eval.hs`
1.  **`error` プリミティブの追加:**
    -   引数が文字列であることを確認し、`throwError` を呼ぶ。

2.  **`bound?` プリミティブの追加:**
    -   引数がシンボルであることを確認する。
    -   現在の環境 (`Env`) を参照し、そのシンボルがキーとして存在するかチェックする。
    -   **注意:** `Primitive.hs` の関数は通常 `Env` にアクセスできない (`[Val] -> Either Text Val` 型のため)。
    -   **対策:** `bound?` は `Env` へのアクセスが必要なため、`Primitive.hs` ではなく `Eval.hs` 内で **特殊形式 (Special Form)** として実装するか、`Eval` モナド内で動作する新しい種類のプリミティブとして扱う必要がある。
    -   **推奨:** 今回は `Eval.hs` の `eval` 関数内で `EList [ESym "bound?", ESym sym] -> ...` のようにパターンマッチで実装するのが最も簡単で確実。

## 2. ライブラリの実装 (Spinor)

### `twister/core.spin`
以下のマクロを追加する。

1.  **`let*` マクロ:**
    -   再帰的に `let` を生成するマクロとして実装。
    -   ベースケース: 束縛リストが空なら `(begin body...)`。
    -   再帰ステップ: `(let ((v1 e1)) (let* rest body...))`。

2.  **`defparameter` マクロ:**
    -   単なる `define` (または `def`) へのエイリアスとして実装。
    -   `(mac (name val . docs) (list 'def name val))`

3.  **`defvar` マクロ:**
    -   `bound?` を使用して分岐するコードを生成。
    -   `(if (bound? 'name) 'name (def name val))`
    -   注意: `name` はクオートして `bound?` に渡す必要がある。

4.  **`dolist` マクロ:**
    -   `for-each` (未実装なら `map` や再帰) を使ったコードに展開。
    -   推奨: 内部で再帰関数 (`loop`) を定義して呼び出す形に展開する。

5.  **`dotimes` マクロ:**
    -   内部でカウンタを持つ再帰関数を定義して呼び出す形に展開する。

## 3. テストの追加

`twister/test.spin` に以下のテストケースを追加する。

```lisp
;; error
;; (assert-error (error "msg")) -- エラーテストの仕組みがあれば

;; bound?
(def defined-var 1)
(assert-equal "bound? true" (bound? 'defined-var) #t)
(assert-equal "bound? false" (bound? 'undefined-var) #f)

;; let*
(assert-equal "let*"
  (let* ((x 1) (y (+ x 1))) y)
  2)

;; defvar
(defvar v1 10)
(defvar v1 20) ; 上書きされないはず
(assert-equal "defvar no-overwrite" v1 10)

;; defparameter
(defparameter p1 10)
(defparameter p1 20) ; 上書きされるはず
(assert-equal "defparameter overwrite" p1 20)

;; dotimes
(def sum 0)
(dotimes (i 4) (setq sum (+ sum i))) ; 0+1+2+3 = 6
(assert-equal "dotimes" sum 6)
```

## 実装方針

### カーネルとライブラリの役割分担

本ステップでは、環境アクセスや副作用の伝播が必要な機能はカーネル (Eval.hs) の特殊形式として、
純粋なコード変換で済む機能はライブラリ (core.spin) のマクロとして実装する。

| 機能 | 実装場所 | 理由 |
|------|----------|------|
| `error` | Eval.hs 特殊形式 | `throwError` の呼び出しが必要 |
| `bound?` | Eval.hs 特殊形式 | 現在の `Env` への参照が必要 |
| `dotimes` | Eval.hs 特殊形式 | `setq` の副作用がクロージャ境界を超えない問題の回避 |
| `dolist` | Eval.hs 特殊形式 | 同上 |
| `let*` | core.spin マクロ | ネストした `let` への純粋な構文変換 |
| `defparameter` | core.spin マクロ | `def` へのエイリアス |
| `defvar` | core.spin マクロ | `bound?` + `def` の組み合わせ |

### `dotimes`/`dolist` をマクロではなく特殊形式にした理由

当初の計画ではマクロとして再帰関数に展開する方針だったが、Spinor の `applyClosureBody` が
関数終了時に `put savedEnv` で環境全体を復元する設計のため、クロージャ内部の `setq` による
副作用が外部スコープに伝播しないことが判明した。

```
問題の例:
  (def sum 0)
  (dotimes (i 4) (setq sum (+ sum i)))
  ;; マクロ展開 → 再帰クロージャ内で setq → クロージャ終了時に env 復元 → sum は 0 のまま
```

この問題を回避するため、`dotimes`/`dolist` は Eval.hs 内で eval モナドのループとして
直接実装し、環境の保存/復元を行わないようにした。

### Expander.hs への追加

マクロが `(list 'let ...)` で `let` フォームを生成すると、`valToExpr` により `EList` 形式になる。
パーサー経由の `ELet` とは異なるため、Expander.hs にマクロ生成の `let` を `ELet` に変換する
ケースを追加した。これは `let*` マクロが正しく動作するために必須。

---

## 実装内容

### 1. `src/Spinor/Eval.hs`

#### `error` 特殊形式
```haskell
eval (EList [ESym "error", msgExpr]) = do
  val <- eval msgExpr
  case val of
    VStr msg -> throwError msg
    _        -> throwError $ "error: 文字列が必要です"
```

#### `bound?` 特殊形式
```haskell
eval (EList [ESym "bound?", arg]) = do
  val <- eval arg
  case val of
    VSym name -> do
      env <- get
      pure $ VBool (Map.member name env)
    _ -> throwError $ "bound?: シンボルが必要です"
```

#### `dotimes` 特殊形式
```haskell
eval (EList (ESym "dotimes" : EList [ESym var, countExpr] : body)) = do
  countVal <- eval countExpr
  case countVal of
    VInt count -> dotimesLoop var count body 0
    _ -> throwError "dotimes: 整数が必要です"

dotimesLoop :: Text -> Integer -> [Expr] -> Integer -> Eval Val
dotimesLoop _   count _    i | i >= count = pure VNil
dotimesLoop var count body i = do
  modify (Map.insert var (VInt i))
  mapM_ eval body
  dotimesLoop var count body (i + 1)
```

#### `dolist` 特殊形式
```haskell
eval (EList (ESym "dolist" : EList [ESym var, listExpr] : body)) = do
  listVal <- eval listExpr
  case listVal of
    VList xs -> dolistLoop var xs body
    VNil     -> pure VNil
    _        -> throwError "dolist: リストが必要です"

dolistLoop :: Text -> [Val] -> [Expr] -> Eval Val
dolistLoop _   []     _    = pure VNil
dolistLoop var (x:xs) body = do
  modify (Map.insert var x)
  mapM_ eval body
  dolistLoop var xs body
```

### 2. `src/Spinor/Expander.hs`

#### `dotimes`/`dolist` 展開ケース
束縛変数は展開せず、カウント式/リスト式と本体のみ再帰展開する。

```haskell
expand (EList (ESym "dotimes" : EList [var@(ESym _), countExpr] : body)) = do
  countExpr' <- expand countExpr
  body' <- mapM expand body
  pure $ EList (ESym "dotimes" : EList [var, countExpr'] : body')

expand (EList (ESym "dolist" : EList [var@(ESym _), listExpr] : body)) = do
  listExpr' <- expand listExpr
  body' <- mapM expand body
  pure $ EList (ESym "dolist" : EList [var, listExpr'] : body')
```

#### マクロ生成 `let` の `ELet` 変換
```haskell
expand (EList (ESym "let" : EList bindingExprs : body@(_:_))) =
  case mapM parseBinding bindingExprs of
    Just bindings -> do
      let bodyExpr = case body of
            [b] -> b
            _   -> EList (ESym "begin" : body)
      expand (ELet bindings bodyExpr)
    Nothing -> throwError "let: 不正な束縛リストです"
  where
    parseBinding (EList [ESym name, val]) = Just (name, val)
    parseBinding _ = Nothing
```

### 3. `twister/core.spin`

#### `let*` マクロ
```lisp
(def let*
  (mac (bindings . body)
    (if (null? bindings)
        (cons 'begin body)
        (list 'let (list (car bindings))
              (cons 'let* (cons (cdr bindings) body))))))
```

#### `defparameter` マクロ
```lisp
(def defparameter
  (mac (name val . docs)
    (list 'def name val)))
```

#### `defvar` マクロ
```lisp
(def defvar
  (mac (name val . docs)
    (list 'if (list 'bound? (list 'quote name))
          name
          (list 'def name val))))
```

### 4. `twister/test.spin` — テストケース (6件追加)

- `bound?`: 定義済み変数 → `#t`、未定義変数 → `#f`
- `let*`: 順次束縛で前の変数を後の定義で参照
- `defvar`: 2回定義しても最初の値が維持される
- `defparameter`: 2回定義すると後の値で上書きされる
- `dotimes`: 0〜3 の合計で `sum = 6`

### テスト結果

- Haskell ユニットテスト: **79件 全パス**
- Spinor セルフテスト: **83件 全パス** (Step 24 の新規 6件含む)

---


