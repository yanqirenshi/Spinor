# タスク: ステップ12 - トップレベル定義と多相化 (Generalization)

## 現在の状況

* 式単位での型推論 (`infer`) は動作している。
* しかし、REPL で `define` しても、その型情報が適切に保存・汎化されていない可能性がある。

## 目標

1. `define` 文を型推論した際、その型を **多相型 (Scheme)** に昇格 (`generalize`) させる。
2. 更新された型環境 (`TypeEnv`) を REPL ループ内で保持し続ける。
3. これにより、Twister ライブラリ (`map` 等) が多相関数として正しくロードされるようにする。

## 実装詳細指示

### 1. `src/Spinor/Infer.hs` (拡張)

トップレベルの式を処理するための関数を追加・修正してください。

* **`inferTop :: TypeEnv -> Expr -> Infer (TypeEnv, Type)`** (シグネチャは目安)
    * 式が `(define name expr)` の形式かどうかチェック。
    * **Yes (`define`) の場合:**
        1. `expr` の型 (`ty`) を推論する。
        2. `eval` 後の値が再帰関数になる可能性を考慮し、環境に一時登録して推論するなどの工夫が必要だが、まずは単純に「右辺を推論」で良い。
        3. **重要:** `generalize env ty` を呼び出し、型スキーム (`Scheme`) に変換する。
           (例: `t1 -> t1` という型を、`forall t1. t1 -> t1` にする)
        4. 新しい `TypeEnv` (name が追加されたもの) と `ty` を返す。
    * **No (普通の式) の場合:**
        1. 普通に `infer` する。
        2. 変更なしの `TypeEnv` と推論結果を返す。

* **`generalize` の確認:**
    * ステップ11で未実装なら実装する。
    * 現在の環境 (`TypeEnv`) に出現しない自由型変数 (`ftv`) を探し、それらを `Scheme` の量子化変数リストに追加するロジック。

### 2. `app/Main.hs` (修正)

REPL の状態管理を強化します。

* **`IORef` または `StateT` で `TypeEnv` を保持:**
    * 実行環境 (`Env`) と同様に、型環境 (`TypeEnv`) もループ間で更新し続ける必要がある。
* **ループ処理の変更:**
    * 入力を `expand` する。
    * `inferTop` を呼び出す。
    * 成功したら:
        * 返ってきた **新しい TypeEnv** を保存する。
        * 推論された型を表示する。
        * その後 `eval` を実行する。
    * 失敗したら:
        * 型エラーを表示し、`eval` はしない。

## 確認事項 (REPL)

実装後、以下が動くことを確認してください（多相性のテスト）。

```lisp
SPINOR> (define id (fn (x) x))
:: forall a. a -> a  (表示形式は _a -> _a などでも可)
<func>

SPINOR> (id 10)
:: Int
10

SPINOR> (id #t)
:: Bool    <-- ここでエラーにならず、Boolとして推論されれば成功！
#t

```

## 出力要件

* `Infer.hs` の `inferTop` (またはそれに準ずる関数) のコード。
* `Main.hs` のループ処理部分。
* 日本語での解説。

# 実装方針

## 概要

`define` で定義した関数の型を `generalize` で多相型 (`Scheme`) に昇格させ、REPL ループ内で `TypeEnv` を保持・更新し続ける。これにより Twister ライブラリの関数が多相関数として正しくロードされる。

## 設計判断

### inferTop 関数

`inferTop :: TypeEnv -> Expr -> Infer (TypeEnv, Subst, Type)` — `infer` との違いは戻り値に**更新された TypeEnv** を含むこと。`define` 時に右辺を推論 → `generalize` → 型環境に登録。

### 再帰対応

`define` の推論時に、まずフレッシュ型変数を環境に仮登録する。これにより再帰関数の本体内で自分自身を参照しても型推論が成功する。推論後に仮型変数と推論結果を unify。

### generalize のタイミング

`define` の右辺を推論した後に `generalize` を呼ぶ。環境に含まれない自由型変数を `forall` で量子化する。例: `t1 -> t1` → `forall t1. t1 -> t1`。

### REPL の型環境保持

REPL ループで `TypeEnv` を引き回す。`inferTop` が返す新しい `TypeEnv` を次のループに渡すことで、`define` された関数の型情報が永続する。

## 変更の流れ

1. `src/Spinor/Infer.hs` (拡張) — `inferTop` 関数追加
2. `app/Main.hs` (修正) — REPL ループで `TypeEnv` を保持・更新

# 実装内容

## 変更ファイル

| ファイル | 操作 | 概要 |
|---|---|---|
| `src/Spinor/Infer.hs` | 拡張 | `inferTop` 関数を追加。define 時に `generalize` して多相型を型環境に登録 |
| `app/Main.hs` | 修正 | REPL ループで `TypeEnv` を保持・更新。`infer` → `inferTop` に切り替え |

## 設計メモ

### inferTop の仕組み

```haskell
inferTop :: TypeEnv -> Expr -> Infer (TypeEnv, Subst, Type)
```

`infer` との違い: 戻り値に **更新された TypeEnv** を含む。

| 入力 | 処理 |
|---|---|
| `(define name body)` / `(def name body)` | 右辺を推論 → `generalize` で多相型に昇格 → `name` を新型環境に登録 → 更新環境を返す |
| その他の式 | 通常の `infer` → 型環境は変更なし |

### define の型推論フロー

```
(define id (fn (x) x))
  1. フレッシュ型変数 t0 を env に仮登録 (再帰対応)
  2. (fn (x) x) を推論 → t1 -> t1
  3. t0 と t1 -> t1 を unify → t0 = t1 -> t1
  4. generalize: env に出現しない自由型変数 {t1} を量子化
     → Scheme ["t1"] (TArr (TVar "t1") (TVar "t1"))
     → forall t1. t1 -> t1
  5. 型環境に id :: forall t1. t1 -> t1 を登録
```

### generalize による多相化

`generalize env ty` は:
1. `ftv ty` (型の自由型変数) から `ftv env` (環境の自由型変数) を引く
2. 残った変数を `forall` で量子化

これにより、`(define id (fn (x) x))` の型 `t1 -> t1` が `forall t1. t1 -> t1` に昇格する。
後続の `(id 10)` では `instantiate` でフレッシュな `t5` に置き換えてから `Int` と unify → `Int` と推論される。

### REPL の型環境保持

```
loop env tyEnv:
  1. Read → Expand → inferTop tyEnv expanded
  2. 成功: (tyEnv', subst, ty) を受け取る
     - 型を表示
     - expandAndEval で実行
     - loop env' tyEnv'  ← 型環境も更新して次のループへ
  3. 失敗: 型エラー表示、型環境・実行環境は変更なし
```

## テスト結果

```
$ cabal run spinor
Spinor REPL (step12)
Loading Twister environment...
Twister loaded.
spinor> (define id (fn (x) x))
:: (t1 -> t1)
<function>
spinor> (id 10)
:: Int
10
spinor> (id #t)
:: Bool
#t
spinor> (id "hello")
:: Str
"hello"
spinor> (define double (fn (x) (+ x x)))
:: (Int -> Int)
<function>
spinor> (double 5)
:: Int
10
spinor> (define const (fn (x y) x))
:: (t1 -> (t2 -> t1))
<function>
spinor> (const 1 #t)
:: Int
1
spinor> (define apply-fn (fn (f x) (f x)))
:: ((t2 -> t3) -> (t2 -> t3))
<function>
spinor> (apply-fn double 3)
:: Int
6
spinor> (if #t 1 #f)
型エラー: 型が一致しません: Int と Bool
spinor> (let x 10 (* x x))
:: Int
100
```
