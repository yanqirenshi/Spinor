# タスク: ステップ10 - 型システムの基礎 (Hindley-Milner: Types & Unification)

## 現在の状況
* マクロ展開フェーズが分離され、AST がクリーンな状態で手に入るようになった。
* 次は、この AST に対して型推論を行いたい。

## 目標
**Hindley-Milner 型推論エンジンの基礎部分（データ構造と単一化ロジック）を実装する。**
まだ AST の走査（Inference）は行わず、型同士の計算ができる状態を目指す。

## 実装詳細指示

### 1. `src/Spinor/Type.hs` (新規作成)
以下のデータ型を定義してください。
* **`Type` (型):**
    * `TVar Text` (型変数: a, b...)
    * `TInt`
    * `TBool`
    * `TArr Type Type` (関数: t1 -> t2)
    * `TList Type` (リスト: [t])
* **`Scheme` (型スキーム):**
    * 多相型を表現するため (`forall a. a -> a` など)。
    * `Scheme [Text] Type` (量子化された変数リスト + 本体)

### 2. `src/Spinor/Infer.hs` (新規作成)
型推論の中核ロジックです。
* **`Subst` (置換):**
    * `Map.Map Text Type` (型変数名 -> 具体的な型のマッピング)。
    * `nullSubst`, `compose` などを実装。
* **`Types` クラス (型クラス):**
    * `apply` (置換の適用) と `ftv` (自由型変数の取得) メソッドを持つ。
    * `Type`, `Scheme`, `[Type]` に対してインスタンスを定義。
* **`unify` (単一化):**
    * 2つの `Type` を受け取り、それらを一致させるための `Subst` を返す関数。
    * `unify TInt TInt = Ok nullSubst`
    * `unify (TVar "a") TInt = Ok ("a" -> TInt)`
    * `unify (TArr t1 t2) (TArr t3 t4) = ...` (再帰的に単一化)
    * エラー処理 (`Either Text Subst`) を行うこと。
    * **Occurs Check:** `a` と `a -> b` を単一化しようとしたらエラーにする（無限型エラー）。

## 確認用コード (Main.hs は変更なし)
今回はライブラリの実装のみですが、正しく実装されたか確認するために、`src/Spinor/Infer.hs` の末尾に小さなテスト関数（コメントアウト状態でも可）を含めてください。
例: `testUnify = unify (TArr (TVar "a") TInt) (TArr TBool (TVar "b"))` -> `a=Bool, b=Int`

## 出力要件
* `src/Spinor/Type.hs`
* `src/Spinor/Infer.hs`
* `unify` のロジックについての日本語解説。

# 実装方針

## 概要

Hindley-Milner 型推論エンジンの基礎部分（型データ構造、置換、Types クラス、単一化）を実装する。AST の走査はまだ行わず、型同士の計算ができる状態を目指す。

## 設計判断

### Type データ型

`TInt`, `TBool`, `TStr` を独立したコンストラクタとして定義（`TCon "Int"` 等にリファクタリングするのは将来のオプション）。`TArr Type Type` で関数型、`TList Type` でリスト型を表現。

### Scheme (型スキーム)

`Scheme [Text] Type` で量子化された多相型を表現。`forall a b. a -> b -> a` を `Scheme ["a", "b"] (TArr (TVar "a") (TArr (TVar "b") (TVar "a")))` と表す。

### Types クラス

`apply` (置換の適用) と `ftv` (自由型変数の取得) を統一するクラス。`Type`, `Scheme`, `[a]` にインスタンスを定義。置換適用時に `Scheme` の量子化変数は除外する。

### 置換の合成 (composeSubst)

`composeSubst s1 s2 = Map.map (apply s1) s2 `Map.union` s1` — 「まず s2、次に s1 を適用する」置換を生成。

### unify (単一化)

再帰的な構造比較:
- 同じ基本型 → 空の置換
- 型変数 `TVar a` と型 `t` → occurs check 後に束縛
- `TArr` 同士 → 引数型と戻り値型をそれぞれ単一化して合成
- `TList` 同士 → 要素型を再帰的に単一化

**Occurs Check**: 無限型を防ぐため、束縛先の型の自由型変数に自分自身が含まれていないかチェック。

## 変更の流れ

1. `src/Spinor/Type.hs` (新規) — `Type`, `Scheme` データ型
2. `src/Spinor/Infer.hs` (新規) — `Subst`, `Types` クラス、`unify`, `composeSubst`

# 実装内容

## 変更・新規ファイル

| ファイル | 操作 | 概要 |
|---|---|---|
| `src/Spinor/Type.hs` | 新規 | `Type` (型) と `Scheme` (型スキーム) のデータ型定義 |
| `src/Spinor/Infer.hs` | 新規 | `Subst` (置換), `Types` クラス, `unify` (単一化), `composeSubst` (置換合成) |
| `spinor.cabal` | 修正 | `Spinor.Type`, `Spinor.Infer` を exposed-modules に追加 |

## 設計メモ

### Type データ型

```haskell
data Type
  = TVar  Text       -- 型変数: a, b, ...
  | TInt             -- 整数型
  | TBool            -- 真偽値型
  | TStr             -- 文字列型
  | TArr  Type Type  -- 関数型: t1 -> t2
  | TList Type       -- リスト型: [t]
```

タスク仕様の `TInt`, `TBool`, `TArr`, `TList`, `TVar` に加え、Spinor に既に文字列リテラルがあるため `TStr` も追加した。

### Scheme (型スキーム)

```haskell
data Scheme = Scheme [Text] Type
-- 例: forall a b. a -> b -> a
-- Scheme ["a", "b"] (TArr (TVar "a") (TArr (TVar "b") (TVar "a")))
```

量子化された型変数リストと本体の型を保持。`apply` で置換を適用する際、量子化された変数は除外する。

### Types クラス

```haskell
class Types a where
  apply :: Subst -> a -> a  -- 置換の適用
  ftv   :: a -> Set.Set Text -- 自由型変数の取得
```

`Type`, `Scheme`, `[a]` に対してインスタンスを定義。

### unify (単一化) のロジック

2つの型を受け取り、それらを一致させるための置換 (`Subst`) を返す。

| ケース | 処理 |
|---|---|
| 同じ基本型 (`TInt`≡`TInt` 等) | 空の置換を返す (一致済み) |
| 型変数 `TVar a` と型 `t` | occurs check 後、`a → t` の束縛を返す |
| 関数型 `TArr t1 t2` ≡ `TArr t3 t4` | 引数型 `t1≡t3` で `s1` を得る → `s1` を適用後に戻り値型 `t2≡t4` で `s2` を得る → `s2 ∘ s1` を合成 |
| リスト型 `TList t1` ≡ `TList t2` | 要素型 `t1≡t2` を再帰的に単一化 |
| それ以外 | 型エラー |

**Occurs Check**: 型変数 `a` を `a -> b` に束縛すると無限型 `a = a -> b = (a -> b) -> b = ...` になるため、束縛先の型の自由型変数に自分自身が含まれていないかチェックする。

### 置換の合成 (composeSubst)

```haskell
composeSubst s1 s2 = Map.map (apply s1) s2 `Map.union` s1
```

「まず s2 を適用し、次に s1 を適用する」置換を生成。s2 の各値に s1 を適用してから、s1 のマッピングを追加する。

## テスト結果 (GHCi)

```
=== Test 1: (a -> Int) unified with (Bool -> b) ===
Right (fromList [("a",TBool),("b",TInt)])

=== Test 2: Occurs check ===
Left "無限型エラー: a は (a -> b) に出現します"

=== Test 3: [a] unified with [Int] ===
Right (fromList [("a",TInt)])

=== Test 4: Int vs Bool ===
Left "型が一致しません: Int と Bool"

=== Test 5: (a -> b) unified with (Int -> (c -> Bool)) ===
Right (fromList [("a",TInt),("b",TArr (TVar "c") TBool)])
```
