# Linear Types & Ownership (Experimental)

> **注意**: この機能は実験的 (Experimental) です。将来のバージョンで API が変更される可能性があります。

Spinor の所有権システムは、静的解析によってメモリの生存期間を追跡し、C 言語へのトランスパイル時に適切なタイミングで `free()` を自動挿入する機能です。

---

## 概要

### 目的

- **GC なしのメモリ安全性**: 実行時のガベージコレクションを必要とせず、コンパイル時にメモリ管理を決定
- **Rust 風の所有権モデル**: 値の「所有者」を追跡し、所有権が移動 (ムーブ) したかどうかを静的に検証
- **線形型 (Linear Types)**: 特定の値が「必ず一度だけ使用される」ことを型システムで保証

### 用語

| 用語 | 説明 |
|------|------|
| **Owned** | 値の所有権を持つ状態。スコープ終了時に解放責任あり |
| **Borrowed** | 値への参照のみを持つ状態。解放責任なし |
| **Consumed** | 値がムーブまたは使用され、再利用不可の状態 |
| **Linear** | 必ず一度だけ消費されなければならない特別な所有権 |

---

## 基本的な使い方

### 線形変数の宣言

`linear` 特殊形式を使って線形変数を宣言します。

```lisp
;; 線形変数の宣言
(linear resource (open-file "data.txt"))

;; 線形変数は必ず1回使用しなければならない
(process resource)  ; OK: resource は消費された
```

### 線形変数の制約

1. **必ず消費**: 線形変数はスコープを抜ける前に必ず使用される必要があります
2. **一度だけ**: 同じ線形変数を2回以上参照するとコンパイルエラー
3. **ムーブセマンティクス**: 関数に渡すと所有権が移動し、元の変数は使用不可

```lisp
;; エラー例 1: 未消費
(linear x 42)
;; x を使わずにスコープ終了 → エラー: "Linear variable 'x' must be consumed"

;; エラー例 2: 二重使用
(linear y 100)
(+ y y)  ; → エラー: "Linear variable 'y' used more than once"

;; エラー例 3: ムーブ後の使用
(linear z (create-resource))
(transfer z)  ; z の所有権が transfer に移動
z             ; → エラー: "Variable 'z' used after being moved"
```

---

## 明示的な解放

`drop` 特殊形式で明示的にリソースを解放できます。

```lisp
(linear handle (open-connection "localhost"))

;; 使い終わったら明示的に解放
(drop handle)

;; この時点で handle は使用不可
```

---

## C コード生成との連携

所有権解析の結果は C コード生成時に活用されます。

### 入力 (Spinor)

```lisp
(defun process-data ()
  (let ((buffer (allocate-buffer 1024)))
    (fill-buffer buffer)
    (print-buffer buffer)))
    ;; buffer はここでスコープを抜ける
```

### 出力 (C)

```c
SpObject* user_process_data() {
    SpObject* user_buffer = user_allocate_buffer(sp_make_int(1024));
    user_fill_buffer(user_buffer);
    user_print_buffer(user_buffer);

    /* --- Automatic memory management (ownership system) --- */
    sp_free(user_buffer);  /* drop point */

    return sp_make_nil();
}
```

---

## 型システムの拡張

### Linearity 修飾子

内部的には、型に線形性修飾子が付与されます。

```
Type ::= ...
       | TLinear Linearity Type

Linearity ::= Linear       -- 一度だけ使用
            | Unrestricted -- 制限なし (デフォルト)
```

### 将来の拡張

- **Borrowed 型**: 参照の借用を明示的に表現
- **Lifetime 推論**: Rust 風のライフタイム追跡
- **Region-based Memory**: リージョンベースのメモリ管理

---

## BorrowCheck モジュール

所有権チェックは `Spinor.BorrowCheck` モジュールで実装されています。

### API

```haskell
-- 所有権チェックの実行
checkBorrow :: [Expr] -> BorrowResult

-- 結果
data BorrowResult = BorrowResult
  { brErrors     :: [BorrowError]      -- 検出されたエラー
  , brDropPoints :: Map Text SourceSpan -- 解放すべき位置
  }

-- エラーの種類
data BorrowError
  = DoubleUse Text SourceSpan SourceSpan    -- 二重使用
  | Unconsumed Text SourceSpan              -- 未消費
  | UseAfterMove Text SourceSpan SourceSpan -- ムーブ後使用
```

### 解析フロー

```
Spinor Source
     |
     v
+------------+
|   Parser   |
+------------+
     |
     v
+------------+
| Type Infer |
+------------+
     |
     v
+---------------+
| Borrow Check  |  <-- 所有権解析
+---------------+
     |
     v
+------------+
|  Codegen   |  <-- free() 自動挿入
+------------+
     |
     v
  C Code
```

---

## 制限事項

現在のプロトタイプ実装には以下の制限があります:

1. **関数境界を越えた追跡**: 関数呼び出し後の所有権移動は完全には追跡されません
2. **条件分岐**: `if` 式の両分岐での所有権状態の統合は簡略化されています
3. **クロージャ**: クロージャがキャプチャする変数の所有権は未サポート
4. **相互再帰**: 相互再帰関数での所有権追跡は限定的

---

## 関連ドキュメント

- [Control Flow](control-flow) - 条件分岐と制御構文
- [Definitions](definitions) - 変数定義
- [Architecture](../architecture) - 内部アーキテクチャ
