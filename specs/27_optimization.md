# Step 27: 末尾呼び出し最適化 (TCO: Tail Call Optimization) - 技術仕様

## 1. 概要

本仕様は、Spinor の C トランスパイラ (Codegen) に末尾呼び出し最適化 (TCO) を導入する技術仕様を定義する。

TCO により、末尾位置での自己再帰呼び出しが C の `while(1)` ループ + `continue` (引数更新+ジャンプ) に変換され、スタック消費が O(1) になる。これにより、100万回以上の再帰でもスタックオーバーフローが発生しなくなる。

## 2. 対象範囲

### 2.1. 対象: 末尾自己再帰 (Tail Self-Recursion)

本ステップで最適化対象とするのは、**自己再帰 (self-recursion)** の末尾呼び出しのみである。

- `(defun f (args...) body)` において、`body` の末尾位置に `(f ...)` が出現するケース。
- 相互再帰 (mutual recursion) やクロージャ経由の再帰は対象外。

### 2.2. 末尾位置の定義

以下の位置が「末尾位置」として認識される。

| 式の形式                  | 末尾位置                               |
| ------------------------- | -------------------------------------- |
| 関数本体                  | 本体の式全体が末尾位置                 |
| `(if cond then else)`     | `then` と `else` の両方が末尾位置      |
| その他 (リテラル、演算等) | その式自体が値を返す (末尾位置で return) |

## 3. コード生成戦略

### 3.1. 通常の関数定義 (TCO なし)

末尾自己再帰が検出されない場合、従来通り `return <expr>;` を生成する。

```c
SpObject* user_add(SpObject* user_x, SpObject* user_y) {
    return sp_add(user_x, user_y);
}
```

### 3.2. TCO 適用時の関数定義

末尾自己再帰が検出された場合、以下の構造を生成する。

```c
SpObject* user_countdown(SpObject* user_n) {
    while(1) {
        if (sp_eq(user_n, sp_make_int(0))->value.boolean) {
            return sp_make_int(0);
        } else {
            SpObject* _tco_tmp_0 = sp_sub(user_n, sp_make_int(1));
            user_n = _tco_tmp_0;
            continue;
        }
    }
}
```

### 3.3. 一時変数による同時代入

複数引数の関数で安全に引数を更新するため、**一時変数** (`_tco_tmp_N`) を使用する。

例: `(defun f (a b) ... (f (+ a 1) (+ b a)))` の末尾呼び出しは

```c
SpObject* _tco_tmp_0 = sp_add(user_a, sp_make_int(1));
SpObject* _tco_tmp_1 = sp_add(user_b, user_a);
user_a = _tco_tmp_0;
user_b = _tco_tmp_1;
continue;
```

一時変数を使わないと、`user_a` の更新が `user_b` の計算に影響してしまう。

## 4. 実装モジュール

### 4.1. `src/Spinor/Compiler/Codegen.hs`

以下の関数を追加・変更する。

| 関数 | 種別 | 説明 |
| --- | --- | --- |
| `hasTailSelfCall` | 新規 | 関数本体に末尾自己再帰があるか判定 |
| `compileTailBody` | 新規 | 末尾位置の式を TCO 対応コードに変換 |
| `compileFunDef` | 変更 | TCO 検出時に `while(1)` ループ版を生成 |

## 5. 検証

- 100万回の再帰カウントダウン関数でスタックオーバーフローが発生しないこと。
- TCO 対象外の関数は従来通りのコードが生成されること。
