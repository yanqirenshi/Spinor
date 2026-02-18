# Step 27: 末尾呼び出し最適化 (TCO) - 実装指示書

## 概要

このタスクでは、Spinor の C トランスパイラ (`Codegen.hs`) に末尾呼び出し最適化 (TCO) を実装します。
仕様書 `specs/27_optimization.md` に基づき、以下のステップで進めてください。

## Step 1: Codegen.hs の改修

`src/Spinor/Compiler/Codegen.hs` を修正し、TCO を導入します。

### 1.1. `hasTailSelfCall` の追加

関数本体の末尾位置に自己再帰呼び出しがあるかを判定する関数を追加してください。

```haskell
hasTailSelfCall :: Text -> Expr -> Bool
hasTailSelfCall fname (EList [ESym "if", _, thenE, elseE]) =
    hasTailSelfCall fname thenE || hasTailSelfCall fname elseE
hasTailSelfCall fname (EList (ESym f : _)) = f == fname
hasTailSelfCall _ _ = False
```

### 1.2. `compileTailBody` の追加

末尾位置の式を TCO 対応の C コードに変換する関数を追加してください。

- `if` 式: C の `if/else` 文に変換し、各分岐を再帰的に `compileTailBody` で処理。
- 自己再帰呼び出し: 一時変数で引数を評価 → パラメータを更新 → `continue`。
- その他: `return <expr>;` を生成。

### 1.3. `compileFunDef` の変更

`hasTailSelfCall` が `True` を返した場合、関数本体を `while(1) { ... }` で囲み、`compileTailBody` を使用するようにしてください。

## Step 2: 動作確認

### 2.1. テストファイルの作成

`test-tco.spin` を以下の内容で作成してください。

```lisp
; test-tco.spin - TCO verification (1M recursive countdown)
(defun countdown (n)
  (if (= n 0)
    0
    (countdown (- n 1))))

(countdown 1000000)
```

### 2.2. ビルドと実行

```sh
cabal build
cabal run spinor -- build test-tco.spin
./test-tco
```

- スタックオーバーフローせずに `0` が表示されれば成功です。

以上でタスクは完了です。

---

## 実装報告

### 1. 実装方針

#### 1.1. 自己再帰の判定ロジック

`hasTailSelfCall :: Text -> Expr -> Bool` 関数を新規追加した。この関数は、関数本体の AST を走査し、**末尾位置** に自分自身への呼び出しが存在するかを判定する。

- `(if cond then else)` 式: `then` と `else` の両分岐を再帰的に検査する。いずれかに自己再帰があれば TCO 対象とする。
- `(fname args...)` 式: `fname` が定義中の関数名と一致すれば末尾自己再帰と判定する。
- その他 (リテラル、演算等): 自己再帰ではないので `False` を返す。

この判定は保守的であり、false positive (TCO 不要なのに適用) は発生しない。`if` の分岐のうち片方だけが自己再帰の場合でも正しく動作する (非再帰側は `return` 文になる)。

#### 1.2. C 言語の制御構文の選択理由

TCO の実装には `while(1)` + `continue` パターンを採用した。

**`while(1)` を選択した理由:**
- C 言語で無限ループを表現する最も標準的かつ可読性の高い方法である。
- `goto` と比較して構造化されており、スコープが明確。
- コンパイラ (gcc/clang) が `-O1` 以上で同等のコードに最適化するため、性能差はない。

**`continue` を選択した理由:**
- 引数更新後にループ先頭へジャンプする意図を明確に表現する。
- `goto` + ラベルに比べてコードの局所性が高く、生成コードの可読性が良い。

**`if/else` 文 (三項演算子ではなく) を選択した理由:**
- TCO 適用時の `if` は「文」として分岐を処理する必要がある (各分岐が `return` または `continue` を含む)。
- 三項演算子は「式」であり、`continue` や `return` を含められないため、`if/else` 文への変換が必須。

#### 1.3. 一時変数を使った引数更新（同時代入対策）

複数引数の自己再帰呼び出しでは、引数の更新順序が結果に影響する可能性がある。例:

```lisp
(defun f (a b) ... (f (+ a 1) (+ b a)))
```

ここで `a` を先に更新すると、`(+ b a)` の計算に更新後の `a` が使われてしまう。この問題を解決するため、すべての引数値を先に一時変数 (`_tco_tmp_N`) に退避してから、パラメータに一括代入する方式を採用した。

```c
SpObject* _tco_tmp_0 = sp_add(user_a, sp_make_int(1));  // 旧 a を使用
SpObject* _tco_tmp_1 = sp_add(user_b, user_a);          // 旧 a を使用
user_a = _tco_tmp_0;  // ここで初めて a が更新される
user_b = _tco_tmp_1;
continue;
```

単一引数の場合でも一時変数を使用する (一貫性と安全性のため)。

### 2. 実装内容

#### 2.1. 変更した関数の概要

**ファイル:** `src/Spinor/Compiler/Codegen.hs`

| 関数 | 種別 | 説明 |
| --- | --- | --- |
| `compileFunDef` | **変更** | `hasTailSelfCall` で TCO 適用可否を判定し、適用時は `while(1)` + `compileTailBody` を使用するように分岐を追加。非適用時は従来の `return <expr>;` を生成。 |
| `hasTailSelfCall` | **新規** | 関数本体の末尾位置に自己再帰呼び出しがあるかを判定する。`if` の両分岐を再帰的に検査。 |
| `compileTailBody` | **新規** | 末尾位置の式を TCO 対応の C コード (文) に変換する。`if` → `if/else` 文、自己再帰 → 一時変数 + 引数更新 + `continue`、その他 → `return` 文。 |

#### 2.2. 生成される C コードの例

**入力 (Spinor):**
```lisp
(defun countdown (n)
  (if (= n 0)
    0
    (countdown (- n 1))))
```

**出力 (C):**
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

#### 2.3. `test-tco.spin` の実行結果

```
$ cabal run spinor -- build test-tco.spin
Compiling test-tco.spin to test-tco.c...
Building test-tco with gcc...
Build successful. Executable created: test-tco

$ ./test-tco
0
```

100万回の再帰カウントダウン (`countdown 1000000`) がスタックオーバーフローせずに正常完了し、期待通り `0` が返された。
