# Step 25: トランスパイラ基盤 - 実装指示書

## 概要

このタスクでは、Spinor プログラムを C 言語に変換するトランスパイラの基盤を構築します。
仕様書 `specs/step25_transpiler.md` に基づき、以下の4つのステップで実装を進めてください。

## Step 1: C ランタイムヘッダと実装の作成

まず、コンパイルされた C コードが利用するランタイムのヘッダファイルと、最小限の実装を作成します。

1.  **`runtime/spinor.h` の作成:**
    -   仕様書に定義されている `SpType`, `SpValue`, `SpObject` の各型を定義してください。
    -   以下の関数プロトタイプを宣言してください。
        ```c
        #ifndef SPINOR_H
        #define SPINOR_H

        #include <stdbool.h>

        // --- 型定義 ---
        typedef enum { /* ... */ } SpType;
        typedef union { /* ... */ } SpValue;
        typedef struct SpObject { /* ... */ } SpObject;

        // --- コンストラクタ ---
        SpObject* sp_make_nil();
        SpObject* sp_make_bool(bool value);
        SpObject* sp_make_int(long value);

        // --- プリミティブ演算 ---
        SpObject* sp_add(SpObject* a, SpObject* b);
        // (subtract, multiply, divide, eq も同様に追加)

        // --- ユーティリティ ---
        void sp_print(SpObject* obj);

        #endif // SPINOR_H
        ```

2.  **`runtime/spinor.c` の作成:** (新規作成)
    -   `spinor.h` をインクルードし、上記プロトタイプの関数を実装します。
    -   メモリ確保には `malloc` を使用してください。
    -   `sp_add` は2つの `SpObject` の `integer` 値を足し算し、新しい `sp_make_int` で結果を返します。
    -   `sp_print` は `SpObject` の型に応じて `printf` で値を出力します。（まずは `SP_INT` と `SP_BOOL` に対応してください）
    -   `sp_make_...` 関数群は、`malloc` で `SpObject` の領域を確保し、型と値を設定して返します。

## Step 2: コードジェネレータの実装

Haskell で AST から C コード文字列を生成するモジュールを作成します。

1.  **`src/Spinor/Compiler/Codegen.hs` の作成:** (新規作成)
    -   `Spinor.Syntax` と `Data.Text` をインポートします。
    -   `compileProgram :: [Expr] -> Text` 関数を定義します。この関数は C コード全体のテンプレート（`#include`, `main` 関数）を生成します。
    -   `codegen :: Expr -> Text` 関数を定義します。この関数が変換のコアロジックです。
    -   `EInt`, `EBool` リテラル、および `if`, `+` の `EList` 形式に対してパターンマッチで C コードを生成してください。
        ```haskell
        module Spinor.Compiler.Codegen (compileProgram) where

        import Data.Text (Text, pack, unpack, intercalate)
        import Spinor.Syntax (Expr(..))

        compileProgram :: [Expr] -> Text
        compileProgram exprs =
            pack $ unlines
                [ "#include <stdio.h>"
                , "#include "runtime/spinor.h""
                , ""
                , "int main() {"
                , "    SpObject* tmp;" // 結果を一時保持する変数
                , unpack $ intercalate "
" (map compileToplevel exprs)
                , "    return 0;"
                , "}"
                ]

        -- トップレベル式をCの文に変換
        compileToplevel :: Expr -> Text
        compileToplevel expr = "    tmp = " <> codegen expr <> "; sp_print(tmp);"

        -- 式をCの式 (SpObject* を返す) に変換
        codegen :: Expr -> Text
        codegen (EInt n)  = "sp_make_int(" <> pack (show n) <> ")"
        codegen (EBool b) = "sp_make_bool(" <> (if b then "true" else "false") <> ")"
        codegen (EList [ESym "if", c, t, e]) =
            "(" <> codegen c <> "->value.boolean ? " <> codegen t <> " : " <> codegen e <> ")"
        codegen (EList [ESym "+", a, b]) =
            "sp_add(" <> codegen a <> ", " <> codegen b <> ")"
        -- 他のプリミティブも同様に
        codegen other = "sp_make_nil() // TODO: " <> pack (show other)
        ```

## Step 3: `app/Main.hs` への `compile` サブコマンドの統合

`compile` サブコマンドを実装し、トランスパイラを呼び出せるようにします。

1.  **`app/Main.hs` の修正:**
    -   `Spinor.Compiler.Codegen` をインポートします。
    -   `main` 関数の引数パーサに `["compile", file]` のケースを追加します。
    -   `compileMode :: FilePath -> IO ()` 関数を実装します。
        -   指定されたファイルを `parseFile` でパースします。
        -   パース結果の `[Expr]` を `compileProgram` に渡して C コードを生成します。
        -   生成された C コードを `output.c` という名前でファイルに書き出します。
        -   `putStrLn "Compiled to output.c"` のように完了メッセージを表示します。

## Step 4: 動作確認

すべての実装が完了したら、以下の手順で動作を確認してください。

1.  **テスト用 Spinor ファイルの作成:**
    -   `test-compile.spin` というファイルを以下の内容で作成します。
        ```lisp
        (+ 10 (* 2 3)) ; 想定: 16
        (if (= 1 1) 100 200) ; 想定: 100
        ```
    *注: `*`, `=` のサポートも Step 2 で追加実装が必要です。*

2.  **コンパイルの実行:**
    ```sh
    cabal run spinor -- compile test-compile.spin
    ```
    - `output.c` が生成されることを確認します。

3.  **C コードのビルド:**
    ```sh
    gcc -o test_c output.c runtime/spinor.c
    ```
    - `test_c` (または `test_c.exe`) という実行可能ファイルが生成されることを確認します。

4.  **実行:**
    ```sh
    ./test_c
    ```
    - コンソールに以下のように表示されれば成功です。
        ```
        16
        100
        ```

以上でタスクは完了です。不明な点があれば質問してください。

---

## 実装方針

### 1. Tagged Union アプローチ

Spinor の動的型システムを C の静的型システム上で表現するため、**Tagged Union** パターンを採用した。すべての Spinor オブジェクトは型タグ (`SpType`) と値 (`SpValue`) のペアとして表現される。

**選択理由:**
- C で動的型を表現する標準的な手法である
- メモリレイアウトが明確で、デバッグしやすい
- 将来的な GC 実装時に、型タグを利用したオブジェクト走査が可能

### 2. メモリ管理: malloc のみ (GC なし)

初期実装では意図的に GC を省略し、すべてのオブジェクトを `malloc` で確保する方針とした。

**選択理由:**
- 複雑さを最小限に抑え、コード生成の基盤を確立することを優先
- 短命なプログラム（テスト、ワンショット実行）では GC がなくても問題ない
- GC は将来の Step (Step 26 以降) で Mark-Sweep または参照カウント方式で実装予定

### 3. ヘッダ/ソース分離

仕様書に従い、`spinor.h` (プロトタイプ宣言) と `spinor.c` (実装) を分離した。

**利点:**
- 複数の生成 C ファイルから同じランタイムを利用可能
- コンパイル単位が明確になり、増分ビルドが効率化される
- 旧 API (`sp_int`, `sp_bool`) との互換性はマクロで維持

### 4. 演算のランタイム関数化

算術演算 (`+`, `-`, `*`, `/`) と比較演算 (`=`, `<`, `>`) はすべてランタイム関数として実装した。

**選択理由:**
- 将来的な型チェックの追加が容易
- オーバーフロー検出やエラーハンドリングを集中管理できる
- コード生成がシンプルになる（常に関数呼び出しに変換）

---

## 実装内容

### 1. `runtime/spinor.h` の構造

```c
/* 型タグ */
typedef enum {
    SP_NIL,
    SP_BOOL,
    SP_INT,
    SP_SYM,      /* 将来の拡張用 */
    SP_PAIR,     /* 将来の拡張用 */
    SP_FUN,      /* 将来の拡張用 */
    SP_CLOSURE   /* 将来の拡張用 */
} SpType;

/* 値ユニオン */
typedef union {
    bool     boolean;
    long     integer;
    char*    symbol;
    SpPair*  pair;
} SpValue;

/* オブジェクト構造体 */
typedef struct SpObject {
    SpType  type;
    SpValue value;
} SpObject;
```

**関数プロトタイプ:**

| 関数 | 説明 |
|------|------|
| `sp_make_nil()` | nil オブジェクトを生成 |
| `sp_make_bool(bool)` | 真偽値オブジェクトを生成 |
| `sp_make_int(long)` | 整数オブジェクトを生成 |
| `sp_add(a, b)` | 整数の加算 |
| `sp_sub(a, b)` | 整数の減算 |
| `sp_mul(a, b)` | 整数の乗算 |
| `sp_div(a, b)` | 整数の除算 (ゼロ除算でエラー終了) |
| `sp_eq(a, b)` | 等値比較 (型も含めて比較) |
| `sp_lt(a, b)` | 小なり比較 |
| `sp_gt(a, b)` | 大なり比較 |
| `sp_print(obj)` | オブジェクトを標準出力に表示 |

### 2. `runtime/spinor.c` の実装

- 各コンストラクタは `malloc` で `SpObject` を確保し、型タグと値を設定
- メモリ確保失敗時は `stderr` にエラーメッセージを出力し `exit(1)`
- `sp_div` はゼロ除算を検出してエラー終了
- `sp_eq` は型が異なる場合 `false` を返す

### 3. `src/Spinor/Compiler/Codegen.hs` のマッピングルール

| Spinor 式 | 生成される C コード |
|-----------|---------------------|
| `(EInt n)` | `sp_make_int(n)` |
| `(EBool b)` | `sp_make_bool(true/false)` |
| `(if c t e)` | `(codegen(c)->value.boolean ? codegen(t) : codegen(e))` |
| `(+ a b)` | `sp_add(codegen(a), codegen(b))` |
| `(- a b)` | `sp_sub(codegen(a), codegen(b))` |
| `(* a b)` | `sp_mul(codegen(a), codegen(b))` |
| `(/ a b)` | `sp_div(codegen(a), codegen(b))` |
| `(= a b)` | `sp_eq(codegen(a), codegen(b))` |
| `(< a b)` | `sp_lt(codegen(a), codegen(b))` |
| `(> a b)` | `sp_gt(codegen(a), codegen(b))` |
| その他 | `sp_make_nil() /* TODO: ... */` |

### 4. `compile` コマンドの使用方法

```bash
# Spinor ファイルを C コードに変換
cabal run spinor -- compile <input.spin>

# 出力: output.c が生成される

# C コンパイラでビルド
gcc -o program output.c runtime/spinor.c -I runtime

# 実行
./program
```

**例:**

```lisp
; test.spin
(+ 10 (* 2 3))
(if (= 1 1) 100 200)
```

```bash
$ cabal run spinor -- compile test.spin
Compiled to output.c

$ gcc -o test_c output.c runtime/spinor.c -I runtime
$ ./test_c
16
100
```

### 5. 制限事項と今後の課題

- **GC なし:** 長時間実行プログラムではメモリリークが発生する
- **変数束縛なし:** `let`, `define`, `lambda` は未サポート
- **リスト操作なし:** `cons`, `car`, `cdr` は未サポート
- **型チェックなし:** ランタイム関数は引数の型を検証しない

これらは Step 26 以降で段階的に実装予定。
